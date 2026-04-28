#
base::Sys.setlocale(locale = "nb.utf8")
source("apar.R")
source("toll_station_functions.R")

# Toll stations were changed 30th March 2026
# Before: Haugalandspakken
# After: Bypakke Haugesund og fastlands-Karmøy


# Haugalandspakken ----
# 2025-01 -- 2026-03
bomstasjoner_haugalandspakken <- readr::read_rds("bomdata_haugesund/bomstasjoner_haugalandspakken.rds")


## Fetch hourly data ----
tolling_station_ids_apar <- c(1, 2, 5:9)

month_string <- "march" # English!
year_number <- 2026

apar_data_for_month <-
  purrr::map_dfr(
    tolling_station_ids_apar,
    ~ get_apar_data(
        dataset_id = haugesund_apar_id_haugalandspakken,
        station_code = .,
        month_string = month_string,
        year_number = year_number
    )
  )

apar_data_for_month_tidy <-
  apar_data_for_month |>
  dplyr::select(
    trp_id = toll_station_code,
    lane,
    date,
    hour = hour_start,
    class = vehicle_class_ID,
    traffic
  ) |>
  dplyr::mutate(
    class =
      dplyr::case_when(
        class == "1" ~ "lette",
        class == "2" ~ "tunge",
        TRUE ~ "ukjent"
      ),
    traffic = as.numeric(traffic)
  )

readr::write_rds(
  apar_data_for_month_tidy,
  file = paste0(
    "H:/Programmering/R/byindeks/bomdata_haugesund/data/autopass_",
    year_number,
    "-",
    month_string
  )
)


## Gather all hourly data ----
apar_files <-
  list.files(
    "H:/Programmering/R/byindeks/bomdata_haugesund/data/",
    # pattern = "2025.*|2026.*",
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )

apar_data_hourly <-
  do.call(
    bind_rows,
    lapply(
      apar_files,
      readr::read_rds
    )
  ) |> 
  dplyr::filter(
    # Incomplete day due to changeover to new stations
    date != "2026-03-30"
  )


## Daily by lane ----
tolling_data_daily_lane <-
  apar_data_hourly |> 
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(trp_id, lane, date, class)
  ) |> 
  dplyr::mutate(
    date = lubridate::as_date(date),
    weekday = lubridate::wday(date, week_start = 1),
    day = lubridate::mday(date),
    month = lubridate::floor_date(date, "month"),
    first_wday = lubridate::wday(month, week_start = 1),
    day_aligned_by_weekday = day + (first_wday - 1),
    year = lubridate::year(date),
    lane = factor(lane, levels = c("1", "3", "5", "7", "2", "4", "6", "8"))
  )


## Check ----
plot_toll_station_data_per_lane(bomstasjoner_haugalandspakken$trp_id[2], c(2025, 2026), bomstasjoner_haugalandspakken)


## Daily ----
tolling_data_daily <-
  tolling_data_daily_lane |>
  dplyr::summarise(
    traffic = sum(traffic),
    lanes = paste(sort(unique(lane)), collapse = ", "),
    .by = c(trp_id, date, class)
  ) |>
  dplyr::mutate(
    day = lubridate::mday(date),
    month = lubridate::floor_date(date, "month"),
    year = lubridate::year(date)
  ) |>
  dplyr::filter(
    !(trp_id == "2" & date %in% ymd(c("2025-12-02"))),
    !(trp_id == "9" & date %in% ymd(c("2026-02-11", "2026-02-12")))
  )

tolling_data_daily_sum_classes <-
  tolling_data_daily |>
  dplyr::group_by(
    trp_id,
    date
  ) |>
  dplyr::summarise(
    traffic = sum(traffic),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    day = lubridate::mday(date),
    month = lubridate::floor_date(date, "month"),
    year = lubridate::year(date),
    class = "alle"
  )

tolling_data_daily_final <-
  dplyr::bind_rows(
    tolling_data_daily,
    tolling_data_daily_sum_classes
  ) |>
  dplyr::arrange(
    trp_id,
    date,
    class
  )

readr::write_rds(
  tolling_data_daily_final,
  file = "bomdata_haugesund/tolling_data_daily_2025-2026.rds"
)


## MDT ----
toll_mdt_class <-
  tolling_data_daily_final |>
  dplyr::group_by(
    trp_id,
    month,
    class
  ) |> 
  dplyr::summarise(
    traffic = sum(traffic),
    n_days = n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    mdt = base::round(traffic / n_days),
    n_days_of_month = lubridate::days_in_month(month),
    # TODO: should count no Feb as 29?
    coverage = (n_days / n_days_of_month) * 100
    # NB! Not correct for HMV as some stations have days without any HMVs
  )

readr::write_rds(
  toll_mdt_class,
  file = "data_indexpoints_tidy/haugesund_toll_mdt.rds",
)


## AADT ----
toll_aadt_class <-
  tolling_data_daily_final |>
  dplyr::group_by(
    trp_id,
    year,
    class
  ) |> 
  dplyr::summarise(
    traffic = sum(traffic),
    n_days = n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    aadt = base::round(traffic / n_days),
    n_days_of_year = dplyr::if_else(lubridate::leap_year(year), 366, 365),
    coverage = (n_days / n_days_of_year) * 100
    # NB! Not correct for HMV as some stations have days without any HMVs
  )

readr::write_rds(
  toll_aadt_class,
  file = "data_indexpoints_tidy/haugesund_toll_aadt.rds",
)


## TRP monthly index ----
index_years <- c(2026)

tolling_station_indices <-
  purrr::map(
    index_years,
    ~ calculate_monthly_index_for_tolling_stations_from_daily_traffic(tolling_data_daily_final, .x - 1, .x)
  ) |> 
  purrr::list_rbind()
  
readr::write_rds(
  tolling_station_indices,
  file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser_haugesund.rds",
)


## TRP yearly index ----
# Not all toll stations have value in latest month
tolling_station_indices_latest_month_per_year <-
  tolling_station_indices |>
  dplyr::mutate(year = year(month_calc)) |>
  dplyr::summarise(
    month = max(month_calc),
    .by = "year"
  )

tolling_station_indices_yearly <-
  tolling_station_indices |>
  dplyr::mutate(
    year = lubridate::year(month_calc)
  ) |>
  dplyr::group_by(
    trp_id,
    year,
    class
  ) |>
  dplyr::summarise(
    #month = max(month), # latest month per year
    base_volume = sum(monthly_volume_base),
    calc_volume = sum(monthly_volume_calc),
    index_p = (calc_volume / base_volume - 1) * 100,
    n_days = sum(n_days),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    coverage = (n_days / 365) * 100
  ) |>
  dplyr::left_join(
    tolling_station_indices_latest_month_per_year,
    by = "year"
  )

readr::write_rds(
  tolling_station_indices_yearly,
  file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser_haugesund.rds"
)


# Bypakke Haugesund og fastlands-Karmøy
# 2026-04 --
tolling_station_ids_apar <- c(1:11)

# Fetch all data for all trp_ids for a month, and store
month_string <- "april" # English!
year_number <- 2026

apar_data_for_month <-
  purrr::map_dfr(
    tolling_station_ids_apar,
    ~ get_apar_data(
        dataset_id = haugesund_apar_id_bypakke_haugesund,
        station_code = .,
        month_string = month_string,
        year_number = year_number
    )
  )
