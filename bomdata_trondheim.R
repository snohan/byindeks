# IN
# - Hourly traffic per class per lane per tolling station (APAR)
# - Tolling station metadata (NVDB)

# OUT
# - Tolling station metadata tidied
# - Tolling station MDT
# - Tolling station index by month
# - Tolling station index by year

# NEXT
# 1. city_index_check.Rmd
# 2. city_index_dataprep_trondheim_toll_stations.R

{
  #source("rmd_setup.R")
  #source("get_from_trafficdata_api.R")
  #library(readxl)
}


# Tolling station info ----
tolling_station_ids_original <-
  c(
    "51",  # Dette er egentlig to ulike, hver med to felt
    # Klett (1,2), Røddeveien (3,4)
    # Endrer nedenfor Røddeveien til id 512 og felt 1 og 2
    "512",
    "52", "53", "54", "55",
    "56", # "Kroppan bru", som egentlig ikke er på Kroppan bru, men
    # Holtermannsvegen utenfor Siemens er to stasjoner, også 57.
    # Slår disse sammen nedenfor, og setter feltnummer etter dagens metrering
    "58", "59", "60", "61", "62", "64", "65", "66", "67",
    "68", "69", "85", "86",
    "72"
    # From 01.11.2023, Ranheim changed ID from 72 (operator ID 100121) to 1 (operator ID 100149)
    # this affects fetching data from NDVB API and APAR API.
  )

# Moholt
# 63 felt 5 er KD3
# Antar: 63 felt 6 er KD4


# Hourly ----

# Different formats:
# - 2017--2021-03
# - 2021--04 just daily
# - 2021--05-
# Normalizing data
# Storing together

# Pre APAR, see bomdata_trondheim_hourly_pre_apar.R


## 2021-05- APAR API ----
### Fetch new ----
source("apar.R")

tolling_station_ids_apar <-
  c(
    "51",  # Dette er egentlig to ulike, hver med to felt
    # Klett (1,2), Røddeveien (3,4)
    # Endrer nedenfor Røddeveien til id 512 og felt 1 og 2
    #"512",
    "52", "53", "54", "55",
    "56", "57", # "Kroppan bru", som egentlig ikke er på Kroppan bru, men
    # Holtermannsvegen utenfor Siemens er to stasjoner, også 57.
    # Slår disse sammen nedenfor, og setter feltnummer etter dagens metrering
    "58", "59", "60", "61", "62", "64", "65", "66", "67",
    "68", "69", "85", "86",
    # From 01.11.2023, Ranheim changed ID from 72 (operator ID 100121) to 1 (operator ID 100149)
    #"72"
    "1"
  )

# Fetch all data for all trp_ids for a month, and store
month_string <- "november" # English
year_number <- 2025

apar_data_for_month <-
  purrr::map_dfr(
    tolling_station_ids_apar,
    ~ get_apar_data(
      dataset_id = trondheim_apar_id,
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
    trp_id =
      dplyr::case_when(
        trp_id == "51" & lane %in% c("3", "4") ~ "512",
        trp_id == "57" ~ "56",
        trp_id == "1" ~ "72",
        TRUE ~ trp_id
      ),
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
    "H:/Programmering/R/byindeks/bomdata_trondheim/raw_apar_2021-5_/apar_trd_",
    year_number,
    "-",
    month_string
  )
)


### Gather all hourly APAR data ----
apar_files <-
  list.files(
    "H:/Programmering/R/byindeks/bomdata_trondheim/raw_apar_2021-5_",
    #pattern = "2021.*",
    #pattern = "2022.*|2023.*|2024.*",
    pattern = "2025.*",
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
  )


# Daily ----
# Quality check: look first at daily traffic per lane
# If anything strange, look at hourly

tolling_data_daily_lane <-
  dplyr::bind_rows(
    #data_2019_2021_hourly,
    apar_data_hourly
  ) %>%
  dplyr::filter(
    trp_id %in% tolling_station_ids_original
  ) %>%
  dplyr::group_by(
    trp_id,
    lane,
    date,
    class
  ) %>%
  dplyr::summarise(
    traffic = sum(traffic),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    date = lubridate::as_date(date),
    day = lubridate::mday(date),
    month = lubridate::floor_date(date, "month"),
    year = lubridate::year(date)
  ) #|>
  #dplyr::bind_rows(
  #  april_2021_daily
  #)

tolling_data_daily <-
  tolling_data_daily_lane |>
  # Removing unknowns since they are not vehicles (presumably)
  #dplyr::filter(
  #  class != "ukjent"
  #) |>
  dplyr::group_by(
    trp_id,
    date,
    class
  ) %>%
  dplyr::summarise(
    traffic = sum(traffic),
    .groups = "drop"
  ) |>
  # Creating zeros to match corresponding non-zero hours ONLY NEEDED FOR HOURLY TRAFFIC
  # tidyr::complete(
  #   trp_id,
  #   tidyr::nesting(
  #     date,
  #     class
  #   ),
  #   fill = list(traffic = 0)
  # ) |>
  dplyr::mutate(
    day = lubridate::mday(date),
    month = lubridate::floor_date(date, "month"),
    year = lubridate::year(date)
  )


# Check ----
# Plot to see if data are ok
kommune_bomer <-
  readr::read_rds(
    file = "bomdata_trondheim/trd_toll_stations.rds"
  )

plot_toll_station_data_per_lane <- function(toll_id_chosen, year_chosen) {

    toll_station_name <-
      kommune_bomer |>
      dplyr::filter(
        trp_id == toll_id_chosen
      ) |>
      dplyr::select(name) |>
      purrr::pluck(1)

    tolling_data_daily_lane |>
    dplyr::filter(
      trp_id == toll_id_chosen,
      year %in% c(year_chosen)
    ) |>
    ggplot(aes(day, traffic, color = lane, linetype = class)) +
    geom_line(linewidth = 1) +
    facet_grid(
      rows = vars(month)
    ) +
    theme_minimal() +
    scale_x_continuous(
      breaks = base::seq(5, 30, by = 5),
      minor_breaks = c(1:31)
    ) +
    ggplot2::ggtitle(toll_station_name)

}

# Ranheim is 72, 21 stations
plot_toll_station_data_per_lane(tolling_station_ids_original[15], 2025)

# Exclusions
source("bomdata_trondheim_exclusions.R")

tolling_data_daily_total <-
  tolling_data_daily_tidy |>
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

tolling_data_daily_all <-
  dplyr::bind_rows(
    tolling_data_daily_tidy,
    tolling_data_daily_total
  ) |>
  dplyr::arrange(
    trp_id,
    date,
    class
  )

readr::write_rds(
  tolling_data_daily_all,
  #file = "bomdata_trondheim/tolling_data_daily_2019-2021.rds"
  #file = "bomdata_trondheim/tolling_data_daily_2022-2024.rds"
  file = "bomdata_trondheim/tolling_data_daily_2025-2026.rds"
)


# Read all daily ----
tolling_data_daily_all_years_files <-
  list.files(
    "H:/Programmering/R/byindeks/bomdata_trondheim",
    pattern = "tolling_data_daily.*rds",
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )

tolling_data_daily_all_years <-
  do.call(
    bind_rows,
    lapply(
      tolling_data_daily_all_years_files,
      readr::read_rds
    )
  )


# MDT ----
toll_mdt_class <-
  tolling_data_daily_all_years |>
  dplyr::group_by(
    trp_id,
    month,
    class
  ) %>%
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
  file = "data_indexpoints_tidy/trd_toll_mdt.rds",
)

# toll_mdt_class <-
#   readr::read_rds(
#     file = "data_indexpoints_tidy/trd_toll_mdt.rds",
#   )


# CMDT ----
toll_nvdb_id <-
  kommune_bomer |>
  dplyr::select(
    trp_id, nvdb_id
  ) |>
  dplyr::mutate(
    trp_id =
      dplyr::case_when(
        trp_id == "1" ~ "72",
        TRUE ~ trp_id
      ),
    nvdb_id = base::as.character(nvdb_id)
  )

source("calculate_cmdt_toll.R")

{
  tic()
  for (i in 1:length(tolling_station_ids_original)) {

    cmdt <-
      purrr::map(
        years_from_reference_to_today,
        ~ calculate_cmdt_toll(tolling_station_ids_original[i], .x)
      ) |>
      purrr::list_rbind()

    cmdt |>
      readr::write_rds(
        file =
          paste0(
            "cmdt/cmdt_",
            city_number,
            "_",
            tolling_station_ids_original[i],
            ".rds"
          )
      )
  }
  toc()
}


# AADT ----
toll_aadt_class <-
  tolling_data_daily_all_years |>
  dplyr::group_by(
    trp_id,
    year,
    class
  ) %>%
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
  file = "data_indexpoints_tidy/trd_toll_aadt.rds",
)

# TRP index ----
## Monthly ----
# calculate_monthly_index_for_tolling_stations <-
#   function(monthly_class_data, baseyear) {
#
#     basedata <-
#       monthly_class_data %>%
#       dplyr::filter(year == baseyear)
#
#     calcdata <- monthly_class_data %>%
#       dplyr::filter(year == baseyear + 1)
#
#     indexdata <-
#       dplyr::inner_join(
#         basedata,
#         calcdata,
#         by = c("month", "trp_id", "class"),
#         suffix = c("_base", "_calc"),
#       ) %>%
#       dplyr::group_by(
#         trp_id,
#         class,
#         month
#       ) %>%
#       dplyr::summarise(
#         monthly_volume_base = sum(traffic_base),
#         monthly_volume_calc = sum(traffic_calc),
#         .groups = "drop"
#       ) %>%
#       dplyr::mutate(
#         index_p =
#           (monthly_volume_calc / monthly_volume_base - 1) * 100 %>%
#           round(digits = 2),
#         month_as_date =
#           lubridate::ymd(
#             paste(baseyear + 1, month, "1", sep = "-")
#           )
#       )
#   }


# daily_class_data <- tolling_data_daily_all_years
# baseyear <- 2021
# calcyear <- 2022

calculate_monthly_index_for_tolling_stations_from_daily_traffic <-
  function(daily_class_data, baseyear, calcyear) {

    basedata <-
      daily_class_data %>%
      dplyr::filter(year == baseyear) |>
      dplyr::mutate(
        month_number = lubridate::month(date)
      )

    calcdata <-
      daily_class_data %>%
      dplyr::filter(year == calcyear) |> # baseyear + 1
      dplyr::mutate(
        month_number = lubridate::month(date)
      )

    indexdata <-
      dplyr::inner_join(
        basedata,
        calcdata,
        by = c("day", "month_number", "trp_id", "class"),
        suffix = c("_base", "_calc"),
      ) |>
      dplyr::group_by(
        trp_id,
        class,
        month_calc
      ) |>
      dplyr::summarise(
        monthly_volume_base = sum(traffic_base),
        monthly_volume_calc = sum(traffic_calc),
        n_days = n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        index_p =
          (monthly_volume_calc / monthly_volume_base - 1) * 100 %>%
          round(digits = 2),
        n_days_of_month = lubridate::days_in_month(month_calc),
        # TODO: should count no Feb as 29?
        coverage = (n_days / n_days_of_month) * 100
        # NB! Not correct for HMV as some stations have days without any HMVs
      )
  }

tolling_station_index_2020 <-
  tolling_data_daily_all_years %>%
  calculate_monthly_index_for_tolling_stations_from_daily_traffic(2019, 2020)

tolling_station_index_2021 <-
  tolling_data_daily_all_years %>%
  calculate_monthly_index_for_tolling_stations_from_daily_traffic(2020, 2021)

tolling_station_index_2022 <-
  tolling_data_daily_all_years %>%
  calculate_monthly_index_for_tolling_stations_from_daily_traffic(2021, 2022)

tolling_station_index_2023 <-
  tolling_data_daily_all_years %>%
  calculate_monthly_index_for_tolling_stations_from_daily_traffic(2022, 2023)

tolling_station_index_2024 <-
  tolling_data_daily_all_years %>%
  calculate_monthly_index_for_tolling_stations_from_daily_traffic(2023, 2024)

tolling_station_index_2025 <-
  tolling_data_daily_all_years %>%
  calculate_monthly_index_for_tolling_stations_from_daily_traffic(2024, 2025)

tolling_station_indices <-
  dplyr::bind_rows(
    tolling_station_index_2020,
    tolling_station_index_2021,
    tolling_station_index_2022,
    tolling_station_index_2023,
    tolling_station_index_2024,
    tolling_station_index_2025
  )

readr::write_rds(
  tolling_station_indices,
  file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser.rds",
)


## Direct index ----
# tolling_station_index_2019_2023 <-
#   tolling_data_daily_all_years %>%
#   calculate_monthly_index_for_tolling_stations_from_daily_traffic(2019, 2023)
#
# tolling_station_index_2019_2024 <-
#   tolling_data_daily_all_years %>%
#   calculate_monthly_index_for_tolling_stations_from_daily_traffic(2019, 2024)
#
# dplyr::bind_rows(
#   tolling_station_index_2019_2023,
#   tolling_station_index_2019_2024
# ) |>
# readr::write_rds(
#   file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser_direkte.rds",
# )


# tolling_station_indices <-
#   readr::read_rds(
#     file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser.rds",
#   )
# TODO: Dekningsgrad for antall måneder


## Yearly ----
# Not all toll stations have value in latest month
tolling_station_indices_latest_month_per_year <-
  tolling_station_indices |>
  dplyr::mutate(year = year(month_calc)) |>
  dplyr::group_by(
    year
  ) |>
  dplyr::summarise(
    month = max(month_calc),
    .groups = "drop"
  )

tolling_station_indices_yearly <-
  tolling_station_indices |>
  dplyr::mutate(
    year = year(month_calc)
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
  file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.rds"
)
