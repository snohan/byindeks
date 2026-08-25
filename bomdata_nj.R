#
base::Sys.setlocale(locale = "nb.utf8")
source("apar.R")
source("toll_station_functions.R")


#
# Monthly data ----
# A preliminary analysis

## 2019 - 2021-04 ----
# Filer fra BFIN (Nina Lysfjord)

# Stasjonsinfo
tollstations_nj_bfin <- 
  readxl::read_excel("bomdata_nj/nj_bomstasjoner_bfin.xlsx") |> 
  dplyr::select(
    trp_id = ChargingPointID,
    navn_csn = 'Navn i CSN',
    navn_analytics = 'Navn i Analytics'
  ) |> 
  dplyr::mutate(
    trp_id = base::as.character(trp_id)
  )

# Bomdata per måned
tolldata_nj_month_bfin <-
  readxl::read_excel("bomdata_nj/nj_bomdata_month_bfin.xlsx") |> 
  dplyr::select(
    navn_csn = Bomstasjon,
    month_orig = Måned,
    lmv = 'Liten bil',
    hmv = 'Stor bil',
    no_class = Ukjent
  ) |> 
  dplyr::left_join(
    tollstations_nj_bfin |> 
      dplyr::select(-navn_analytics),
    by = "navn_csn"
  ) |> 
  dplyr::mutate(
    month_object = base::paste0("01.", month_orig) |> lubridate::dmy()
  ) |> 
  dplyr::select(
    trp_id, month_object, lmv, hmv, no_class
  ) |> 
  dplyr::mutate(
    month_no = lubridate::month(month_object),
    year = lubridate::year(month_object),
    # Need to incorporate class "ukjent"
    # BFIN: add them to light and heavy by ratio
    # sum_class = (lmv + hmv),
    # lmv_ratio = lmv / sum_class,
    # no_class_lmv_ratio = no_class * lmv_ratio,
    # no_class_to_lmv = 
    #   dplyr::case_when(
    #     no_class_lmv_ratio - base::floor(no_class_lmv_ratio) < 0.5 ~ base::floor(no_class_lmv_ratio),
    #     no_class_lmv_ratio - base::floor(no_class_lmv_ratio) >= 0.5 ~ base::ceiling(no_class_lmv_ratio)
    #   ),
    # lmv_adj = lmv + no_class_to_lmv,
    # hmv_adj = hmv + (no_class - no_class_to_lmv)
    # diff_adj = sum_class + no_class - lmv_adj - hmv_adj # All zero! :)
    # Ferde: "ukjent" are all light
    lmv_adj = lmv + no_class,
    hmv_adj = hmv
  ) |> 
  dplyr::select(
    trp_id, month_object, month_no, year,
    lmv = lmv_adj,
    hmv = hmv_adj
  )
  
readr::write_rds(tolldata_nj_month_bfin, "bomdata_nj/nj_bomdata_month_bfin.rds")

## 2025 ----
# CSV export from PowerBI
tolldata_nj_month_2025 <-
  readr::read_csv("bomdata_nj/nj_bomdata_month_2025.csv") |> 
  dplyr::select(
    trp_id = 'toll station code',
    class = 'vehicle class ID',
    month_no = 'month no.',
    year,
    traffic = 'Accepted passages'
  ) |> 
  dplyr::mutate(
    trp_id = base::as.character(trp_id),
    month_object = base::paste0("01-", month_no, "-", year) |> lubridate::dmy(),
    class = dplyr::case_when(
      class == 1 ~ "lmv",
      class == 2 ~ "hmv"
    )
  ) |> 
  dplyr::select(
    trp_id, month_object, month_no, year, class, traffic
  ) |> 
  tidyr::pivot_wider(
    names_from = class,
    values_from = traffic
  )

readr::write_rds(tolldata_nj_month_2025, "bomdata_nj/nj_bomdata_2025.rds")


## Compare 2025 to 2019 ----
tolldata_nj_2019_2025_month <-
  dplyr::full_join(
    readr::read_rds("bomdata_nj/nj_bomdata_bfin.rds") |> 
      dplyr::filter(year == 2019) |> 
      dplyr::select(trp_id, month_no, lmv, hmv) |> 
      dplyr::mutate(
        # Toll stations around Tananger have changed ID and direction
        trp_id = 
          dplyr::case_when(
            trp_id == "601" ~ "603",
            trp_id == "602" ~ "604",
            TRUE ~ trp_id
          )
      ),
    readr::read_rds("bomdata_nj/nj_bomdata_2025.rds") |> 
      dplyr::select(trp_id, month_no, lmv, hmv),
    by = dplyr::join_by(trp_id, month_no),
    suffix = c("_2019", "_2025")
  ) |> 
  dplyr::mutate(
    index_p = 100 * (lmv_2025 / lmv_2019 - 1)
  ) |> 
  dplyr::filter(
    !(trp_id %in% c(101, 114))
  ) |> 
  dplyr::arrange(trp_id, month_no)

readr::write_rds(tolldata_nj_2019_2025_month, "bomdata_nj/nj_bomstasjonsindeks_2019_2025.rds")

# Data considerations ----
# 1. 109 ved sykehuset har lavere tall i nov og des 2025 - hvorfor?
# 2. Bybrua har skiftet ID fra 101 til 114. Den har byttet innkrevingsretning fra 25.11.2020.
#    Men i 2025 går mye av trafikken i Hundvågtunnelen i stedet for over Bybrua. Da må bomstasjoene i tunnelen være med. 
#    Må da ta bare den ene kjøreretningen som er lik den ene som fanges opp av 114.
#    Men sammenligningen blir ikke komplett da en del av trafikken som i dag går i tunnelen, gikk med ferje i 2019.
#    Ergo er det best å utelate bomstasjonen på Bybrua fra sammenligningen før 2023.
# 3. Bomstasjonene på Jåsund og Tananger er snudd fra og med 25.11.2020. Ser ut til at tallene er sum liten og stor etter dette og ut april 2021. 
#    Denne perioden må ekskluderes, men gitt at vi kan sammenligne motsatte retninger kan sammenlignes, kan stasjonene være med.


# Daily data ----
# Toll station info from NVDB
source("bomdata_nj_stations.R")


## 2019-2021 from Ferde ----
nj_tolldata_daily_pre_2022 <- 
  purrr::map(
    c("2018", "2019", "2020", "2021_jan_april", "2021_mai_des"),
    ~ readxl::read_excel("bomdata_nj/nj_bomdata_daily_ferde.xlsx", sheet = .x, skip = 1, na = c("-"))
  ) |> 
  purrr::list_rbind()

tolling_data_daily_lane <-
  nj_tolldata_daily_pre_2022 |> 
  tidyr::pivot_longer(
    cols = -date,
    names_to = c("trp_id", "class"),
    names_sep = "_",
    values_to = "traffic"
  ) |> 
  tidyr::pivot_wider(
    id_cols = c(date, trp_id),
    names_from = class,
    values_from = traffic
  ) |> 
  dplyr::mutate(
    lette = dplyr::coalesce(liten, 0) + dplyr::coalesce(u, 0),
    tunge = dplyr::coalesce(stor, 0)
  ) |> 
  dplyr::select(trp_id, date, lette, tunge) |> 
  tidyr::pivot_longer(
    cols = c(lette, tunge),
    names_to = "class",
    values_to = "traffic"
  ) |> 
  dplyr::mutate(
    # Fake lane in order for plot function to work
    lane = 1,
    date = lubridate::as_date(date),
    weekday = lubridate::wday(date, week_start = 1),
    day = lubridate::mday(date),
    month = lubridate::floor_date(date, "month"),
    first_wday = lubridate::wday(month, week_start = 1),
    day_aligned_by_weekday = day + (first_wday - 1),
    year = lubridate::year(date),
    lane = factor(lane, levels = c("1", "3", "5", "7", "2", "4", "6", "8"))
  )

# Check data further down, then do exclusions, and write final rds file for later use (when reading all daily files)


## 2022- from Autopass Analytics ----
{
  month_string <- "may" # English!
  year_number <- 2022

  apar_data_for_month <-
    purrr::map_dfr(
      unique(bomstasjoner_nj$trp_id),
      ~ get_apar_data(
          dataset_id = nj_apar_id,
          autopass_station_id = .,
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
      "H:/Programmering/R/byindeks/bomdata_nj/analytics/nj_tolldata_",
      year_number,
      "-",
      month_string,
      ".rds"
    )
  )
}


## Gather all hourly data
hourly_files <-
  base::list.files(
    "H:/Programmering/R/byindeks/bomdata_nj/analytics",
    # pattern = "2022.*|2023.*",
    # pattern = "2024.*|2025.*",
    # pattern = "2026.*",
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )

hourly_data <-
  do.call(
    bind_rows,
    lapply(
      hourly_files,
      readr::read_rds
    )
  )

## Daily by lane
tolling_data_daily_lane_raw <-
  hourly_data |> 
  # NB! 201, 801 and 802 is duplicated in all hourly files!!!
  dplyr::distinct() |> 
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
    year = lubridate::year(date)    
  ) |> 
  # The flipping of 107
  dplyr::filter(
    !(trp_id %in% c("107", "110", "114") & date >= ymd("2022-12-12") & lane == 1)
  ) |> 
  dplyr::mutate(
    lane = 
      dplyr::case_when(
        trp_id %in% c("107", "110", "114") ~ as.factor(1),
        TRUE ~ lane
      ),
    lane = factor(lane, levels = c("1", "3", "5", "7", "2", "4", "6", "8"))
  )

source("bomdata_nj_exclusions.R")

## Check daily by lane ----
plot_toll_station_data_per_lane(base::unique(bomstasjoner_nj$trp_id)[1], c(2022), bomstasjoner_nj)


## Daily data
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
  )


## Sum classes
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
  # file = "bomdata_nj/daily/nj_tolldata_daily_2018-2021.rds"
  file = "bomdata_nj/daily/nj_tolldata_daily_2022-2026.rds"
)


## Calculating CMDT ----
# Gather all daily data
tolling_data_daily_all_years_files <-
  list.files(
    "H:/Programmering/R/byindeks/bomdata_nj/daily",
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
  ) |> 
  dplyr::filter(
    # Skipping stations on Buøy and Hundvåg
    trp_id < 800
  )


# Toll NVDB ID for linking to traffic links
toll_nvdb_id <-
  bomstasjoner_nj |>
  dplyr::select(
    trp_id, nvdb_id
  ) |>
  dplyr::mutate(
    nvdb_id = base::as.character(nvdb_id)
  )

source("calculate_cmdt_toll.R")

calculate_cmdt_toll_for_all_stations(toll_nvdb_id$trp_id, c(2018:2026), tolling_data_daily_all_years)


# Toll or TRP? ----
# 4. Bomstasjoner og TRP som ligger på samme sted, og basert på data - hvilken bør velges:
#    402 og Tastatorget, bom
#    102 og Bjergsted, trp
#    104 og Randabergveien, trp
#    107 og Madlaveien ved Mosvatnet, trp
#    114 og Bybrua sør, trp
#    210 og Forus Gamleveien, trp
#    301 og Strandgata nord, trp
#    206 og Jåtten, trp
#    207 og Forus (hovedveg pluss ramper), bom
#    310 og Hana ved Rovik, bom
#    308 og Austrått, trp
#    306 og Brualand, bom
#    304 og Oalsgata, trp
#    205 og Bærheim, trp
#    502 og Solastrand sør, trp
#    TRP er å foretrekke da disse har begge retninger

# TODO: Compare CMDT
cmdt_city <- readr::read_rds("data_indexpoints_tidy/cmdt_952.rds")

cmdt_compare <- 
  cmdt_city |> 
  dplyr::filter(
    length_class == "korte",
    trp_id %in% c("906727251", "12478V320582")
  ) |> 
  dplyr::left_join(
    universal_calendar_periods,
    by = dplyr::join_by(year, month == period_name)
  )

visualize_cmdt_comparison(cmdt_compare)

