source("rmd_setup.R")
source("get_from_trafficdata_api.R")
source("get_from_nvdb_api.R")

library(readxl)

# Tidying data from tolling stations

# The 20 to use
tolling_station_ids <-
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
    "68", "69", "85", "86", "72"
    )

# Moholt
# 63 felt 5 er KD3
# Antar: 63 felt 6 er KD4


# Tolling station info ----
kommunenr <- "5001"
kommunenavn <-
  hent_kommune_v3(kommunenr) |>
  dplyr::select(kommunenavn) |>
  purrr::pluck(1)

kommune_bomer_uttak <-
  get_tolling_stations_v3(kommunenr)

kommune_bomer <-
  kommune_bomer_uttak %>%
  dplyr::rename(
    trp_id = msnr
  ) %>%
  dplyr::mutate(
    station_type = "Bomstasjon"
  ) %>%
  dplyr::select(trp_id, everything()) %>%
  dplyr::filter(
    trp_id %in% tolling_station_ids
  ) %>%
  dplyr::mutate(
    trp_id =
      dplyr::case_when(
        name == "Røddeveien" ~ "512",
        TRUE ~ trp_id
      ),
    name = stringr::str_replace(name, "\\,.+$", ""),
    name = stringr::str_replace(name, " M-snitt\\)$", ""),
    name = stringr::str_replace(name, "\\. K-snitt$", ""),
    name = stringr::str_replace(name, " \\(Nordgående\\)$", ""),
    name = stringr::str_replace(name, " \\(Sørgående\\)$", ""),
    name = stringr::str_replace(name, "Rv.707", "Fv 707"),
    municipality_name = "Trondheim"
  ) %>%
  dplyr::arrange(
    trp_id
  )

# write.csv2(
#   kommune_bomer,
#   file = "trd_toll_stations.csv",
#   fileEncoding = "latin1"
# )

# Names from toll data files
# bom_felt_og_stasjon <-
#   read.csv2(
#     "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_felt_og_stasjon.csv"
#   ) %>%
#   dplyr::select(-felt) %>%
#   dplyr::rename(name = stasjon)
#
# trh_bomer <-
#   kommune_bomer %>%
#   dplyr::select(-name) %>%
#   dplyr::left_join(bom_felt_og_stasjon, by = c("msnr" = "kode")) %>%
#   dplyr::select(-msnr) %>%
#   dplyr::mutate(municipality_name = "Trondheim")


# Read and prepare hourly ----

# Different formats:
# 2017--2021-03
# 2021--05-
# Normalizing data
# Storing together


## 2019-01--2021-03 Vegamot files ----

# Adding station ID
# Implicitly adding the two Kroppan bru to one trp
tolling_station_codes <-
  readr::read_csv2(
    "H:/Programmering/R/byindeks/bomdata_trondheim/tolling_station_codes.csv",
    locale = readr::locale(encoding = "latin1")
  ) %>%
  dplyr::mutate(
    trp_id = as.character(trp_id)
  ) |>
  dplyr::arrange(
    trp_id
  )


files_2019_2021 <-
  list.files(
    "H:/Programmering/R/byindeks/bomdata_trondheim/raw_2019_2021-3",
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )

data_2019_2021 <-
  do.call(
    bind_rows,
    lapply(
      files_2019_2021,
      readxl::read_xlsx
    )
  )

data_2019_2021_hourly <-
  data_2019_2021 %>%
  dplyr::select(
    date = Dato,
    hour = Time,
    station_code = Kjørefelt,
    class = Klasse,
    traffic = Passeringer
  ) %>%
  dplyr::filter(date != "29.02.2016") %>%
  dplyr::filter(date != "29.02.2020") %>%
  dplyr::mutate(
    date = lubridate::dmy(date),
    hour = stringr::str_sub(hour, 1, 2) %>% as.numeric(),
    station_code = stringr::str_replace(station_code, "KROP-N-2", "KROP-6"),
    station_code = stringr::str_replace(station_code, "KROP-N-3", "KROP-4"),
    station_code = stringr::str_replace(station_code, "KROP-N-4", "KROP-2"),
    station_code = stringr::str_replace(station_code, "KROP-S-2", "KROP-1"),
    station_code = stringr::str_replace(station_code, "KROP-S-3", "KROP-3"),
    lane =
      stringr::str_extract(station_code, "-[:digit:]+") |>
      stringr::str_sub(2, -1),
    station_code = stringr::str_sub(station_code, 1, 5),
    station_code = stringr::str_replace(station_code, "KLE-1", "KLETT-E6"),
    station_code = stringr::str_replace(station_code, "KLE-2", "KLETT-E6"),
    station_code = stringr::str_replace(station_code, "KLE-3", "KLETT-E6"),
    station_code = stringr::str_replace(station_code, "KLE-4", "KLETT-E6"),
    station_code = stringr::str_replace(station_code, "Rødde", "Rodde"),
    class =
      dplyr::case_when(
        class == "Ukjent" ~ "unknown",
        class == "Liten bil" ~ "light",
        class == "Stor bil" ~ "heavy"
      )
  ) %>%
  dplyr::filter(
    !(station_code %in%
        c("BUVIK", "E39-T", "HOMVI", "JONSV", "RAMPE", "THAMS",
          "TONST", "ØYSAN", "LEIST"))
  ) %>%
  dplyr::left_join(
    # Herein lies the adding of 56 and 57 to just 56
    tolling_station_codes,
    by = "station_code"
  ) %>%
  dplyr::select(
    trp_id,
    lane,
    date,
    hour,
    class,
    traffic
  ) |>
  dplyr::arrange(
    date,
    hour,
    trp_id,
    lane,
    class
  )

readr::write_rds(
  data_2019_2021_hourly,
  file = "bomdata_trondheim/data_2019_2021_hourly.rds"
  )

remove(data_2019_2021)

data_2019_2021_hourly <-
  readr::read_rds(
    file = "bomdata_trondheim/data_2019_2021_hourly.rds"
  )

## 2021-04 ----
# Status per november 2022:
# Har fått et datasett for april 2021, men dette er ikke komplett siden det
# mangler fritakspasseringer. Dersom ikke APAR en gang får komplette data må
# denne måneden anses som tapt. Dermed forsinner bomdata fra indeksen også
# for mars.

# TODO: se nøyere på data: om noe kan brukes?


## 2021-05--2022-04 Vegamot files ----
data_2021_ <-
  readr::read_csv2(
    "H:/Programmering/R/byindeks/bomdata_trondheim/raw_2021-5_/bom_2021-05_2022-04.csv",
    locale = readr::locale(encoding = "latin1")
  )

data_2021_hourly <-
  data_2021_ %>%
  dplyr::select(
    trp_id = stasjon,
    lane = felt,
    date = dato,
    hour = time,
    class = klasse,
    traffic = trafikk
  ) %>%
  dplyr::mutate(
    date = lubridate::dmy(date),
    class =
      dplyr::case_when(
        class == 1 ~ "light",
        class == 2 ~ "heavy"
      ),
    trp_id = stringr::str_replace(trp_id, ".*\\[0", ""),
    trp_id = stringr::str_replace(trp_id, "\\]", ""),
    traffic = stringr::str_replace(traffic, " ", ""),
    traffic = as.numeric(traffic)
  ) %>%
  dplyr::mutate(
    trp_id =
      dplyr::case_when(
        trp_id == "51" & lane %in% c(3, 4) ~ "512",
        TRUE ~ trp_id
      ),
    lane =
      dplyr::case_when(
        trp_id == "512" & lane == 3 ~ 1,
        trp_id == "512" & lane == 4 ~ 2,
        trp_id == "56" & lane == 2 ~ 6,
        trp_id == "56" & lane == 3 ~ 4,
        trp_id == "56" & lane == 4 ~ 2,
        trp_id == "57" & lane == 2 ~ 1,
        trp_id == "57" & lane == 3 ~ 3,
        TRUE ~ lane
      ),
    # Merging the two Kroppan bru tolling stations 56 and 57
    trp_id = stringr::str_replace(trp_id, "57", "56"),
    lane = as.character(lane)
  ) |>
  dplyr::filter(
    # Just one day with data in May 2022 thus far
    date < "2022-05-01"
  ) |>
  dplyr::arrange(
    date,
    hour,
    trp_id,
    lane
  )

readr::write_rds(
  data_2021_hourly,
  file = "bomdata_trondheim/data_2021_hourly.rds"
)

remove(data_2021_)

data_2021_hourly <-
  readr::read_rds(
    file = "bomdata_trondheim/data_2021_hourly.rds"
  )


## 2021-05- APAR test ----
# Just a test with file fetched via PowerBI

apar_apr_2022 <-
  readr::read_csv(
    "H:/Programmering/R/byindeks/bomdata_trondheim/apar/april_2022.csv",
    locale = readr::locale(encoding = "latin1")
  ) |>
  dplyr::select(
    trp_id = 'toll station code',
    date,
    class = 'vehicle class ID',
    traffic_apar = 'Accepted passages'
  ) |>
  dplyr::mutate(
    trp_id = as.character(trp_id),
    date = lubridate::date(date),
    class = dplyr::case_when(
      class == 1 ~ "light",
      class == 2 ~ "heavy",
      TRUE ~ "unknown"
    )
  )

compare_apar <-
  apar_apr_2022 |>
  dplyr::left_join(
    tolling_data_daily,
    by = c("trp_id", "date", "class")
  ) |>
  dplyr::select(
    trp_id,
    date,
    class,
    traffic,
    traffic_apar
  ) |>
  dplyr::mutate(
    diff = traffic - traffic_apar
  )

write.csv2(
  compare_apar,
  file = "sammenligning_apar.csv",
  row.names = F
)


## 2021-05- APAR API ----

# TODO: compare APAR API data with data from Vegamot files
# Awaiting unfiltered and complete data in APAR

# TODO: Prepare data quality check independent of source


# Daily ----
# Quality check: look first at daily traffic per lane
# If anything strange, look at hourly

tolling_data_daily_lane <-
  dplyr::bind_rows(
    data_2019_2021_hourly,
    data_2021_hourly
  ) %>%
  dplyr::filter(
    trp_id %in% tolling_station_ids
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
    day = lubridate::mday(date),
    month = lubridate::floor_date(date, "month"),
    year = lubridate::year(date)
  )

tolling_data_daily <-
  tolling_data_daily_lane |>
  dplyr::group_by(
    trp_id,
    date,
    class
  ) %>%
  dplyr::summarise(
    traffic = sum(traffic),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    day = lubridate::mday(date),
    month = lubridate::floor_date(date, "month"),
    year = lubridate::year(date)
  )

readr::write_rds(
  tolling_data_daily,
  file = "bomdata_trondheim/tolling_data_daily.rds"
)

tolling_data_daily <-
  readRDS(
    file = "bomdata_trondheim/tolling_data_daily.rds"
  )


## Plot ----
# Plot to see if data are ok
tolling_data_daily_lane %>%
  dplyr::filter(
    trp_id == 51,
    year %in% c(2021)
  ) %>%
  ggplot(aes(day, traffic, color = lane, linetype = class)) +
  geom_line(size = 1) +
  facet_grid(
    rows = vars(month)
  ) +
  theme_minimal()


# Monthly ----
tolling_data_monthly <-
  tolling_data_daily %>%
  dplyr::group_by(
    trp_id,
    month
  ) %>%
  dplyr::summarise(
    traffic = sum(traffic),
    .groups = "drop"
  )

tolling_data_monthly_by_class <-
  tolling_data_daily %>%
  dplyr::group_by(
    trp_id,
    month,
    class
  ) %>%
  dplyr::summarise(
    traffic_by_class = sum(traffic),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    tolling_data_monthly,
    by = c(
      "trp_id",
      "month"
      )
  ) %>%
  dplyr::mutate(
    traffic_ratio =
      round(
        100 * traffic_by_class / traffic,
        digits = 2)
  )

readr::write_rds(
  tolling_data_monthly_by_class,
  file = "data_index_raw/trd_toll_data_2019_monthly.rds",
)

tolling_data_monthly_by_class <-
  readr::read_rds(
    file = "data_index_raw/trd_toll_data_2019_monthly.rds",
  )


# Exclusions ----

# Tungasletta høy andel ukjente juli og aug 2018, ukjentandel er over 30 %!
# 54 2021-08
# 85 and 86: 2021-01, 2021-03--2021-04 (high ratio of unknowns intermittently)
# TODO:
# Keep Nord for Sluppen bru or Bjørndalen when Oslovegen is closed?

tolling_data_monthly_by_class_excluded <-
  tolling_data_monthly_by_class %>%
  dplyr::select(-traffic_ratio, -traffic) %>%
  dplyr::rename(traffic = traffic_by_class) %>%
  #dplyr::filter(!(felt == "TUNGA" & aar_maaned == "2018-07-01")) %>%
  #dplyr::filter(!(felt == "TUNGA" & aar_maaned == "2018-08-01"))
  dplyr::filter(!(trp_id == "54" & month == "2021-03-01")) %>%
  dplyr::filter(!(trp_id == "54" & month == "2021-08-01")) %>%
  dplyr::filter(!(trp_id == "54" & month == "2022-01-01")) %>%
  dplyr::filter(!(trp_id == "85" & month == "2021-01-01")) %>%
  dplyr::filter(!(trp_id == "85" & month == "2021-03-01")) %>%
  dplyr::filter(!(trp_id == "85" & month == "2021-04-01")) %>%
  dplyr::filter(!(trp_id == "86" & month == "2021-01-01")) %>%
  dplyr::filter(!(trp_id == "86" & month == "2021-03-01")) %>%
  dplyr::filter(!(trp_id == "86" & month == "2021-04-01"))

tolling_data_monthly_excluded_total <-
  tolling_data_monthly_by_class_excluded %>%
  dplyr::group_by(
    trp_id,
    month
  ) %>%
  dplyr::summarise(
    traffic = sum(traffic),
    .groups = "drop"
  ) %>%
  dplyr::mutate(class = "all")

tolling_data_monthly_by_all_classes <-
  dplyr::bind_rows(
    tolling_data_monthly_excluded_total,
    tolling_data_monthly_by_class_excluded
  ) %>%
  dplyr::mutate(
    year = lubridate::year(month),
    month = lubridate::month(month)
  )


# felt_og_stasjon <-
#   all_data_monthly_by_all_classes %>%
#   dplyr::distinct_at(vars(stasjon, felt)) %>%
#   dplyr::left_join(bomfeltkoder)
#
# write.csv2(
#   felt_og_stasjon,
#   file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_felt_og_stasjon.csv",
#   row.names = F
#   )


# TRP index ----
calculate_monthly_index_for_tolling_stations <-
  function(monthly_class_data, baseyear) {

    basedata <-
      monthly_class_data %>%
      dplyr::filter(year == baseyear)

    calcdata <- monthly_class_data %>%
      dplyr::filter(year == baseyear + 1)

    indexdata <-
      dplyr::inner_join(
        basedata,
        calcdata,
        by = c("month", "trp_id", "class"),
        suffix = c("_base", "_calc"),
      ) %>%
      dplyr::group_by(
        trp_id,
        class,
        month
      ) %>%
      dplyr::summarise(
        monthly_volume_base = sum(traffic_base),
        monthly_volume_calc = sum(traffic_calc),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        index_p =
          (monthly_volume_calc / monthly_volume_base - 1) * 100 %>%
          round(digits = 2),
        month_as_date =
          lubridate::ymd(
            paste(baseyear + 1, month, "1", sep = "-")
          )
      )
}

# bomindeks_2017 <- all_data_monthly_by_all_classes %>%
#   dplyr:: select(-stasjon, -aar_maaned) %>%
#   #dplyr::filter(klasse == "Liten_bil") %>%
#   calculate_monthly_index_for_tolling_stations(2016)
#
# bomindeks_2018 <- all_data_monthly_by_all_classes %>%
#   dplyr:: select(-stasjon, -aar_maaned) %>%
#   #dplyr::filter(klasse == "Liten_bil") %>%
#   calculate_monthly_index_for_tolling_stations(2017)
#
# bomindeks_2019 <- all_data_monthly_by_all_classes %>%
#   dplyr:: select(-stasjon, -aar_maaned) %>%
#   #dplyr::filter(klasse == "Liten_bil") %>%
#   calculate_monthly_index_for_tolling_stations(2018)

tolling_station_index_2020 <-
  tolling_data_monthly_by_all_classes %>%
  calculate_monthly_index_for_tolling_stations(2019)

tolling_station_index_2021 <-
  tolling_data_monthly_by_all_classes %>%
  calculate_monthly_index_for_tolling_stations(2020)

tolling_station_index_2022 <-
  tolling_data_monthly_by_all_classes %>%
  calculate_monthly_index_for_tolling_stations(2021)

tolling_station_indices <-
  dplyr::bind_rows(
    #bomindeks_2017,
    #bomindeks_2018,
    #bomindeks_2019,
    tolling_station_index_2020,
    tolling_station_index_2021,
    tolling_station_index_2022
  )

write.csv2(
  tolling_station_indices,
  file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser.csv",
  row.names = F
  )

tolling_station_indices <-
  read.csv2(
    "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser.csv"
  )

# TODO: Dekningsgrad for antall måneder

# Not all toll stations have value in latest month
tolling_station_indices_latest_month_per_year <-
  tolling_station_indices %>%
  dplyr::mutate(year = year(month_as_date)) %>%
  dplyr::group_by(
    year
  ) %>%
  dplyr::summarise(
    month = max(month),
    .groups = "drop"
  )

# As of June 2022, data for April 2021 is missing. Treat them as present:
tolling_station_indices_latest_month_per_year$month[c(3)] <- 4

tolling_station_indices_yearly <-
  tolling_station_indices %>%
  dplyr::mutate(
    year = year(month_as_date),
    trp_id = as.character(trp_id)
  ) %>%
  dplyr::group_by(
    trp_id,
    year,
    class
  ) %>%
  dplyr::summarise(
    #month = max(month), # latest month per year
    base_volume = sum(monthly_volume_base),
    calc_volume = sum(monthly_volume_calc),
    index_p = (calc_volume / base_volume - 1) * 100,
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    tolling_station_indices_latest_month_per_year,
    by = "year"
  )

saveRDS(
  tolling_station_indices_yearly,
  file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.rds"
)

# write.csv2(
#   tolling_station_indices_yearly,
#   file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.csv",
#   row.names = F
# )

# Plott for å se etter avvik: bomdata_trondheim.Rmd

city_monthly_toll_indices <-
  tolling_station_indices %>%
  dplyr::group_by(
    trp_id,
    month_as_date,
    class
  ) %>%
  dplyr::summarise(
    base_volume = sum(monthly_volume_base),
    calc_volume = sum(monthly_volume_calc),
    index_p = (calc_volume / base_volume - 1) * 100,
    .groups = "drop"
  ) #%>%
  #dplyr::left_join(felt_og_stasjon)

write.csv2(
  city_monthly_toll_indices,
  file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_bymaanedsindekser.csv",
  row.names = F
)
