source("rmd_setup.R")
source("get_from_trafficdata_api.R")
source("get_from_nvdb_api.R")

library(readxl)

# Tidying data from tolling stations

# The 20 to use
tolling_station_ids <-
  c("51", "52", "53", "54", "55", "56", "58",
    "59", "60", "61", "62", "64", "65", "66", "67",
    "68", "69", "85", "86", "72")


# Tolling station info ----
kommunenr <- "5001"
kommunenavn <-
  hent_kommune(kommunenr)[[1]]

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


# Read hourly ----

# Different formats:
# 2017--2021-03
# 2021--05-
# Normalizing data
# Storing together


## 2019--2021-03 ----

# Adding station ID
# Implicitly adding the two Kroppan bru to one trp
tolling_station_codes <-
  readr::read_csv2(
    "H:/Programmering/R/byindeks/bomdata_trondheim/tolling_station_codes.csv",
    locale = readr::locale(encoding = "latin1")
  ) %>%
  dplyr::mutate(
    trp_id = as.character(trp_id)
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
    !(station_code %in% c("BUVIK", "E39-T", "HOMVI", "JONSV",
                          "RAMPE", "Rodde", "THAMS",
                          "TONST", "ØYSAN", "LEIST"))
  ) %>%
  dplyr::left_join(
    # Herein lies the adding of 56 and 57 to just 56
    tolling_station_codes,
    by = "station_code"
  ) %>%
  dplyr::select(
    trp_id,
    date,
    hour,
    class,
    traffic
  )

saveRDS(
  data_2019_2021_hourly,
  file = "bomdata_trondheim/data_2019_2021_hourly.rds"
  )

remove(data_2019_2021)


## 2021--05- ----
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
    # Adding the two Kroppan bru tolling stations 56 and 57
    trp_id = stringr::str_replace(trp_id, "57", "56"),
    trp_id = stringr::str_replace(trp_id, ".*\\[0", ""),
    trp_id = stringr::str_replace(trp_id, "\\]", ""),
    traffic = stringr::str_replace(traffic, " ", ""),
    traffic = as.numeric(traffic)
  ) %>%
  dplyr::filter(
    # Just one day with data in May 2022 thus far
    date < "2022-05-01"
  ) %>%
  dplyr::group_by(
    trp_id,
    date,
    hour,
    class
  ) %>%
  dplyr::summarise(
    traffic = sum(traffic),
    .groups = "drop"
  )

remove(data_2021_)


# Daily ----
# Assuming all days are complete, exclusions will be done on months
tolling_data_daily <-
  dplyr::bind_rows(
    data_2019_2021_hourly,
    data_2021_hourly
  ) %>%
  dplyr::filter(
    trp_id %in% tolling_station_ids
  ) %>%
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

saveRDS(
  tolling_data_daily,
  file = "bomdata_trondheim/tolling_data_daily.rds"
)

# tolling_data_daily <-
#   readRDS(
#     file = "bomdata_trondheim/tolling_data_daily.rds"
#   )

# Plot to see if data are ok
tolling_data_daily %>%
  dplyr::filter(
    trp_id == 56,
    year %in% c(2021, 2022)
  ) %>%
  ggplot(aes(day, traffic, color = class)) +
  geom_line() +
  facet_grid(
    rows = vars(month)
  )


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

saveRDS(
  tolling_data_monthly_by_class,
  file = "data_index_raw/trd_toll_data_2019_monthly.rds",
)

tolling_data_monthly_by_class <-
  readRDS(
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
  dplyr::filter(!(trp_id == "55" & month == "2021-03-01")) %>%
  dplyr::filter(!(trp_id == "55" & month == "2021-04-01")) %>%
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

# maanedsindekser <-
#   read.csv2(
#     "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser.csv"
#   )

# TODO: Dekningsgrad for antall måneder
tolling_station_indices_yearly <-
  tolling_station_indices %>%
  dplyr::mutate(year = year(month_as_date)) %>%
  dplyr::group_by(
    trp_id,
    year,
    class
  ) %>%
  dplyr::summarise(
    month = max(month), # latest month per year
    base_volume = sum(monthly_volume_base),
    calc_volume = sum(monthly_volume_calc),
    index_p = (calc_volume / base_volume - 1) * 100,
    .groups = "drop"
  ) #%>%
  #dplyr::left_join(felt_og_stasjon)

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
