## 2019--2021-03 Vegamot files ----

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
        class == "Ukjent" ~ "ukjent",
        class == "Liten bil" ~ "lette",
        class == "Stor bil" ~ "tunge"
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
# Datasettet for april 2021 skal etter sigende mangle fritakspasseringer.
# Men etter en titt på dataene så virker det ikke som om dette er et problem.

april_2021_daily <-
  readxl::read_xlsx(
    path = "H:/Programmering/R/byindeks/bomdata_trondheim/raw_2021-5_/bom_2021-04.xlsx"
  ) |>
  dplyr::select(
    date = Dato,
    Stasjon,
    lane = 'Kjørefelt',
    class = Klasse,
    traffic = Passeringer
  ) |>
  dplyr::mutate(
    date = lubridate::as_date(date),
    trp_id = stringr::str_extract(Stasjon, "\\[?[:digit:]+\\]{1}") |>
      stringr::str_sub(2, -2) |>
      base::as.numeric() |>
      base::as.character(),
    trp_id =
      dplyr::case_when(
        trp_id %in% c("10034", "10035") ~ "512",
        trp_id %in% c("57") ~ "56",
        TRUE ~ trp_id
      ),
    lane = base::as.character(lane),
    class =
      dplyr::case_when(
        class == "Ukjent" ~ "ukjent",
        class == "1" ~ "lette",
        class == "2" ~ "tunge"
      )
  ) |>
  dplyr::filter(
    trp_id %in% tolling_station_ids
  ) |>
  dplyr::select(
    trp_id,
    lane,
    date,
    class,
    traffic
  ) |>
  # Data set has a hidden parameter (hour rule, maybe) - summarize
  dplyr::group_by(
    trp_id,
    lane,
    date,
    class
  ) |>
  dplyr::summarise(
    traffic = sum(traffic),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    day = lubridate::mday(date),
    month = lubridate::floor_date(date, "month"),
    year = lubridate::year(date)
  )
