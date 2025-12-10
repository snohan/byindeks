# Road events (e.g. closures) dumped from GCP (SAGA 1)

library(tidyverse)
library(sf)


# Events from Saga ----
events <-
  dplyr::bind_rows(
    jsonlite::fromJSON("events/events_2023_1.json"),
    jsonlite::fromJSON("events/events_2023_2.json"),
    jsonlite::fromJSON("events/events_2024_01.json")
  ) |>
  dplyr::filter(
    geography != "GEOMETRYCOLLECTION EMPTY"
  ) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::ends_with("time"),
      .fns = ~ lubridate::ymd_hms(.x, tz = "CET")
    ),
    duration = base::difftime(endTime, startTime, units = "hours"), # All are > 48
    interval = lubridate::interval(startTime, endTime)
  ) |>
  sf::st_as_sf(
    wkt = "geography",
    crs = "wgs84"
  )

readr::write_rds(
  events,
  "events/events.rds"
)


# New version, Saga 2
# Fetch selected events by SQL in Trino (Starburst?) using DataGrip in "traind" datalab

events <-
  readr::read_csv(
    "H:/my_data/events_20251202.csv"
    #n_max = 20
  ) |> 
  dplyr::mutate(
    interval = lubridate::interval(date_start, date_end)
  ) |> 
  sf::st_as_sf(
    wkt = "geografi",
    crs = "wgs84"
  ) |> 
  dplyr::select(
    -date_start, - date_end, -vei_nummer
  )

readr::write_rds(
  events,
  "H:/my_data/events_20251202.rds"
)
