# Road events (e.g. closures) dumped from GCP (SAGA 1)

library(tidyverse)
library(sf)

# Fetch selected events by SQL in Trino (Starburst?) using DataGrip in "traind" datalab

events <-
  readr::read_csv(
    "H:/my_data/events_20251202.csv"
    #n_max = 20
  ) |> 
  dplyr::mutate(
    interval = lubridate::interval(date_start, date_end),
    info_text =
      paste0(
        note, "<br/>",
        description, "<br/>",
        "Alle kjøretøy:", all_vehicles, "<br/>",
        lokasjon_beskrivelse, "<br/>",
        duration_days, " dager", "<br/>",
        interval
      )
  ) |> 
  dplyr::filter(
    # Remove beveglig arbeid
    !(stringr::str_detect(info_text, "evegelig") & stringr::str_detect(info_text, "tengt", negate = TRUE))
  ) |> 
  sf::st_as_sf(
    wkt = "geografi",
    crs = "wgs84"
  ) |> 
  dplyr::select(
    event_id = t_id,
    info_text,
    interval
  )

# A look at "bevegelig"
moving_events <-
  events |> 
  sf::st_drop_geometry() |> 
  dplyr::filter(
    !(stringr::str_detect(info_text, "evegelig") & stringr::str_detect(info_text, "tengt", negate = TRUE))
  )
# Just keep them for now, though most of them probably won't matter.

readr::write_rds(
  events,
  "H:/my_data/events_20251202.rds"
)
