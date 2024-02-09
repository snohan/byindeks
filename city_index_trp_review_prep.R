# Traffic Data API calls to get points metadata and aadt
{
source("rmd_setup.R")
source("get_from_trafficdata_api.R")
source("split_road_system_reference.R")
}


# Municipalities ----
municipalities <-
  get_municipalities()


# TRPs ----
points <-
  get_points() |>
  dplyr::distinct(trp_id, .keep_all = T)

trp_time_span <- get_trp_data_time_span()


# Nord-Jæren 2023 ----
## TRPs used today ----
index_year <- 2023
index_month <- 12
city_number <- 952

municipality_names <- c("Randaberg", "Sola", "Stavanger", "Sandnes")

municipality_numbers <-
  municipalities |>
  dplyr::filter(
    municipality_name %in% municipality_names
  )

city_index <- get_published_index_for_months(city_number, index_year, index_month)
pointindex <- get_published_pointindex_for_months(city_number, index_year, index_month)

city_trps <- pointindex[[1]]
city_name <- city_index$area_name[1]


## TRPs in area ----
trps_filtered <-
  points |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    operational_status != "RETIRED"
  ) |>
  dplyr::mutate(
    operational_status = stringr::str_to_lower(operational_status, locale = "no")
  ) |>
  dplyr::filter(
    municipality_name %in% c("Randaberg", "Sola", "Stavanger", "Sandnes")
  ) |>
  dplyr::mutate(included = trp_id %in% city_trps) |>
  dplyr::left_join(
    trp_time_span,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(!is.na(first_data_with_quality_metrics)) |>
  split_road_system_reference() |>
  dplyr::select(
    trp_id,
    name,
    operational_status,
    road_reference,
    road_category_and_number,
    municipality_name,
    lat, lon,
    included,
    first_data, first_data_with_quality_metrics,
    latest_daily_traffic
  )


## AADT ----
trp_aadts <-
  trps_filtered$trp_id |>
  get_aadt_for_trp_list() |>
  dplyr::filter(
    year == 2023
  ) |>
  dplyr::mutate(
    valid_length_percent = round(valid_length_volume / adt * 100, digits = 1)
  ) |>
  dplyr::mutate(
    good_enough = dplyr::case_when(
      (coverage > 95 & valid_length_percent > 99) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::select(
    trp_id,
    adt,
    good_enough
  )


## TRP tidy ----
trps_unsuited <- c(
  # For mange TRP med samme trafikk
  "03108V320583",
  "81631V1727485",
  "07051V2725969",
  "92879V2726065",
  # Stengte veger 2023
  "83652V319725" # Strandgata nord
)


trp_aadt_tidy <-
  trps_filtered |>
  dplyr::left_join(
    trp_aadts,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(
    operational_status == "operational",
    good_enough == TRUE,
    # øyene nord for Randabergs fastland skal ikke med
    lat < 59.05,
    # områder langt øst skal ikke være med
    lon < 5.82,
    !(trp_id %in% trps_unsuited)
  ) |>
  dplyr::mutate(
    status = dplyr::case_when(
      included == TRUE ~ "Opprinnelig",
      included == FALSE ~ "Ny"
    ),
    trp_label = paste(name, road_category_and_number, adt, sep = "<br/>"),
    trp_label = lapply(trp_label, htmltools::HTML)
  )



# Traffic links ----
# Geojson from ADM
links <-
  sf::st_read("C:/Users/snohan/Desktop/traffic_links_2023.geojson") |>
  dplyr::rename(
    municipality_ids = municipalityIds,
    trp_id = primaryTrpId,
  ) |>
  dplyr::select(
    id,
    municipality_ids,
    trp_id,
    primaryTrpIds,
    associatedTrpIds,
    length,
    trafficVolumes
  ) |>
  tidyr::unnest(
    cols = municipality_ids
  ) |>
  dplyr::filter(
    municipality_ids %in% municipality_numbers$municipality_number
  ) |>
  dplyr::select(
    -municipality_ids
  ) |>
  # Remove duplicates (links crossing municipality borders inside city)
  dplyr::distinct() |>
  # manually update trp_id on links missing correct info
  dplyr::mutate(
    trp_id = dplyr::case_when(
      id == "1018316790" ~ "61929V2422166",
      id == "1018204697" ~ "96553V2725986",
      id == "1018200234" ~ "65096V2726170",
      id == "1018200253" ~ "94180V2726102",
      TRUE ~ trp_id
    )
  )


## Traffic work ----
traffic_work <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    length,
    trp_id,
    trafficVolumes
  ) |>
  # Some may be missing, often ferry links as per feb 24
  dplyr::filter(
    !(is.na(trafficVolumes))
  ) |>
  dplyr::mutate(
    volumes = purrr::map(trafficVolumes, ~ jsonlite::fromJSON(.x))
  ) |>
  tidyr::unnest(
    volumes
  ) |>
  dplyr::filter(
    trafficVolumeType == "GUESSTIMATED"
  ) |>
  dplyr::select(
    id,
    length,
    trp_id,
    traffic_work = trafficWorkValue
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(traffic_work) / 1e9,
    n_links = n()
  )

traffic_work_measured <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    length,
    trp_id,
    trafficVolumes
  ) |>
  # Some may be missing, often ferry links as per feb 24
  dplyr::filter(
    !(is.na(trafficVolumes)),
    trp_id %in% trp_aadt_tidy$trp_id
  ) |>
  dplyr::mutate(
    volumes = purrr::map(trafficVolumes, ~ jsonlite::fromJSON(.x))
  ) |>
  tidyr::unnest(
    volumes
  ) |>
  dplyr::filter(
    trafficVolumeType == "GUESSTIMATED"
  ) |>
  dplyr::select(
    id,
    length,
    trp_id,
    traffic_work = trafficWorkValue
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(traffic_work) / 1e9,
    n_links = n()
  )

traffic_work_measured$traffic_work_mill_km / traffic_work$traffic_work_mill_km


## Chosen links ----
links_with_chosen_trp <-
  links |>
  dplyr::inner_join(
    trp_aadt_tidy,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    id,
    trp_id,
    name,
    length,
    road_reference,
    road_category_and_number,
    municipality_name,
    lat, lon,
    included,
    adt,
    status,
    trp_label
  )

# missing_trps <-
#   trp_aadt_tidy |>
#   dplyr::filter(
#     !(trp_id %in% links_with_chosen_trp$trp_id)
#   )


readr::write_rds(
  links_with_chosen_trp,
  "chosen_links_nj_2023.rds"
)



