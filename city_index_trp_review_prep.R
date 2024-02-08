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
  dplyr::distinct()


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






# point out unsuited trps
trps_unsuited <- c(
  "74885V319528",
  "07793V319942",
  "50233V320584",
  "22478V1814807",
  "68684V319527",
  "40190V319527",
  "55439V320125",
  "45995V320592",
  "03108V320583",
  "45342V320223",
  "10028V320295",
  "93189V320582",
  "55507V319881",
  "71535V319524",
  "03819V320689",
  "74137V319919",
  "12478V320582",
  "84064V320581",
  "71798V319583",
  "13433V319582",
  "35382V1727514",
  "50741V1727509",
  "10569V2580836",
  "74012V320634",
  "95626V320634",
  "13715V2721330",
  "63247V3131641",
  "58400V2721295",
  "33926V2721315",
  "91556V2721320",
  "20586V2721334",
  "41451V320581",
  "00462V320124",
  "13000V2783697",
  "94180V2726102",
  "65096V2726170",
  "06011V2726196"
)

trps_filtered_classified <-
  trps_filtered %>%
  dplyr::mutate(unsuited = trp_id %in% trps_unsuited,
                status = dplyr::case_when(
                  included == TRUE ~ "Er med",
                  included == FALSE & unsuited == FALSE ~ "Tas med",
                  included == FALSE & unsuited == TRUE ~ "Utelates",
                ))

n_to_include <-  trps_filtered_classified %>%
  dplyr::filter(status == "Tas med") %>%
  nrow()

## AADT ----
adt <- get_aadt_for_trp_list(trps_filtered$trp_id)

adt_filtered <- adt %>%
  #dplyr::filter(length_range == "[..,5.6)") %>%
  #dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  #dplyr::filter(length_quality > 90) %>%
  #dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  #dplyr::filter(year >= 2019) %>%
  dplyr::filter(year == 2020) %>%
  dplyr::select(trp_id, adt)

trps_filtered_adt <- trps_filtered_classified %>%
  dplyr::left_join(adt_filtered, by = "trp_id")