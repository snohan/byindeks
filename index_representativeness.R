# Is the selection of city index points representative for the area?

# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  library(tidyverse)
  library(readxl)
  library(car)
  library(pwr)
  library(tidygraph)
  library(ggraph)
  library(paletteer)
  library(leaflet)
  library(sf)
  source("get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/traffic_link_functions.R")
  source("H:/Programmering/R/byindeks/leaflet_nvdb_map_setup.R")
}

# Later, need to keep only continuous TRPs on links
trp_continuous <-
  get_points() |>
  dplyr::filter(
    registration_frequency == "CONTINUOUS"
  ) |>
  dplyr::select(
    trp_id
  ) |>
  dplyr::distinct()


# Link population ----
# Need geometry for map plot

# Need to know layer names if a query should limit rows during test readings.
#layers <- sf::st_layers("C:/Users/snohan/Desktop/traffic_links_2023_2024-10-08.geojson")
#names(links)

links <-
  sf::st_read(
    "C:/Users/snohan/Desktop/traffic_links_2024_2025-02-13.geojson",
    as_tibble = TRUE
    #query = "SELECT * FROM \"traffic_links_2024_2025-01-28\" LIMIT 150"
  ) |>
  dplyr::select(
    link_id = id,
    roadSystemReferences,
    from = startTrafficNodeId,
    to = endTrafficNodeId,
    municipalityIds,
    associatedTrpIds,
    hasOnlyPublicTransportLanes,
    isFerryRoute,
    function_class = functionClass,
    length,
    trafficVolumes
  ) |>
  dplyr::mutate(
    function_class = as.factor(function_class)
  ) |>
  dplyr::filter(
    hasOnlyPublicTransportLanes == FALSE,
    isFerryRoute == FALSE
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    trp_id = stringr::str_extract_all(associatedTrpIds, "(?<=\")[:alnum:]+(?=\")")
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    # From list to character
    point_id = purrr::map(trp_id, ~ purrr::pluck(., 1)) # NB! What if there is more than one!
  ) |>
  tidyr::unnest(
    point_id,
    keep_empty = TRUE
    # TODO: remove periodic trps
  ) |>
  dplyr::mutate(
    point_id =
      dplyr::case_when(
        point_id %in% trp_continuous$trp_id ~ point_id,
        TRUE ~ NA_character_
      )
  ) |>
  dplyr::select(
    -hasOnlyPublicTransportLanes,
    -isFerryRoute,
    -associatedTrpIds,
    -trp_id
  ) |>
  sf::st_as_sf()

traffic_volumes <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id,
    trafficVolumes
  ) |>
  dplyr::filter(
    !is.na(trafficVolumes)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    traffic_volumes = list(jsonlite::fromJSON(trafficVolumes))
  ) |>
  # Need to remove empty lists before unnesting.
  dplyr::filter(
    purrr::map_int(list(traffic_volumes), ~length(.)) > 0
  ) |>
  tidyr::unnest(
    traffic_volumes
  ) |>
  dplyr::filter(
    year == 2023,
    trafficVolumeResolution == "ADT"
  ) |>
  dplyr::select(
    link_id,
    trafficVolumeValue,
    #year,
    #coverage,
    trafficWorkValue
    #correctedStandardError,
    #sourceType,
    #registrationFrequency
  ) |>
  dplyr::summarise(
    traffic_volume = mean(trafficVolumeValue) |> round(),
    traffic_work_km = mean(trafficWorkValue),
    .by = "link_id"
  )

# Filter initially by municipality, but also remove links on the outer border of area
all_municipality_ids <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id,
    all_municipality_ids = municipalityIds
  )

# Need to filter links that do not intersect
not_intersected <- function(x, y) !sf::st_intersects(x, y)


## City TRPs ----
# Need to know what the sample is supposed to be (if all TRPs give good data)

city_id <-
  c(960, 952, 8952, 959, 1952, 955, 19953, 18952)

city_names <-
  c("Trondheim", "Nord-Jæren", "Bergen", "Oslo", "Buskerudbyen", "Grenland", "Kristiansand", "Nedre Glomma")

city_names_and_ids <-
  tibble::tibble(
    city_id,
    city_names
  )

city_trp_info <-
  purrr::map(
    city_id,
    ~ readr::read_rds(
      file = paste0(
        "index_trp_metadata/trp_",
        .x,
        ".rds"
      )
    ) |>
      dplyr::mutate(
        city_id = .x
      )
  ) |>
  dplyr::bind_rows() |>
  dplyr::select(
    city_id,
    p_id = trp_id,
    name
  ) |>
  dplyr::filter(
    # Removing toll station "Nord for Sluppen bru" which has been moved
    !(p_id == "1017875672")
  ) |>
  dplyr::left_join(
    city_names_and_ids,
    by = join_by(city_id)
  )

city_info_stats <-
  city_trp_info |>
  dplyr::summarise(
    n_trp = n(),
    .by = city_names
  )


## Population Nord-Jæren ----
municipality_ids_nj <- c(1127, 1103, 1124, 1108)

# Need two alternative population definitions to compare
# 1. All links in all municipalities: "whole"
# 2. Just the central nodes in urban area: "central"

# 1. All links
links_nj_whole <-
  links |>
  tidyr::unnest_longer(
    municipalityIds,
    values_to = "municipality_id"
  ) |>
  dplyr::filter(
    municipality_id %in% municipality_ids_nj
  ) |>
  dplyr::select(
    -municipality_id,
    -trafficVolumes
  ) |>
  # remove duplicate links (crossing municipality boundaries)
  dplyr::distinct() |>
  dplyr::mutate(
    city_trp =
      dplyr::case_when(
        point_id %in% city_trp_info$p_id ~ TRUE,
        TRUE ~ FALSE
      )
  ) |>
  dplyr::left_join(
    traffic_volumes,
    by = join_by(link_id)
  ) |>
  dplyr::filter(
    !is.na(traffic_work_km),
    !is.na(function_class)
  ) |>
  sf::st_as_sf()

# Save file for use in report
readr::write_rds(links_nj_whole, "representativity/link_population_nj_whole.rds")


# 2. Central links
nj_polygon_north <-
  tibble::tibble(
    lon = c(
      5.7866566,
      6.1565635,
      5.8272271,
      5.4658784
    ),
    lat = c(
      59.0119612,
      59.284531,
      59.3473959,
      59.0678125
    )
  ) |>
  tibble::rowid_to_column("id") |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("POLYGON")

links_nj_central_1 <-
  links |>
  tidyr::unnest_longer(
    municipalityIds,
    values_to = "municipality_id"
  ) |>
  dplyr::filter(
    municipality_id %in% municipality_ids_nj,
    function_class %in% c("A", "B", "C", "D")
  ) |>
  dplyr::select(
    -municipality_id,
    -trafficVolumes
  ) |>
  # remove duplicate links (crossing municipality boundaries)
  dplyr::distinct() |>
  # remove links crossing borders out of area
  dplyr::left_join(
    all_municipality_ids,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rowwise() |>
  dplyr:::mutate(
    not_border_crossing =
      purrr::map_lgl(
        unlist(all_municipality_ids),
        ~ all(. %in% municipality_ids_nj)
      ) |> all()
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    not_border_crossing == TRUE
  ) |>
  dplyr::left_join(
    traffic_volumes,
    by = join_by(link_id)
  ) |>
  dplyr::select(
    -not_border_crossing,
    -all_municipality_ids
  ) |>
  dplyr::filter(
    !is.na(traffic_work_km)
  ) |>
  sf::st_as_sf() |>
  # areas in northern part are former municipalities not to be included
  sf::st_filter(nj_polygon_north, .predicate = not_intersected)

# Need to remove links (with function class D?) in eastern part
nj_polygon_east <-
  tibble::tibble(
    lon = c(
      5.7653664,
      5.8236117,
      6.4870498,
      6.0794556
    ),
    lat = c(
      58.9062453,
      58.815903,
      58.8686332,
      59.0519824
    )
  ) |>
  tibble::rowid_to_column("id") |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("POLYGON")

links_nj_east_D <-
  links_nj_central |>
  sf::st_filter(nj_polygon_east, .predicate = st_intersects) #|>
  #dplyr::filter(
  #  function_class == "D"
  #)

links_nj_central <-
  links_nj_central_1 |>
  dplyr::filter(
    !(link_id %in% links_nj_east_D$link_id),
    # Removing some links in Figgjo
    !(link_id == "0.0-1.0@320083"),
    !(link_id == "0.80529774-0.9731575@320669"),
    !(link_id == "0.72327267-0.80529774@320669"),
    !(link_id == "0.9731575-1.0@320669")
  ) |>
  dplyr::mutate(
    city_trp =
      dplyr::case_when(
        point_id %in% city_trp_info$p_id ~ TRUE,
        TRUE ~ FALSE
      )
  )

# Look at missing data
# links_nj_missing <-
#   links_nj |>
#   dplyr::filter(
#     is.na(traffic_work)
#   )

# Map
link_population |> map_links_with_function_class()

# Save file for use in report
readr::write_rds(link_population, "representativity/link_population_nj.rds")


# Traffic graph ----
# Need the nodes belonging to the links (should be downloaded same day as links)
# nodes <-
#   sf::st_read("C:/Users/snohan/Desktop/traffic-nodes-2024_2025-02-13.geojson") |>
#   sf::st_drop_geometry() |>
#   dplyr::select(id) |>
#   tibble::as_tibble()

#city_graph <- create_graph_from_links(links_nj_central)

# In fact, need only the line graph (no need for the traffic nodes explicitly)
city_line_graph <- create_line_graph(links_nj_central)

## Mean distance to chosen nodes ----
mean_dist <- calculate_mean_distance_to_city_index_points(city_line_graph)


# Sample calculations ----
# Use past rolling index results, and calculate representativity over time

city_id_samples <- c(952, 8952, 1952)
#city_names <- c("Nord-Jæren", "Bergen", "Buskerudbyen")

city_trp_rolling_index <-
  purrr::map(
    city_id_samples,
    ~ readxl::read_xlsx(
        path = paste0(
          "data_indexpoints_tidy/tallmateriale_",
          .x,
          ".xlsx"
        ),
        sheet = "punkt_3_aar_glid_indeks"
    ) |>
      dplyr::mutate(
        city_id = .x
      )
  ) |>
  dplyr::bind_rows() |>
  dplyr::select(
    city_id,
    trp_id,
    last_month_in_index
  )


# Least n TRPs ----
# aka power analysis
# Say we want to detect wether traffic is changed, i.e. different from 0 % change.

# What effect size do we need to detect?
# Cohen's d is the difference between two means divided by a standard deviation for the data.
# If the difference is 1 %-point and the sd is about 5, then d is 0.2.

# Power: By what probability do we want to detect it? 0.9?
# Significance level is 0.05.
# We need to detect if change is unequal to zero, i.e. both positive and negative.
# What is the smallest sample size that fullfills this?

# What is the empirical weighted sd under normal circumstances?
city_ids <- c(
  "8952",
  "1952",
  "955",
  "957",
  "953",
  "952",
  "959"
)

city_indices <-
  purrr::map(
    city_ids,
    ~ get_published_index(., 2022, 12)
  ) |>
  purrr::list_rbind()

city_indices_tidy <-
  city_indices |>
  dplyr::filter(
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    day_type == "ALL",
    period == "year_to_date"
  )

mean(city_indices_tidy$standard_deviation)
median(city_indices_tidy$standard_deviation)
# 4

cohen_d <- 0.2 / 4

# Choose wanted power and difference in mean
# TODO: finite population
# TODO: stratified sampling
power_analysis <-
  pwr::pwr.t.test(
    n = NULL,
    d = cohen_d,
    sig.level = 0.05,
    power = 0.8,
    type = "one.sample",
    alternative = "two.sided"
  )

power_analysis
plot(power_analysis)

# TODO: use FPC to reduce n