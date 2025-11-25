# Purposes:

# A. Define traffic link population objectively.
  # An algorithm for narrowing down the link population:
  # 1. Merge the municipality polygons.
  # 2. Shave off parts that are explicitly stated as outside area of scope. Call this the agreement area.
  # 3. Get all the urban area polygons lying inside the agreement area with more than 1 000 inhabitants.
  # 4. Add areas with many ansatte and besøkende.
  # 5. Call this area the city area.
  # 6. Include all traffic links lying inside or overlaps with the city area.
  # 7. Exclude traffic with high ratio of through traffic.
  # 8. If necessary, add some traffic links to make the resulting road network connected between subareas.

# B. Is the selection of city index points representative for the area?
  # Calculate distance metrics for the selection of traffic links:
  # 1. Number of links in selection (i.e. that has a TRP).
  # 2. The ratio of selected links to the population size.
  # 3. The ratio of the traffic work on the selected links to the population traffic work.
  # 4. The total variation distance (TVD) between the relative distributions of traffic work for selection and population.
  # 5. The Hellinger distance between the relative distributions of traffic work for selection and population.
  # 6. Mean distance between links in the selection, given that the population is a connected graph with one component.

  # Good representativity would be given by:
  # 1. A smallest number of links in the selection
  #    such that the expected uncertainty under typical conditions will be as low as desired.
  #    This can be calculated per area, with a given population definition.
  # 2. High ratio of selected links.
  # 3. High ratio of selected traffic work.
  # 4. Low TVD.
  # 5. Low Hellinger distance.
  # 6. Low mean distance between selected links.
  # There is no general limit to any of these, except the first. All measures can be compared between selections.


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
  source("get_from_nvdb_api.R")
  source("H:/Programmering/R/byindeks/traffic_link_functions.R")
  source("H:/Programmering/R/byindeks/leaflet_nvdb_map_setup.R")
}

# Need to keep only continuous TRPs in the following analyses
trp_continuous <-
  get_points() |>
  dplyr::filter(
    registration_frequency == "CONTINUOUS"
  ) |>
  dplyr::select(
    trp_id
  ) |>
  dplyr::distinct()

# Need to know what the sample is supposed to be (if all TRPs give good data)
# I.e. the TRPs offically defined as the selection
{
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
}

# Need to filter links by geometry that does not intersect
not_intersected <- function(x, y) !sf::st_intersects(x, y)


# A. Population definition ----
## Urban areas ----
# Downloaded fgdb file from Geonorge: tettsteder, UTM33

#urban_layers <- sf::st_layers("C:/Users/snohan/Desktop/tettsteder_2024.gdb")

urban_areas <-
  sf::st_read(
    "H:/Tettsteder/tettsteder_2024.gdb",
    as_tibble = TRUE,
    #layer = "tettsted",
    query =
      "SELECT
      tettstednummer, tettstednavn, totalbefolkning, SHAPE
      FROM \"tettsted\"
      WHERE totalbefolkning > 1000
      "
  ) |>
  dplyr::group_by(tettstednummer, tettstednavn, totalbefolkning) |>
  dplyr::summarise(
    geometry = sf::st_union(SHAPE),
    .groups = "drop"
  )

#slice(urban_areas, 1:2) |> plot()


## Traffic links ----
# From traffic_link_prep.R
links <- readr::read_rds("traffic_link_pop/links_raw.rds")

# Need AADT and traffic work per link
traffic_volumes <-
  readr::read_rds("traffic_link_pop/link_traffic_volumes.rds") |>
  dplyr::filter(
    trafficVolumeType == "GUESSTIMATED",
    year == 2023
  ) |>
  dplyr::select(
    link_id,
    aadt = trafficVolumeValue,
    traffic_work_km = trafficWorkValue
  )

# Tidy links
links_with_traffic_work <-
  links |>
  dplyr::left_join(
    traffic_volumes,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::select(
    link_id,
    from, to,
    road_system_references,
    function_class,
    aadt,
    traffic_work_km
  ) |>
  dplyr::mutate(
    function_class =
      dplyr::case_when(
        link_id %in% c(
          "0.02123723-0.84417966@283442",
          "0.84417966-1.0@283442",
          "0.0-1.0@3522080",
          "0.57243185@319730-1.0@3522081"
        ) ~ "B",
        link_id %in% c(
          "0.33524051-1.0@3607094",
          "0.0-0.33524051@3607094",
          "0.0-1.0@3444182"
        ) ~ "C",
        TRUE ~ function_class
      )
  ) |>
  dplyr::filter(
    #!is.na(traffic_work_km),
    !is.na(function_class)
  )

# The tidy links above does not have columns for TRPs or municipalities.
# This is because some links will have more than one TRP (e.g. one per direction).
# When defining the population, it should have unique links.
# The selection may well have more than one TRP per link (one TRP per direction), but it must still be a unique set of links.
# A link may have multiple values in TPRs and municipalities.
# Thus, make filters based on these two lists accordingly.

link_trp_id <-
  readr::read_rds("traffic_link_pop/link_trp_id.rds") |>
  dplyr::filter(
    trp_id %in% trp_continuous$trp_id
  )

link_municipality_id <-
  readr::read_rds("traffic_link_pop/link_municipality_id.rds")


## Nord-Jæren ----
# Agreement area does not include the former municipalities of Rennesøy, Finnøy and Forsand.
# Therefore, cannot use municipality ids directly, but geometric intersections with polygons.
# There are two historic municipalities, and two contemporary.

municipality_ids_nj_today <- c(1127, 1124)

gamle_sandnes <- hent_historisk_kommune(1108, 1102)
gamle_stavanger <- hent_historisk_kommune(1103, 1103)

nj_municipality_today <-
  purrr::map(
    municipality_ids_nj_today,
    ~ hent_kommune(.)
  ) |>
  purrr::list_rbind() |>
  sf::st_as_sf()

nj_municipality_polygon <-
  dplyr::bind_rows(
    nj_municipality_today,
    gamle_sandnes,
    gamle_stavanger
  ) |>
  sf::st_union() |>
  sf::st_transform("wgs84")

plot(nj_municipality_polygon)

readr::write_rds(nj_municipality_polygon, "representativity/agreement_area_nj.rds")

urban_areas_nj <-
  urban_areas |>
  # Pick areas manually by looking for them at Geonorge map
  dplyr::filter(
    tettstednummer == 4522
  ) |>
  sf::st_transform("wgs84")

# Simplifing the multipolygon to its convex hull in order to keep links between subareas.
# This might lead to the inclusion of some unwanted links, but supposedly these are fewer than those we would miss.
urban_areas_nj_convex_hull <-
  urban_areas_nj |>
  sf::st_convex_hull()

plot(urban_areas_nj)
plot(urban_areas_nj_convex_hull)

readr::write_rds(urban_areas_nj, "representativity/urban_area_nj.rds")


# Need two alternative population definitions to compare
# 1. All links in agreement area: "whole"
# 2. Just the central links in city area: "central"

# 1. Agreement area
# Want to use polygon to filter links
# As geometry comparisons are heavy, limit links first

link_ids_in_municipalities <-
  link_municipality_id |>
  dplyr::filter(
    municipality_id %in% c(1103, 1108, 1124, 1127)
  )

links_nj_whole <-
  links_with_traffic_work |>
  dplyr::filter(
    link_id %in% link_ids_in_municipalities$link_id
  ) |>
  # Filter by polygon, using "covered_by" to not include those who cross the border
  sf::st_filter(nj_municipality_polygon, .predicate = st_covered_by) |>
  dplyr::left_join(
    link_trp_id,
    by = dplyr::join_by(link_id),
    # Gives duplicates unless we specify to keep only one,
    # and it doesn't matter which one here, hence "any":
    multiple = "any"
  ) |>
  dplyr::rename(
    point_id = trp_id
  ) |>
  dplyr::mutate(
    city_trp =
      dplyr::case_when(
        point_id %in% city_trp_info$p_id ~ TRUE,
        TRUE ~ FALSE
      )
  ) |>
  dplyr::filter(
    !(link_id %in% c(
      # Removing links on Åmøy
      "0.73662178-0.97497915@320696",
      "1.0-0.0@285751",
      "0.97497915-0.97767398@320696"
      )
    )
  )

links_nj_whole |>
  dplyr::select(
    function_class
  ) |>
  plot()

# Save file for use in report
readr::write_rds(links_nj_whole, "representativity/link_population_nj_whole.rds")

# 2. City area
# Need to add links in fv. 510 out to Rege to keep a TRP in the population (it is one of the 24)
# Rege could be removed from future version of index


links_nj_city <-
  links_with_traffic_work |>
  dplyr::filter(
    link_id %in% link_ids_in_municipalities$link_id
  ) |>
  sf::st_filter(urban_areas_nj_convex_hull$geometry, .predicate = st_intersects) |>
  # Add som links
  #dplyr::bind_rows(
  #
  #) |>
  # Remove some links
  dplyr::filter(
    !(link_id %in% c(
      # Ryfylketunnelen
      "0.0-1.0@2725983", "0.0-1.0@2725982",
      # Byfjordtunnelen
      "0.41798688@319527-0.56950694@320583",
      # Links in south-west
      "0.75290902@320683-1.0@2829293",
      "0.22210744-1.0@320180",
      "0.60727361-0.75290902@320683",
      "0.68926696-1.0@320127",
      "0.0-0.3298438@320128",
      "0.3298438-1.0@320128",
      "0.58471846@320670-0.75831918@320670"
    )),
    function_class != "E"
  ) |>
  dplyr::left_join(
    link_trp_id,
    by = dplyr::join_by(link_id),
    # Gives duplicates unless we specify to keep only one,
    # and it doesn't matter which one here, hence "any":
    multiple = "any"
  ) |>
  dplyr::rename(
    point_id = trp_id
  ) |>
  dplyr::mutate(
    city_trp =
      dplyr::case_when(
        point_id %in% city_trp_info$p_id ~ TRUE,
        TRUE ~ FALSE
      )
  )

plot(links_nj_city)

# 2. City area more manually
# nj_polygon_north <-
#   tibble::tibble(
#     lon = c(
#       5.7866566,
#       6.1565635,
#       5.8272271,
#       5.4658784
#     ),
#     lat = c(
#       59.0119612,
#       59.284531,
#       59.3473959,
#       59.0678125
#     )
#   ) |>
#   tibble::rowid_to_column("id") |>
#   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
#   dplyr::summarise(geometry = sf::st_combine(geometry)) |>
#   sf::st_cast("POLYGON")
#
# links_nj_central_1 <-
#   links |>
#   tidyr::unnest_longer(
#     municipalityIds,
#     values_to = "municipality_id"
#   ) |>
#   dplyr::filter(
#     municipality_id %in% municipality_ids_nj,
#     function_class %in% c("A", "B", "C", "D")
#   ) |>
#   dplyr::select(
#     -municipality_id,
#     -trafficVolumes
#   ) |>
#   # remove duplicate links (crossing municipality boundaries)
#   dplyr::distinct() |>
#   # remove links crossing borders out of area
#   dplyr::left_join(
#     all_municipality_ids,
#     by = dplyr::join_by(link_id)
#   ) |>
#   dplyr::rowwise() |>
#   dplyr:::mutate(
#     not_border_crossing =
#       purrr::map_lgl(
#         unlist(all_municipality_ids),
#         ~ all(. %in% municipality_ids_nj)
#       ) |> all()
#   ) |>
#   dplyr::ungroup() |>
#   dplyr::filter(
#     not_border_crossing == TRUE
#   ) |>
#   dplyr::left_join(
#     traffic_volumes,
#     by = join_by(link_id)
#   ) |>
#   dplyr::select(
#     -not_border_crossing,
#     -all_municipality_ids
#   ) |>
#   dplyr::filter(
#     !is.na(traffic_work_km)
#   ) |>
#   sf::st_as_sf() |>
#   # areas in northern part are former municipalities not to be included
#   sf::st_filter(nj_polygon_north, .predicate = not_intersected)
#
# # Need to remove links (with function class D?) in eastern part
# nj_polygon_east <-
#   tibble::tibble(
#     lon = c(
#       5.7653664,
#       5.8236117,
#       6.4870498,
#       6.0794556
#     ),
#     lat = c(
#       58.9062453,
#       58.815903,
#       58.8686332,
#       59.0519824
#     )
#   ) |>
#   tibble::rowid_to_column("id") |>
#   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
#   dplyr::summarise(geometry = sf::st_combine(geometry)) |>
#   sf::st_cast("POLYGON")
#
# links_nj_east_D <-
#   links_nj_central_1 |>
#   sf::st_filter(nj_polygon_east, .predicate = st_intersects) #|>
#   #dplyr::filter(
#   #  function_class == "D"
#   #)
#
# links_nj_central <-
#   links_nj_central_1 |>
#   dplyr::filter(
#     !(link_id %in% links_nj_east_D$link_id),
#     # Removing some links in Figgjo
#     !(link_id == "0.0-1.0@320083"),
#     !(link_id == "0.80529774-0.9731575@320669"),
#     !(link_id == "0.72327267-0.80529774@320669"),
#     !(link_id == "0.9731575-1.0@320669")
#   ) |>
#   dplyr::mutate(
#     city_trp =
#       dplyr::case_when(
#         point_id %in% city_trp_info$p_id ~ TRUE,
#         TRUE ~ FALSE
#       )
#   )

# Look at missing data
# links_nj_missing <-
#   links_nj |>
#   dplyr::filter(
#     is.na(traffic_work)
#   )

# Map
#link_population |> map_links_with_function_class()

# Save file for use in report
readr::write_rds(links_nj_city, "representativity/link_population_nj.rds")


### Mean distance to chosen nodes ----
# Need only the line graph (no need for the traffic nodes explicitly)
city_line_graph <- create_line_graph(links_nj_city)
mean_dist <- calculate_mean_distance_to_city_index_points(city_line_graph)

agreement_line_graph <- create_line_graph(links_nj_whole)
mean_dist_agreement <- calculate_mean_distance_to_city_index_points(agreement_line_graph)


# B. Representativity measures ----
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
  ) |>
  dplyr::mutate(
    last_month_in_index = lubridate::as_date(last_month_in_index)
  )

# Calculate distance metrics for each index month for the city
# Build a tibble with columns:
# index_month
# n_trp
# ratio_trp
# ratio_traffic_work
# tvd
# hellinger
# mean distance

# 1. Filter the city
city_trp_rolling_index_nj <-
  city_trp_rolling_index |>
  dplyr::filter(
    city_id == 952
  )

monthly_sample <- function(link_df, month_index_trp_list, month_string) {

  this_month <-
    month_index_trp_list |>
    dplyr::filter(
      last_month_in_index == month_string
    )

  monthly_sample_df <-
    link_df |>
    sf::st_drop_geometry() |>
    dplyr::select(
      link_id,
      from, to,
      function_class,
      point_id,
      traffic_work_km,
      city_trp
    ) |>
    # dplyr::mutate(
    #   city_trp =
    #     dplyr::case_when(
    #       point_id %in% this_month$trp_id ~ TRUE,
    #       TRUE ~ FALSE
    #     )
    # ) |>
    dplyr::mutate(
      point_id =
        dplyr::case_when(
          point_id %in% this_month$trp_id ~ point_id,
          TRUE ~ NA_character_
        )
    )

  total_tw <- sum(link_df$traffic_work_km)

  monthly_sample_tw <-
    monthly_sample_df |>
    dplyr::filter(
      !is.na(point_id)
    )

  percentage_tw <- 100* (sum(monthly_sample_tw$traffic_work_km) / total_tw)

  statistical_distances_df <-
    monthly_sample_df |>
    summarise_link_population_by_function_class() |>
    calculate_statistical_distance() |>
    dplyr::mutate(
      percentage_tw = percentage_tw
    )

  return(statistical_distances_df)

}

# TODO: add calculation of mean distance to points here
test_1 <- monthly_sample(links_nj_central, city_trp_rolling_index_nj, "2020-12-01")


links_with_monthly_sample <- function(link_df, month_index_trp_list) {

  n_links <- nrow(link_df)

  n_trp_per_index <-
    month_index_trp_list |>
    dplyr::summarise(
      n_trp = n(),
      .by = last_month_in_index
    ) |>
    dplyr::mutate(
      percentage_n_trp = 100 * (n_trp / n_links)
    )

  # 3. For each index month, the links_city_area$city_trp column must be updated

  # Calculate distance metrics
  # For each row,
  #purrr::pmap_df(~ monthly_sample(link_df, month_index_trp_list, last_month_in_index))

 # unnest?

}


test <- links_with_monthly_sample(links_nj_central, city_trp_rolling_index_nj)

## Smallest selection  ----
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