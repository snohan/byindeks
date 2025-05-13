# Define traffic link populations for city areas
# Want to make one file per city that holds
# Link id, geometry, function class, road category, road number, length, municipality_id, trp id, toll station id

# Steps in deciding population per city:
# 1. Find the city area definition in terms of municipalities, or more specific if applicable (City agreements/ØKV/SD).
# 2. Get the municipality polygons (NVDB) and merge these.
# 3. Narrow down city area by using urban area polygons (SSB/Geonorge).
# 4. Find the traffic links intersecting with this final area.

# Global necessities:
# A. All traffic links (ADM)
# B. All urban area polygons (SSB/Geonorge)

# Local necessities:
# C. City area definition in terms of municipalities, and more specific if applicable (City agreements/ØKV/SD)
# D. Municipality polygons (NVDB)



# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  library(tidyverse)
  library(sf)
  library(leaflet)
  source("get_from_trafficdata_api.R")
  source("get_from_nvdb_api.R")
  source("H:/Programmering/R/byindeks/traffic_link_functions.R")
  source("H:/Programmering/R/byindeks/leaflet_nvdb_map_setup.R")

  # Need to filter links by geometry that does not intersect
  not_intersected <- function(x, y) !sf::st_intersects(x, y)
}


# A. Traffic links ----
# From ADM, see script traffic_link_prep.R
links_2024 <- readr::read_rds("traffic_link_pop/link_traffic_2024.rds")

# Need also info on trp id, toll station id, municipality id per link id
link_municipality_id <- readr::read_rds("traffic_link_pop/link_municipality_id.rds")
link_toll_id <- readr::read_rds("traffic_link_pop/link_toll_id.rds")
link_trp_id <- readr::read_rds("traffic_link_pop/link_trp_id.rds")


# B. Urban area polygons ----
# Downloaded fgdb file from Geonorge: tettsteder, UTM33

#urban_layers <- sf::st_layers("C:/Users/snohan/Desktop/tettsteder_2024.gdb")
urban_areas <-
  sf::st_read(
    "C:/Users/snohan/Desktop/tettsteder_2024.gdb",
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
  ) |>
  sf::st_transform("wgs84")


# Bergen ----
city_number <- "8952"

## City area ----
# Nullvekstmålet gjelder innenfor de gamle kommunene Bergen, Askøy, Lindås, Os og Fjell.

# Kommune | ny id | gammel id | gammelt navn
# Bergen 4601 UENDRET
# Alver 4631 1263 Lindås
# Øygarden 4626 1246 Fjell
# Askøy 4627 UENDRET
# Bjørnafjorden 4624 1243 Os

municipality_ids <- c(4601, 4631, 4626, 4627, 4624)

link_municipality_id_bergen <-
  link_municipality_id |>
  dplyr::filter(
    municipality_id %in% municipality_ids
  ) |>
  dplyr::distinct()

municipality_polygon_bergen <-
  dplyr::bind_rows(
    #hent_kommune(4601),
    hent_historisk_kommune(4601, 1201), # Seemingly some minor adjustments have been made
    hent_kommune(4627),
    hent_historisk_kommune(4631, 1263),
    hent_historisk_kommune(4626, 1246),
    hent_historisk_kommune(4624, 1243)
  ) |>
  sf::st_union() |>
  sf::st_transform("wgs84")

plot(municipality_polygon_bergen)

readr::write_rds(
  municipality_polygon_bergen,
  "traffic_link_pop/municipality_polygon_bergen.rds"
)

# To filter links by area
# 1. Can use municipality ids directly, but this doesn't work in cases where area is based on old municipalities
# 2. Filter by area polygon based on old or new municipalities
# 3. Further filtering by urban area polygons

# Better efficiency in geometry matching if the urban area polygons have been filtered beforehand by looking up urban ids in
# Geonorge map


## Urban area ----
# TODO:
# Pick urban areas inside area polygon

# Filter urban areas by area polygon
urban_areas_bergen <-
  urban_areas |>
  sf::st_filter(municipality_polygon_bergen, .predicate = st_intersects) |>
  dplyr::filter(
    !(tettstednummer %in% c(5182, 5241)) # Hammarsland just about touches the polygon, Lindås too remote?
  )

plot(urban_areas_bergen$geometry)

# Simplifing the multipolygon to its convex hull in order to keep links between subareas.
# This might lead to the inclusion of some unwanted links, but supposedly these are fewer than those we would miss.
urban_areas_convex_hull <-
  urban_areas_bergen |>
  sf::st_convex_hull()

plot(urban_areas_convex_hull$geometry)

readr::write_rds(urban_areas_convex_hull, "representativity/urban_area_bergen.rds")


## City TRPs ----
this_citys_trps_all_adt_final <-
  readr::read_rds(
    file = paste0(
      "index_trp_metadata/trp_",
      city_number,
      ".rds"
    )
  )

link_trp_id_city <-
  link_trp_id |>
  dplyr::filter(
    trp_id %in% this_citys_trps_all_adt_final$trp_id
  )

missing <-
  this_citys_trps_all_adt_final |>
  dplyr::filter(
    !(trp_id %in% link_trp_id_city$trp_id)
  )


## Link population ----
links_bergen <-
  links_2024 |>
  dplyr::filter(
    link_id %in% link_municipality_id_bergen$link_id
  ) |>
  sf::st_filter(urban_areas_convex_hull$geometry, .predicate = st_intersects) |>
  # Add some links
  dplyr::bind_rows(
    links_2024 |>
      dplyr::filter(
        link_id %in% c(
          "0.6051633@2799781-0.62504593@805284",
          "0.0@2799790-0.6051633@2799781",
          "0.0-1.0@3144562",
          "0.0-1.0@3144564",
          "0.0-1.0@3144533",
          "0.0-1.0@3144563",
          "0.0-1.0@3144534",
          "0.46122322-0.87083099@3144561",
          "0.07223305-0.5311563@3144515",
          "0.82103689-0.95636983@805614",
          "0.88227822@2684724-0.61423683@2684728",
          "0.61423683@2684728-0.0@1684324",
          "1.0-0.0@1684319",
          "1.0-0.0@1684318",
          "1.0-0.0@1684317",
          "0.7132308-1.0@805086",
          "0.37001859-0.56645889@805723",
          "0.0@1669839-0.37001859@805723",
          "0.16618949@1669845-1.0@1669842",
          "0.58043969@3447966-1.0@805406",
          "0.38197472-1.0@3448017",
          "0.0-0.61742596@3447967",
          "0.61742596-1.0@3447967",
          "0.0-0.38197472@3448017",
          "0.10549267-0.24149404@805709",
          "0.0-1.0@3441693",
          "0.0-1.0@805018"
        )
      )
  ) |>
  # Remove some links
  dplyr::filter(
    !(link_id %in% c(
      "0.0-0.58710252@805329",
      "0.58710252-1.0@805329",
      "0.0-1.0@769592"
    )),
    function_class != "E"
  ) |>
  dplyr::left_join(
    link_trp_id_city,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rename(
    point_id = trp_id
  ) #|>
  # dplyr::mutate(
  #   city_trp =
  #     dplyr::case_when(
  #       point_id %in% city_trp_info$p_id ~ TRUE,
  #       TRUE ~ FALSE
  #     )
  # )

# Visual check
map_links_with_function_class(links_bergen) |>
  addPolygons(
    data = municipality_polygon_bergen,
    weight = 3,
    opacity = 0.3,
    fill = FALSE
  ) |>
  addPolygons(
    data = urban_areas_bergen,
    weight = 3,
    opacity = 0.3,
    fill = FALSE
  )

readr::write_rds(
  links_bergen,
  "traffic_link_pop/links_bergen.rds"
)
