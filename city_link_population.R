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
  source("get_from_nvdb_api.R")
  source("H:/Programmering/R/byindeks/traffic_link_functions.R")

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

# Nullvekstmålet gjelder innenfor de gamle kommunene Bergen, Askøy, Lindås, Os og Fjell.

# Kommune | ny id | gammel id | gammelt navn
# Bergen 4601 UENDRET
# Alver 4631 1263 Lindås
# Øygarden 4626 1246 Fjell
# Askøy 4627 UENDRET
# Bjørnafjorden 4624 1243 Os

test <- hent_historisk_kommune(4631, 1263)

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


# TODO:
# Pick urban areas inside area polygon

# Filter urban areas by area polygon
urban_areas_bergen <-
  urban_areas |>
  sf::st_filter(municipality_polygon_bergen, .predicate = st_intersects)

plot(urban_areas_bergen$geometry)

# Simplifing the multipolygon to its convex hull in order to keep links between subareas.
# This might lead to the inclusion of some unwanted links, but supposedly these are fewer than those we would miss.
urban_areas_convex_hull <-
  urban_areas_bergen |>
  sf::st_convex_hull()

plot(urban_areas_convex_hull$geometry)

# HERE!!!!


readr::write_rds(urban_areas_nj, "representativity/urban_area_nj.rds")



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