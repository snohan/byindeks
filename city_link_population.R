# Define traffic link populations for city areas
# Want to make one file per city that holds:
# - link id
# - geometry
# - function class
# - road category
# - road number
# - length
# - municipality_id
# - trp id
# - toll station id

# Steps in deciding population per city:
# 1. Find the city area definition in terms of municipalities, or more specific if applicable (City agreements/ØKV/SD).
# 2. Get the municipality polygons (NVDB) and merge these.
# 3. Narrow down city area by using urban area polygons (SSB/Geonorge).
# 4. Find the traffic links intersecting with this final area.

# Global necessities:
# A. All traffic links (downloadable in ADM)
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

readr::write_rds(urban_areas_bergen, "representativity/urban_area_bergen.rds")


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


# Trondheim ----
city_number <- "960"

## City area ----
# Nullvekstmålet gjelder innenfor kommunene Trondheim, Melhus, Skaun, Orkland, Malvik, Stjørdal.

# Trondheim 5001
# Melhus 5028
# Skaun 5029
# Orkland 5059
# Malvik 5031
# Stjørdal 5035

municipality_ids <- c(5001, 5028, 5029, 5059, 5031, 5035)

link_municipality_id_trondheim <-
  link_municipality_id |>
  dplyr::filter(
    municipality_id %in% municipality_ids
  ) |>
  dplyr::distinct()

municipality_polygon_trondheim <-
  purrr::map(
    municipality_ids,
    ~ hent_kommune(.x)
  ) |>
  dplyr::bind_rows() |>
  sf::st_union() |>
  sf::st_transform("wgs84")

plot(municipality_polygon_trondheim)

readr::write_rds(
  municipality_polygon_trondheim,
  "traffic_link_pop/municipality_polygon_trondheim.rds"
)


## Urban area ----
# TODO:
# Pick urban areas inside area polygon

# Filter urban areas by area polygon
urban_areas_trondheim <-
  urban_areas |>
  sf::st_filter(municipality_polygon_trondheim, .predicate = st_intersects) |>
  dplyr::filter(
    !(tettstednummer %in% c(6611, 6654)) # Løkken verk, Lundamo too remote?
  )

plot(urban_areas_trondheim$geometry)

# Simplifing the multipolygon to its convex hull in order to keep links between subareas.
# This might lead to the inclusion of some unwanted links, but supposedly these are fewer than those we would miss.
urban_areas_convex_hull <-
  urban_areas_trondheim |>
  sf::st_convex_hull() |>
  sf::st_union() |>
  sf::st_convex_hull()

plot(urban_areas_convex_hull)

readr::write_rds(urban_areas_trondheim, "representativity/urban_area_trondheim.rds")


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

link_toll_id_city <-
  link_toll_id |>
  dplyr::filter(
    toll_id %in% this_citys_trps_all_adt_final$trp_id
  ) |>
  dplyr::rename(
    trp_id = toll_id
  )

link_trp_id_city_all <-
  dplyr::bind_rows(
    link_trp_id_city,
    link_toll_id_city
  )

missing <-
  this_citys_trps_all_adt_final |>
  dplyr::filter(
    !(trp_id %in% link_trp_id_city_all$trp_id)
  )


## Link population ----
links_trondheim <-
  links_2024 |>
  dplyr::filter(
    link_id %in% link_municipality_id_trondheim$link_id
  ) |>
  sf::st_filter(urban_areas_convex_hull, .predicate = st_intersects) |>
  # # Add some links
  # dplyr::bind_rows(
  #   links_2024 |>
  #     dplyr::filter(
  #       link_id %in% c(
  #       )
  #     )
  # ) |>
  # Remove some links
  dplyr::filter(
    !(link_id %in% c(
      "0.0@1894431-0.58389315@578621",
      "0.14550818-0.79428077@578644",
      "0.53103253-0.85434986@72832",
      "0.0-0.31477798@72825",
      "0.03295556-0.2516549@72816",
      "0.0-0.6999057@72844",
      "0.32247438@72837-0.17890729@72326",
      "0.50173511-0.74177907@72322",
      "0.01461415@72322-0.49617953@72322",
      "0.02138781-1.0@72671",
      "0.0-0.5831847@72317",
      "0.01434102@3293702-0.51747797@3293701",
      "0.0@72167-1.0@3403124",
      "0.0@3403125-1.0@3293717",
      "0.56535722@3293701-0.97017035@72809",
      "0.0@3293687-1.0@72646",
      "0.41121651@72313-0.02582217@72836",
      "0.02582217-0.20243871@72836",
      "0.20243871-0.84854102@72836",
      "0.84854102@72836-0.38777253@72314",
      "0.38777253-0.43638662@72314",
      "0.43638662@72314-1.0@2487849",
      "0.0@42842-0.33210306@42346",
      "0.33210306@42346-0.54681195@42346",
      "0.54681195-0.83544095@42346",
      "0.0@3355204-1.0@3925056",
      "0.0@2599730-1.0@72680",
      "0.81432724@42835-0.38680226@42837",
      "0.42222879@72677-1.0@42840",
      "0.02760821-0.03295556@72816",
      "0.49617953-0.50173511@72322",
      "0.51747797-0.56535722@3293701",
      "0.0-1.0@41397",
      "0.0-1.0@42838",
      "0.20717289-0.81432724@42835",
      "0.67943695-1.0@72678",
      "0.08266532-0.42222879@72677",
      "0.0@2200826-0.6423011@2739613",
      "0.0-1.0@2739617",
      "0.6423011-1.0@2739613",
      "0.32162572-1.0@2739612",
      "0.0-0.32162572@2739612",
      "0.18437181-0.72729001@72696"
    )),
    function_class != "E"
  ) |>
  dplyr::left_join(
    link_trp_id_city_all,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rename(
    point_id = trp_id
  )

# Visual check
map_links_with_function_class(links_trondheim) |>
  addPolygons(
    data = municipality_polygon_trondheim,
    weight = 3,
    opacity = 0.3,
    fill = FALSE
  ) |>
  addPolygons(
    data = urban_areas_trondheim,
    weight = 3,
    opacity = 0.3,
    fill = FALSE
  )

readr::write_rds(
  links_trondheim,
  "traffic_link_pop/links_trondheim.rds"
)


# Nord-Jæren ----
city_number <- "952"

## City area ----
# Nullvekstmålet gjelder innenfor kommunene Stavanger, Sandnes, Sola og Randaberg.
# Agreement area does not include the former municipalities of Rennesøy, Finnøy and Forsand.
# Therefore, cannot use municipality ids directly, but geometric intersections with polygons.
# There are two historic municipalities, and two contemporary.

municipality_ids <- c(1103, 1108, 1124, 1127)

link_municipality_id_nj <-
  link_municipality_id |>
  dplyr::filter(
    municipality_id %in% municipality_ids
  ) |>
  dplyr::distinct()


municipality_polygon_nj <-
  dplyr::bind_rows(
    hent_historisk_kommune(1103, 1103),
    hent_historisk_kommune(1108, 1102),
    hent_kommune(1124),
    hent_kommune(1127)
  ) |>
  sf::st_union() |>
  sf::st_transform("wgs84")

plot(municipality_polygon_nj)

readr::write_rds(
  municipality_polygon_nj,
  "traffic_link_pop/municipality_polygon_nj.rds"
)


## Urban area ----
# TODO:
# Pick urban areas inside area polygon

# Filter urban areas by area polygon
urban_areas_nj <-
  urban_areas |>
  sf::st_filter(municipality_polygon_nj, .predicate = st_intersects) |>
  dplyr::filter(
    tettstednummer == 4522
  )

plot(urban_areas_nj$geometry)

# Simplifing the multipolygon to its convex hull in order to keep links between subareas.
# This might lead to the inclusion of some unwanted links, but supposedly these are fewer than those we would miss.
urban_areas_convex_hull <-
  urban_areas_nj |>
  sf::st_convex_hull() |>
  sf::st_union() |>
  sf::st_convex_hull()

plot(urban_areas_convex_hull)

readr::write_rds(urban_areas_nj, "representativity/urban_area_nj.rds")


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


## Link population ----
links_nj <-
  links_2024 |>
  dplyr::filter(
    link_id %in% link_municipality_id_nj$link_id
  ) |>
  sf::st_filter(urban_areas_convex_hull, .predicate = st_intersects) |>
  # # Add some links
  # dplyr::bind_rows(
  #   links_2024 |>
  #     dplyr::filter(
  #       link_id %in% c(
  #       )
  #     )
  # ) |>
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
    link_trp_id_city,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rename(
    point_id = trp_id
  )

# Visual check
map_links_with_function_class(links_nj) |>
  addPolygons(
    data = municipality_polygon_nj,
    weight = 3,
    opacity = 0.3,
    fill = FALSE
  ) |>
  addPolygons(
    data = urban_areas_nj,
    weight = 3,
    opacity = 0.3,
    fill = FALSE
  )

readr::write_rds(
  links_nj,
  "traffic_link_pop/links_nj.rds"
)


# Oslo ----
city_number <- "959"

## City area ----
# Nullvekstmålet gjelder Oslo kommune og Akershus slik fylket var avgrenset før 1. januar 2020.

municipality_ids <- c(301)

link_municipality_id_osl <-
  link_municipality_id |>
  dplyr::filter(
    municipality_id %in% municipality_ids |
    stringr::str_detect(municipality_id, "^32")
  ) |>
  dplyr::distinct()

municipality_polygon_osl <-
  dplyr::bind_rows(
    hent_fylke(3),
    hent_fylker_historiske() |>
      dplyr::filter(
        nr == "2"
      )
  ) |>
  sf::st_union() |>
  sf::st_transform("wgs84") |>
  sf::st_simplify(dTolerance = 1000)

plot(municipality_polygon_osl)

readr::write_rds(
  municipality_polygon_osl,
  "traffic_link_pop/municipality_polygon_oslo.rds"
)

## Urban area ----
urban_areas_oslo <-
  urban_areas |>
  sf::st_filter(municipality_polygon_osl, .predicate = st_intersects) |>
  sf::st_simplify(dTolerance = 100) |>
  dplyr::filter(
    !(tettstednummer %in% c("0031")) # Moss
  )

plot(urban_areas_oslo$geometry)

# urban_areas_convex_hull <-
#   urban_areas_oslo |>
#   sf::st_union() |>
#   sf::st_convex_hull()
#
# plot(urban_areas_convex_hull)

urban_areas_concave <-
  urban_areas_oslo |>
  sf::st_union() |>
  sf::st_concave_hull(ratio = 0.4)

urban_areas_concave |>
  leaflet(options = leafletOptions(crs = nvdb_crs)) |>
  addTiles(
    urlTemplate = nvdb_map_url,
    attribution = nvdb_map_attribution
  ) |>
  addPolylines(
    opacity = 1,
    weight = 4,
    highlightOptions = highlightOptions(
      bringToFront = TRUE,
      sendToBack = FALSE,
      color = "purple",
      opacity = 0.6
    )
  ) |>
  addPolylines(
    data = urban_areas_oslo,
    opacity = 1,
    weight = 8,
    color = "red",
    highlightOptions = highlightOptions(
      bringToFront = TRUE,
      sendToBack = FALSE,
      color = "purple",
      opacity = 0.6
    )
  ) |>
  addPolylines(
    data = municipality_polygon_osl,
    opacity = 1,
    weight = 8,
    color = "green",
    highlightOptions = highlightOptions(
      bringToFront = TRUE,
      sendToBack = FALSE,
      color = "purple",
      opacity = 0.6
    )
  )

readr::write_rds(urban_areas_oslo, "representativity/urban_area_oslo.rds")


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
links_oslo <-
  links_2024 |>
  dplyr::filter(
    link_id %in% link_municipality_id_osl$link_id
  ) |>
  sf::st_filter(urban_areas_concave, .predicate = st_intersects) |>
  # Add some links
  dplyr::bind_rows(
    links_2024 |>
      dplyr::filter(
        link_id %in% c(
          "0.40035126@443722-0.9488912@443721",
          "1.0-0.40035126@443722",
          "0.0@443499-1.0@1205912",
          "0.0@1205913-1.0@443499",
          "0.0-1.0@443500",
          "0.0-1.0@443160",
          "0.0-1.0@443161",
          "0.44850952-0.46071237@443149",
          "0.46071237-0.55280806@443149",
          "0.0@443501-1.0@1992885",
          "0.0@1992890-0.49517412@443501",
          "0.85064777@971600-1.0@1992888",
          "0.67427556@443733-1.0@443734",
          "0.0@971601-0.67427556@443733",
          "0.0-1.0@2682281",
          "0.89978332@444251-0.25089956@444252",
          "0.25089956-0.27690461@444252"
        )
      )
  ) |>
  # Remove some links
  dplyr::filter(
    !(link_id %in% c(
      "0.0@444315-1.0@409685",
      "0.0-0.60711864@443723",
      "0.0-0.98977308@443726",
      "0.67339081@443723-0.20561992@443752",
      "0.0-1.0@443764",
      "0.7899178-1.0@443756",
      "0.0@443736-1.0@443755",
      "0.0@1748035-0.88675969@1748078",
      "0.65317784-0.78055188@443756",
      "0.52143138-0.48884684@443293",
      "0.78055188-0.7899178@443756",
      "0.15425706@971574-0.3911717@443743",
      "0.3911717-0.0@443743",
      "0.37160563-0.40033598@444251",
      "0.93947297@971549-0.37160563@444251",
      "0.0-1.0@443962",
      "0.0-1.0@443963",
      "0.36461762-1.0@443782",
      "1.0@180902-0.0@443429",
      "0.987741@971417-1.0@3228193",
      "0.0-1.0@443911",
      "0.2524736-0.59761294@444250",
      "0.0-0.51440309@443912",
      "0.20573676@443671-1.0@443672",
      "0.172767@443919-1.0@443920",
      "0.0-0.4857445@444355",
      "0.0-1.0@444356",
      "0.12061301-0.65976112@444303",
      "0.0-0.5665972@443378",
      "0.0-0.38874923@444090",
      "0.72753925-1.0@444098",
      "0.0-1.0@444099",
      "0.0-0.72753925@444098",
      "0.10814654-0.64287309@444313",
      "0.0991457-0.87074757@444306",
      "0.0@1897418-0.89647393@2037750",
      "0.0-1.0@444124",
      "0.0-1.0@2037771",
      "0.3261947-0.67776283@2037788",
      "0.0@2038004-0.37150721@443706",
      "0.15413866@443479-0.25462557@443480",
      "0.0-0.38512095@444134",
      "0.0-1.0@443438",
      "0.19164779-1.0@444136",
      "0.78348151@444241-0.59048337@1060044",
      "0.0-1.0@443928",
      "0.0-1.0@443924",
      "0.12158789-0.172767@443919",
      "0.78597885-1.0@443923",
      "0.0-0.78597885@443923",
      "0.38251546-0.79223076@443925",
      "0.75213077@444129-0.69568483@444130",
      "0.3385874-0.99074851@604781",
      "0.72859351-0.97808765@605550",
      "0.66912707-0.72859351@605550",
      "1.0@3344853-0.01680531@1773691",
      "0.35511204@181021-1.0@181022",
      "0.16410394-0.35511204@181021",
      "0.14362553-1.0@444345",
      "0.69568483-1.0@444130"
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
map_links_with_function_class(links_oslo) |>
  addPolygons(
    data = municipality_polygon_osl,
    weight = 3,
    opacity = 0.3,
    fill = FALSE
  ) |>
  addPolygons(
    data = urban_areas_oslo,
    weight = 3,
    opacity = 0.3,
    fill = FALSE
  )

readr::write_rds(
  links_oslo,
  "traffic_link_pop/links_oslo.rds"
)
