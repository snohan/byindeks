# Evaluate an area for index eligibility.

# Setup ----
{
  source("rmd_setup.R")
  # For checking quality of traffic registrations:
  source("get_from_trafficdata_api.R")
  # Municipality polygons, toll station meta data (not for now):
  #source("get_from_nvdb_api.R")
  source("traffic_link_functions.R")
  library(sf)
}

# Use municipalities first, then reduce area as needed "by hand" or urban polygons.

find_eligible_trp_selection <- function() {

  population_trp <-
    city_link_population_raw |>
    sf::st_drop_geometry() |>
    dplyr::select(
      link_id, point_id
    ) |>
    dplyr::filter(
      !is.na(point_id)
    )

  # length quality
  adt_lmv <-
    get_aadt_by_length_for_trp_list(population_trp$point_id) |>
    dplyr::filter(
      length_range == "[..,5.6)",
      year %in% c(2023, 2024)
    ) |>
    dplyr::select(
      trp_id,
      year,
      aadt_lmv = aadt_length_range,
      aadt_valid_length,
      aadt_total,
      coverage
    )

  eligible_selection <-
    population_trp |>
    dplyr::left_join(
      adt_lmv,
      by = join_by(point_id == trp_id)
    ) |>
    dplyr::mutate(
      length_quality = (aadt_valid_length / aadt_total) |> round(2),
      enough_data = coverage > 75 & length_quality > 0.975
    ) |>
    dplyr::filter(
      enough_data
    ) |>
    dplyr::left_join(
      trp_meta_data,
      by = join_by(point_id == trp_id)
    ) |>
    dplyr::select(
      link_id,
      point_id,
      #coverage,
      #length_quality,
      #enough_data,
      name,
      road_reference,
      year,
      aadt_lmv
    ) |>
    dplyr::mutate(
      aadt_lmv = round(aadt_lmv, -1)
    ) |>
    tidyr::pivot_wider(
      names_from = year,
      names_prefix = "aadt_",
      values_from = aadt_lmv
    )

  return(eligible_selection)

}

# Prerequisites ----
## TRP meta data ----
# From city_index_check_dataprep.R
#source("city_index_check_dataprep.R")
trp_meta_data <- readr::read_rds("trps_for_city_index.rds")


## Traffic links ----
# From traffic_link_prep.R
links <- readr::read_rds("traffic_link_pop/links_raw.rds")

link_trp_id <-
  readr::read_rds("traffic_link_pop/link_trp_id.rds") |>
  dplyr::filter(
    trp_id %in% trp_meta_data$trp_id
  )

link_toll_id <- readr::read_rds("traffic_link_pop/link_toll_id.rds")

link_municipality_id <- readr::read_rds("traffic_link_pop/link_municipality_id.rds")

traffic_volumes <-
  readr::read_rds("traffic_link_pop/link_traffic_volumes.rds") |>
  dplyr::filter(
    trafficVolumeType == "GUESSTIMATED",
    trafficVolumeResolution == "ADT",
    year == 2023
  ) |>
  dplyr::select(
    link_id,
    aadt = trafficVolumeValue,
    traffic_work_km = trafficWorkValue
  )


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


# Bodø ----
municipality_ids <- 1804

urban_area_bodo <-
  urban_areas |>
  dplyr::filter(
    tettstednummer %in% c(7501, 7502, 7503)
  ) |>
  sf::st_transform("wgs84") |>
  dplyr::summarise(
    geometry = sf::st_union(geometry)
  ) |>
  sf::st_convex_hull()

city_link_population_raw <-
  get_link_population_inside_municipalities(municipality_ids) |>
  sf::st_filter(urban_area_bodo, .predicate = st_covered_by) |>
  dplyr::bind_rows(
    links |>
      dplyr::filter(
        # Need connection to Løpsmarka
        link_id == "0.0@3346578-0.46892094@886041"
      )
  ) |>
  dplyr::filter(
    function_class %in% c("A", "B", "C", "D")
  ) |>
  dplyr::left_join(
    link_trp_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    traffic_volumes,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    link_toll_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rename(
    point_id = trp_id
    # TODO: In case of only toll station on link and we want it as a city index point, move its ID to trp_id column.
    # 'point_id' should be a kind of merger of trp_id and toll_id
  )

eligible_selection <- find_eligible_trp_selection()

population_tidy <-
  city_link_population_raw |>
  dplyr::left_join(
    eligible_selection |>
      dplyr::select(-point_id),
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::mutate(
    label_text =
      dplyr::case_when(
        !is.na(name) ~ paste(name, "<br/>", road_reference),
        TRUE ~ NA_character_
      ),
    label_text = lapply(label_text, htmltools::HTML)
  )

readr::write_rds(
  population_tidy,
  "new_area_index/links_bdo_2024.rds"
)

# Ålesund ----
municipality_ids <- 1508

urban_area_aalesund <-
  urban_areas |>
  dplyr::filter(
    tettstednummer %in% c(6025)
  ) |>
  sf::st_transform("wgs84") |>
  dplyr::summarise(
    geometry = sf::st_union(geometry)
  ) |>
  sf::st_convex_hull()

city_link_population_raw <-
  get_link_population_inside_municipalities(municipality_ids) |>
  sf::st_filter(urban_area_aalesund, .predicate = st_covered_by) |>
  dplyr::filter(
    function_class %in% c("A", "B", "C", "D")
  ) |>
  dplyr::left_join(
    link_trp_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    traffic_volumes,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    link_toll_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rename(
    point_id = trp_id
    # TODO: In case of only toll station on link and we want it as a city index point, move its ID to trp_id column.
    # 'point_id' should be a kind of merger of trp_id and toll_id
  )

eligible_selection <- find_eligible_trp_selection()

population_tidy <-
  city_link_population_raw |>
  dplyr::left_join(
    eligible_selection |>
      dplyr::select(-point_id),
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::mutate(
    label_text =
      dplyr::case_when(
        !is.na(name) ~ paste(name, "<br/>", road_reference),
        TRUE ~ NA_character_
      ),
    label_text = lapply(label_text, htmltools::HTML)
  )

readr::write_rds(
  population_tidy,
  "new_area_index/links_aal_2024.rds"
)

# Haugesund ----
municipality_ids <- c(1106, 1149)

# Must reduce Haugesund are (just the mainland part of Karmøy)
haugesund_polygon <-
  tibble::tibble(
    lon = c(
      5.4023042,
      5.4225312,
      5.3385784,
      5.3236261,
      5.2969286,
      5.2926611,
      5.2236771,
      5.1980626
    ),
    lat = c(
      59.5391317,
      59.2708638,
      59.2651765,
      59.3304398,
      59.3666045,
      59.3846508,
      59.4258514,
      59.5270527
    )
  ) |>
  tibble::rowid_to_column("id") |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("POLYGON")

urban_area_haugesund <-
  urban_areas |>
  dplyr::filter(
    tettstednummer %in% c(4532)
  ) |>
  sf::st_transform("wgs84") |>
  dplyr::summarise(
    geometry = sf::st_union(geometry)
  ) |>
  sf::st_convex_hull()

city_link_population_raw <-
  get_link_population_inside_municipalities(municipality_ids) |>
  sf::st_filter(urban_area_haugesund, .predicate = st_covered_by) |>
  sf::st_filter(haugesund_polygon, .predicate = st_covered_by) |>
  dplyr::bind_rows(
    links |>
      dplyr::filter(
        link_id %in% c(
          "0.68137233-1.0@319814"
          #"0.67222403@319651-0.45538288@319651"
          )
      )
  ) |>
  dplyr::filter(
    function_class %in% c("A", "B", "C", "D")
  ) |>
  dplyr::left_join(
    link_trp_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    traffic_volumes,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    link_toll_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rename(
    point_id = trp_id
    # TODO: In case of only toll station on link and we want it as a city index point, move its ID to trp_id column.
    # 'point_id' should be a kind of merger of trp_id and toll_id
  )

eligible_selection <- find_eligible_trp_selection()

population_tidy <-
  city_link_population_raw |>
  dplyr::left_join(
    eligible_selection |>
      dplyr::select(-point_id),
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::mutate(
    label_text =
      dplyr::case_when(
        !is.na(name) ~ paste(name, "<br/>", road_reference),
        TRUE ~ NA_character_
      ),
    label_text = lapply(label_text, htmltools::HTML)
  )

readr::write_rds(
  population_tidy,
  "new_area_index/links_hau_2024.rds"
)


# Arendal/Grimstad ----
municipality_ids <- c(4203, 4202)

urban_area_arg <-
  urban_areas |>
  dplyr::filter(
    tettstednummer %in% c(3523, 3511)
  ) |>
  sf::st_transform("wgs84") |>
  dplyr::summarise(
    geometry = sf::st_union(geometry)
  ) |>
  sf::st_convex_hull()

city_link_population_raw <-
  get_link_population_inside_municipalities(municipality_ids) |>
  sf::st_filter(urban_area_arg, .predicate = st_covered_by) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    road_reference_1 = purrr::pluck(road_system_references, 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    # E18 has high percentage through traffic so it will not be part of population.
    !stringr::str_detect(road_reference_1, "EV18"),
  ) |>
  dplyr::select(-road_reference_1) |>
  dplyr::filter(
    function_class %in% c("A", "B", "C", "D")
  ) |>
  dplyr::left_join(
    link_trp_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    traffic_volumes,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    link_toll_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rename(
    point_id = trp_id
    # TODO: In case of only toll station on link and we want it as a city index point, move its ID to trp_id column.
    # 'point_id' should be a kind of merger of trp_id and toll_id
  )

eligible_selection <- find_eligible_trp_selection()

population_tidy <-
  city_link_population_raw |>
  dplyr::left_join(
    eligible_selection |>
      dplyr::select(-point_id),
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::mutate(
    label_text =
      dplyr::case_when(
        !is.na(name) ~ paste(name, "<br/>", road_reference),
        TRUE ~ NA_character_
      ),
    label_text = lapply(label_text, htmltools::HTML)
  )

readr::write_rds(
  population_tidy,
  "new_area_index/links_arg_2024.rds"
)


# Vestfoldbyen ----
municipality_ids <- c(3905, 3907, 3909, 3911)
# Must include Nøtterøy as some of urban area Tønsberg lies here

urban_area_vfl <-
  urban_areas |>
  dplyr::filter(
    tettstednummer %in% c(2551, 2541, 2531, 2521, 2623, 2631, 2621, 2633)
  ) |>
  sf::st_transform("wgs84") |>
  dplyr::summarise(
    geometry = sf::st_union(geometry)
  ) |>
  sf::st_convex_hull()

city_link_population_raw <-
  get_link_population_inside_municipalities(municipality_ids) |>
  sf::st_filter(urban_area_vfl, .predicate = st_covered_by) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    road_reference_1 = purrr::pluck(road_system_references, 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    # E18 has high percentage through traffic so it will not be part of population.
    !stringr::str_detect(road_reference_1, "EV18"),
  ) |>
  dplyr::select(-road_reference_1) |>
  dplyr::filter(
    function_class %in% c("A", "B", "C", "D"),
    # Need to remove some links on Nøtterøy
    !(link_id %in% c(
      "0.55369125-0.98721017@1175688",
      "0.0-0.55369125@1175688",
      "0.7648385@1175848-0.20376071@1175849",
      "0.73029605-0.7648385@1175848",
      "0.52293776-0.73029605@1175848",
      "0.0@2715360-0.52293776@1175848"
    ))
  ) |>
  dplyr::bind_rows(
    links |>
      dplyr::filter(
        link_id %in% c(
          # Need a connction by Torp
          "1.0@2258465-0.0@1175671",
          # fv. 312 Andebu
          "0.0-1.0@2252839",
          "0.0@2252845-0.35865579@1175854",
          # fv. 35 Revetal
          "0.0@2252722-0.41238247@1175824"
        )
      )
  ) |>
  dplyr::left_join(
    link_trp_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    traffic_volumes,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    link_toll_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rename(
    point_id = trp_id
    # TODO: In case of only toll station on link and we want it as a city index point, move its ID to trp_id column.
    # 'point_id' should be a kind of merger of trp_id and toll_id
  )

eligible_selection <- find_eligible_trp_selection()

population_tidy <-
  city_link_population_raw |>
  dplyr::left_join(
    eligible_selection |>
      dplyr::select(-point_id),
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::mutate(
    label_text =
      dplyr::case_when(
        !is.na(name) ~ paste(name, "<br/>", road_reference),
        TRUE ~ NA_character_
      ),
    label_text = lapply(label_text, htmltools::HTML)
  )

readr::write_rds(
  population_tidy,
  "new_area_index/links_vfl_2024.rds"
)


# Tønsberg ----
municipality_ids <- c(3905, 3911)
# Must include Nøtterøy as some of urban area Tønsberg lies here

urban_area_tbg <-
  urban_areas |>
  dplyr::filter(
    tettstednummer %in% c(2521, 2631, 2621, 2633)
  ) |>
  sf::st_transform("wgs84") |>
  dplyr::summarise(
    geometry = sf::st_union(geometry)
  ) |>
  sf::st_convex_hull()

city_link_population_raw <-
  get_link_population_inside_municipalities(municipality_ids) |>
  sf::st_filter(urban_area_tbg, .predicate = st_covered_by) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    road_reference_1 = purrr::pluck(road_system_references, 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    # E18 has high percentage through traffic so it will not be part of population.
    !stringr::str_detect(road_reference_1, "EV18"),
  ) |>
  dplyr::select(-road_reference_1) |>
  dplyr::filter(
    function_class %in% c("A", "B", "C", "D"),
    # Need to remove some links on Nøtterøy
    !(link_id %in% c(
      "0.0@2715360-0.52293776@1175848"
    ))
  ) |>
  dplyr::bind_rows(
    links |>
      dplyr::filter(
        link_id %in% c(
          # fv. 35 Revetal
          "0.0@2252722-0.41238247@1175824"
        )
      )
  ) |>
  dplyr::left_join(
    link_trp_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    traffic_volumes,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::left_join(
    link_toll_id,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rename(
    point_id = trp_id
  )

eligible_selection <- find_eligible_trp_selection()

population_tidy <-
  city_link_population_raw |>
  dplyr::left_join(
    eligible_selection |>
      dplyr::select(-point_id),
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::mutate(
    label_text =
      dplyr::case_when(
        !is.na(name) ~ paste(name, "<br/>", road_reference),
        TRUE ~ NA_character_
      ),
    label_text = lapply(label_text, htmltools::HTML)
  )

readr::write_rds(
  population_tidy,
  "new_area_index/links_tbg_2024.rds"
)
