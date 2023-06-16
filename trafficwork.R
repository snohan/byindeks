# Setup ----
{
  source("rmd_setup.R")
  source("get_from_nvdb_api.R")
  library(stringi)
}


# Get AADT-links ----
# 3  Oslo
# 30 Viken
# 34 Innlandet
# 38 Vestfold og Telemark
# 42 Agder
# 11 Rogaland
# 46 Vestland
# 15 Møre og Romsdal
# 50 Trøndelag
# 18 Nordland
# 54 Troms og Finnmark

last_day_of_year <- "2022-12-31"

t_03 <- get_aadt_by_area(3, "true", last_day_of_year)
t_30 <- get_aadt_by_area(30, "true", last_day_of_year)
t_34 <- get_aadt_by_area(34, "true", last_day_of_year)
t_38 <- get_aadt_by_area(38, "true", last_day_of_year)
t_42 <- get_aadt_by_area(42, "true", last_day_of_year)
t_11 <- get_aadt_by_area(11, "true", last_day_of_year)
t_46 <- get_aadt_by_area(46, "true", last_day_of_year)
t_15 <- get_aadt_by_area(15, "true", last_day_of_year)
t_50 <- get_aadt_by_area(50, "true", last_day_of_year)
t_18 <- get_aadt_by_area(18, "true", last_day_of_year)
t_54 <- get_aadt_by_area(54, "true", last_day_of_year)

aadt_link_raw <-
  dplyr::bind_rows(
    t_03,
    t_30,
    t_34,
    t_38,
    t_42,
    t_11,
    t_46,
    t_15,
    t_50,
    t_18,
    t_54
  )

readr::write_rds(
  aadt_link_raw,
  file = "aadt_link_raw_2021.rds"
)


# Calculate traffic work ----
traffic_work <-
  aadt_link_raw |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    road_category =
      dplyr::case_when(
        road_category == "E" ~ "E+R",
        road_category == "R" ~ "E+R",
        TRUE ~ road_category
      )
  ) |>
  dplyr::group_by(
    county_numbers,
    road_category
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(aadt_total * 365 * length) / 1e9,
    .groups = "drop"
  ) |>
  dplyr::select(
    Fylkenr = county_numbers,
    Vegkategori = road_category,
    trafikkarbeid = traffic_work_mill_km
  )

readr::write_rds(
  traffic_work,
  file = "traffic_work_2021.rds"
)


# For weighting in VTI ----
jsonlite::write_json(
  traffic_work,
  path = "trafikkarbeid_2021.json",
  prettify = TRUE
)


# Traffic work per use class ----
# Will use road reference as a very simplified geometry, as full geometry is too heavy computationally.
aadt_link_raw <-
  readr::read_rds(
    file = "aadt_link_raw_2021.rds"
  ) |>
  dplyr::select(
    nvdb_objekt_id,
    aadt_total,
    heavy_percentage,
    road_reference_section = shortform
  ) |>
  sf::st_drop_geometry() |>
  split_road_system_reference_section() |>
  dplyr::arrange(
    road_system,
    meter_start,
    meter_end
  )


# Read CSV fetched from vegkart.no
# Roadnet selection: not walking and cycling, not roundabouts, just ERF
# Date: 2022-12-31

read_use_class_file <- function(file_name) {

  readr::read_csv2(
    file_name,
    col_select =
      c(
        `VEGOBJEKT-ID`,
        LOK.VEGSYSTEMREFERANSE
      )
  ) |>
  dplyr::rename(
    id = `VEGOBJEKT-ID`,
    road_reference_section = LOK.VEGSYSTEMREFERANSE
  ) |>
  dplyr::filter(
    !is.na(road_reference_section)
  )
}

bk10_50_files <-
  base::list.files(
    "spesialuttak",
    pattern = "^bk10_50.+",
    full.names = TRUE
  )

bk10_50 <-
  purrr::map(bk10_50_files, ~ read_use_class_file(.x)) |>
  purrr::list_rbind()

bk10_50_tidy <-
  bk10_50 |>
  dplyr::select(
    road_reference_section
  ) |>
  dplyr::distinct() |>
  split_road_system_reference_section() |>
  dplyr::filter(
    !is.na(road_system)
  ) |>
  dplyr::mutate(
    use_class = "bk10_50"
  )


bk10_60_files <-
  base::list.files(
    "spesialuttak",
    pattern = "^bk10_60.+",
    full.names = TRUE
  )

bk10_60 <-
  purrr::map(bk10_60_files, ~ read_use_class_file(.x)) |>
  purrr::list_rbind()

bk10_60_tidy <-
  bk10_60 |>
  dplyr::select(
    road_reference_section
  ) |>
  dplyr::distinct() |>
  split_road_system_reference_section() |>
  dplyr::filter(
    !is.na(road_system)
  ) |>
  dplyr::select(
    -road_reference_section
  ) |>
  dplyr::arrange(
    road_system,
    meter_start
  )

bk10_60_reduced <-
  bk10_60_tidy |>
  tidyr::pivot_longer(
    cols = c(meter_start, meter_end),
    names_to = "extreme",
    values_to = "meter"
  ) |>
  dplyr::mutate(
    same_road_system = road_system == dplyr::lag(road_system, default = "TRUE"),
    same_meter_lag = meter == dplyr::lag(meter) & same_road_system,
    same_meter_lead = meter == dplyr::lead(meter) & same_road_system
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    connecting = base::any(same_meter_lag, same_meter_lead)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    connecting == FALSE
  ) |>
  dplyr::select(
    road_system,
    extreme,
    meter
  ) |>
  tidyr::pivot_wider(
    names_from = "extreme",
    values_from = "meter",
    values_fn = list
  ) |>
  tidyr::unnest(cols = everything()) |>
  dplyr::mutate(
    use_class = "bk10_60"
  )

  # sf::st_as_sf(
  #   wkt = "geometry",
  #   crs = 5973
  # ) |>
  # sf::st_zm(drop = T, what = "ZM") |>
  # sf::st_transform("+proj=longlat +datum=WGS84")

# test <-
#   sf::st_combine(bk10_50_normal$geometry) |>
#   sf::st_union()

# TODO: conditional joins, one for meter_start and one for meter_end
# TODO: avoid getting overlap in just intersections: add one meter to start of traffic link, subtract one from end?

aadt_link_bk <-
  aadt_link_raw |>
  dplyr::left_join(
    bk10_60_reduced,
    by = dplyr::join_by(
      road_system == road_system,
      meter_start >= meter_start,
      meter_end <= meter_end
    )
  )

# OBS! aadt_links do not explicitly list all road system reference sections they cover!!! ?


## Test ----
# En trafikkmengdelenke ved Jonsvatnet som overlapper kun i krysset med en bk10_50-lenke
# 1015060830

# En trafikkmengdelenke ved Jonsvatnet som ikke overlapper med en bk10_50-lenke
# 1015060829

# En trafikkmengdelenke ved Jonsvatnet som helt overlapper med en bk10_50-lenke
# 1015060781

aadt_link_test <-
  aadt_link_raw |>
  dplyr::filter(
    nvdb_objekt_id %in% c("1015060830", "1015060829", "1015060781")
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    bk10_50 = sf::st_intersects(geometry, test)
  )
