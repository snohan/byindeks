# Setup ----
{
  source("rmd_setup.R")
  source("get_from_nvdb_api.R")
  library(lubridate)
}


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

jsonlite::write_json(
  traffic_work,
  path = "trafikkarbeid_2021.json",
  prettify = TRUE
)
