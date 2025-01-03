# Index TRPs for Excel

{
  base::Sys.setlocale(locale = "nb.utf8")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  source("get_from_trafficdata_api.R")
  library(writexl)
}

## City IDs ----
# Bergen 8952
# Buskerudbyen 1952
# Grenland 955
# Kristiansand og omegn 957 kommune 956
# Nedre Glomma 18952
# Nord-Jæren 952
# Oslo 959
# Trondheim 960
# Tromsø 961
# Tromsø 2022 16952

city_ids <- c(8952, 1952, 955, 957, 18952, 952, 959, 960, 16952)
city_names <-
  c(
    "Bergensområdet",
    "Buskerudbyen",
    "Grenland",
    "Kristiansandområdet",
    "Nedre Glomma",
    "Nord-Jæren",
    "Osloområdet",
    "Trondheimsområdet",
    "Tromsø"
  )

cities <-
  tibble::tibble(
    area_id = city_ids,
    area_name = city_names
  )

## TRP names ----
city_index_trps <-
  purrr::map(
    city_ids,
    ~ readr::read_rds(
      file = paste0(
        "index_trp_metadata/trp_",
        .x,
        ".rds"
      )
    ) |>
      dplyr::mutate(
        area_id = .x
      )
  ) |>
  purrr::list_rbind() |>
  dplyr::filter(
    station_type_short == "T" | is.na(station_type_short)
  ) |>
  dplyr::select(
    area_id,
    county_name,
    municipality_name,
    trp_id,
    name,
    road_reference
  ) |>
  split_road_system_reference()  |>
  dplyr::left_join(
    cities,
    by = "area_id"
  ) |>
  dplyr::select(
    area_name,
    road_category,
    county_name,
    municipality_name,
    trp_id,
    name,
    road_category_and_number,
    road_reference
  ) |>
  dplyr::arrange(
    area_name
  )


writexl::write_xlsx(
  city_index_trps,
  "trafikkindekspunkt.xlsx"
)
