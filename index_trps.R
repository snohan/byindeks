# Index TRPs for Excel

{
  base::Sys.setlocale(locale = "nb.utf8")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  source("get_from_trafficdata_api.R")
  library(writexl)
}

## City IDs ----
# 2016:
#   Buskerudbyen 1952
#   Grenland 955
# 2017:
#   Nord-Jæren 952
# 2018:
#   Bergen 8952
#   Oslo 959
# 2019:
#   Trondheim 960
# 2022:
#   Tromsø 2022 16952
# 2023:
#   Kristiansandsregionen 19953
#   Nedre Glomma 18952
# 2024:
#   Bodø 19954
#   Ålesund 20952
# 2025:
#   Haugesund 19955

cities <-
  tibble::tribble(
    ~area_id, ~area_name,
    1952, "Buskerudbyen",
    955, "Grenland",
    952, "Nord-Jæren",
    8952, "Bergensområdet",
    959, "Osloområdet",
    960, "Trondheimsområdet",
    16952, "Tromsø",
    19953, "Kristiansandsregionen",
    18952, "Nedre Glomma",
    19954, "Bodø",
    20952, "Ålesund",
    19955, "Haugesund"
  )


## TRP names ----
city_index_trps <-
  purrr::map(
    cities$area_id,
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


# Find TRSs
index_stations <-
  city_index_trps |> 
  dplyr::select(
    trp_id
  ) |> 
  dplyr::left_join(
    readr::read_rds("H:/Programmering/R/trafikkdata/trp_info/trs_trp_ids.rds") |> 
      dplyr::select(trp_id, trs_id),
    by = "trp_id"
  ) |> 
  dplyr::select(trs_id) |> 
  dplyr::distinct()

writexl::write_xlsx(
  index_stations,
  "trafikkindeksstasjoner.xlsx"
)