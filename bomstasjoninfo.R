source("get_from_nvdb_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")


# Trondheim ----
# Toll station ids the way they appear in NVDB and Autopass
tolling_station_ids_nvdb <-
  c(
    "51",
    "52", "53", "54", 
    "55", # Before ved Sluppen bru, then moved to north of Nydalsbrua
    "56",
    "58", "59", "60", "61", "62", "64", "65", "66", "67",
    "68", "69", "85", "86",
    "1" # "72"
  )

kommunenr <- "5001"
kommunenavn <-
  hent_kommune(kommunenr) |>
  dplyr::select(kommunenavn) |>
  purrr::pluck(1)

kommune_bomer_uttak <- get_tolling_stations(kommunenr)

kommune_bomer <-
  kommune_bomer_uttak |> 
  dplyr::mutate(station_type = "Bomstasjon") |> 
  dplyr::select(trp_id, everything()) |> 
  dplyr::filter(trp_id %in% tolling_station_ids_nvdb) |> 
  dplyr::mutate(
    name = stringr::str_replace(name, "\\,.+$", ""),
    name = stringr::str_replace(name, " M-snitt\\)$", ""),
    name = stringr::str_replace(name, "\\. K-snitt$", ""),
    name = stringr::str_replace(name, " \\(Nordgående\\)$", ""),
    name = stringr::str_replace(name, " \\(Sørgående\\)$", ""),
    name = stringr::str_replace(name, "Rv.707", "Fv 707"),
    trp_id =
      dplyr::case_when(
        name == "Klett Røddeveien" ~ "512",
        trp_id == "1" ~ "72",
        TRUE ~ trp_id
      ),
    municipality_name = "Trondheim"
  ) |> 
  # Remove new version of 55, add the old one
  dplyr::filter(
    trp_id != "55"
  ) |> 
  dplyr::bind_rows(
    tibble::tribble(
      ~trp_id, ~nvdb_id, ~name, ~road_reference, ~road_link_position, ~lat, ~lon, ~station_type, ~municipality_name,
      "55", "264832266", "Nord for Sluppen bru", "RV706 S1D20 m202", "0.50809783@72863", 63.39652, 10.38831, "Bomstasjon", "Trondheim"
    )
  ) |> 
  dplyr::arrange(trp_id) |> 
  split_road_system_reference() |> 
  dplyr::select(
    trp_id, nvdb_id, name, road_reference, road_category_and_number, road_link_position, lat, lon, station_type, municipality_name,
  )

readr::write_rds(
  kommune_bomer,
  file = "bomdata_trondheim/trd_toll_stations.rds"
)


# Haugesund ----
bomstasjoner_haugalandspakken <- 
  get_tolling_stations(1106, "2025-04-01") |> 
  dplyr::mutate(station_type = "Bomstasjon") |> 
  split_road_system_reference() |> 
  dplyr::select(
    trp_id, nvdb_id, name, road_reference, road_category_and_number, road_link_position, lat, lon, station_type
  ) |> 
  dplyr::arrange(trp_id)

readr::write_rds(
  bomstasjoner_haugalandspakken,
  file = "bomdata_haugesund/bomstasjoner_haugalandspakken.rds"
)

bomstasjoner_bypakke_haugesund <- 
  get_tolling_stations(1106) |> # Ikke tilgjengelig i NVDB per 23.04.2026
  dplyr::mutate(station_type = "Bomstasjon") |> 
  split_road_system_reference() |> 
  dplyr::select(
    trp_id, nvdb_id, name, road_reference, road_category_and_number, road_link_position, lat, lon, station_type
  ) |> 
  dplyr::arrange(trp_id)

readr::write_rds(
  bomstasjoner_bypakke_haugesund,
  file = "bomdata_haugesund/bomstasjoner_bypakke_haugesund.rds"
)