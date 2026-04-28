source("get_from_nvdb_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

kommune_info <- get_municipalities()


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

bomstasjoner_trd <- 
  get_tolling_stations(5001) |> 
  dplyr::mutate(station_type = "Bomstasjon") |> 
  dplyr::select(trp_id, everything()) |> 
  dplyr::filter(trp_id %in% tolling_station_ids_nvdb) |> 
  dplyr::left_join(kommune_info, by = "municipality_number") |> 
  dplyr::select(-municipality_number, county_number) |> 
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
      )
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
    trp_id, nvdb_id, name, road_reference, road_category_and_number, road_link_position, lat, lon, station_type, municipality_name
  )

readr::write_rds(
  bomstasjoner_trd,
  file = "bomdata_trondheim/trd_toll_stations.rds"
)


# Haugesund ----
bomstasjoner_haugalandspakken <- 
  get_tolling_stations(1106, "2025-04-01") |> 
  dplyr::left_join(kommune_info, by = "municipality_number") |> 
  dplyr::mutate(station_type = "Bomstasjon") |> 
  split_road_system_reference() |> 
  dplyr::select(
    trp_id, nvdb_id, name, road_reference, road_category_and_number, road_link_position, lat, lon, station_type, municipality_name
  ) |> 
  dplyr::arrange(trp_id) |> 
  dplyr::filter(trp_id != 5) # Toskatjønn ligger utenfor tettstedet, og har sikkert stor andel gjennomgangstrafikk

readr::write_rds(
  bomstasjoner_haugalandspakken,
  file = "bomdata_haugesund/bomstasjoner_haugalandspakken.rds"
)

bomstasjoner_bypakke_haugesund <- 
  get_tolling_stations(1106) |> # Ikke tilgjengelig i NVDB per 23.04.2026
  dplyr::left_join(kommune_info, by = "municipality_number") |> 
  dplyr::mutate(station_type = "Bomstasjon") |> 
  split_road_system_reference() |> 
  dplyr::select(
    trp_id, nvdb_id, name, road_reference, road_category_and_number, road_link_position, lat, lon, station_type, municipality_name
  ) |> 
  dplyr::arrange(trp_id)

readr::write_rds(
  bomstasjoner_bypakke_haugesund,
  file = "bomdata_haugesund/bomstasjoner_bypakke_haugesund.rds"
)