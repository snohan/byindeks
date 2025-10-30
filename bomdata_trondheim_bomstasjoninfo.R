source("get_from_nvdb_api.R")

# The 20 to use
tolling_station_ids_nvdb <-
  c(
    "51",  # Dette er egentlig to ulike, hver med to felt
    # Klett (1,2), Røddeveien (3,4)
    # Endrer nedenfor Røddeveien til id 512 og felt 1 og 2
    "512",
    "52", "53", "54", "55",
    "56", # "Kroppan bru", som egentlig ikke er på Kroppan bru, men
    # Holtermannsvegen utenfor Siemens er to stasjoner, også 57.
    # Slår disse sammen nedenfor, og setter feltnummer etter dagens metrering
    "58", "59", "60", "61", "62", "64", "65", "66", "67",
    "68", "69", "85", "86",
    #"72"
    # From 01.11.2023, Ranheim changed ID from 72 (operator ID 100121) to 1 (operator ID 100149)
    "1"
  )

kommunenr <- "5001"
kommunenavn <-
  hent_kommune_v3(kommunenr) |>
  dplyr::select(kommunenavn) |>
  purrr::pluck(1)

kommune_bomer_uttak <-
  get_tolling_stations(kommunenr)

kommune_bomer <-
  kommune_bomer_uttak %>%
  dplyr::rename(
    trp_id = msnr
  ) %>%
  dplyr::mutate(
    station_type = "Bomstasjon"
  ) %>%
  dplyr::select(trp_id, everything()) %>%
  dplyr::filter(
    trp_id %in% tolling_station_ids_nvdb
  ) %>%
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
        TRUE ~ trp_id
      ),
    municipality_name = "Trondheim"
  ) %>%
  dplyr::arrange(
    trp_id
  )

readr::write_rds(
  kommune_bomer,
  file = "bomdata_trondheim/trd_toll_stations.rds"
)


# Names from toll data files
# bom_felt_og_stasjon <-
#   read.csv2(
#     "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_felt_og_stasjon.csv"
#   ) %>%
#   dplyr::select(-felt) %>%
#   dplyr::rename(name = stasjon)
#
# trh_bomer <-
#   kommune_bomer %>%
#   dplyr::select(-name) %>%
#   dplyr::left_join(bom_felt_og_stasjon, by = c("msnr" = "kode")) %>%
#   dplyr::select(-msnr) %>%
#   dplyr::mutate(municipality_name = "Trondheim")
