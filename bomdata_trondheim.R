library(tidyverse)
library(lubridate)
library(readxl)

# Alle filer fra og med 2017 er på samme format.
data_files <- list.files("H:/Programmering/R/byindeks/bomdata_trondheim/raw",
                         all.files = TRUE, no.. = TRUE,
                         full.names = TRUE)

all_data <- do.call(bind_rows,
                    lapply(data_files,
                           readxl::read_xlsx))

all_data_tidy <- all_data %>%
  dplyr::select(1,4:7) %>%
  dplyr::filter(Dato != "29.02.2016") %>%
  dplyr::rename(dato = Dato,
                stasjon = Stasjon,
                felt = 3,
                klasse = Klasse,
                trafikk = Passeringer) %>%
  dplyr::mutate(dato = lubridate::dmy(dato),
                aar_maaned = lubridate::floor_date(dato, unit = "month"),
                felt = stringr::str_sub(felt, 1, 5),
                felt = stringr::str_replace(
                  felt, "KLE-1", "KLETT-E6"),
                felt = stringr::str_replace(
                  felt, "KLE-2", "KLETT-E6"),
                felt = stringr::str_replace(
                  felt, "KLE-3", "KLETT-E6"),
                felt = stringr::str_replace(
                  felt, "KLE-4", "KLETT-E6"),
                klasse = stringr::str_replace(klasse, " ", "_")) %>%
  dplyr::filter(!(felt %in% c("BUVIK", "E39-T", "HOMVI", "JONSV",
                              "RAMPE", "Rødde", "THAMS",
                              "TONST", "ØYSAN", "LEIST"))) %>%
  dplyr::mutate(stasjon = stringr::str_replace(stasjon, "\\,.+$", ""),
                stasjon = stringr::str_replace(stasjon, " M-snitt\\)$", ""),
                stasjon = stringr::str_replace(stasjon, "\\. K-snitt$", ""),
                stasjon = stringr::str_replace(stasjon, " \\(Nordgående\\)$", ""),
                stasjon = stringr::str_replace(stasjon, " \\(Sørgående\\)$", "")) %>%
  dplyr::select(aar_maaned, stasjon, felt, klasse, trafikk)

all_data_monthly <- all_data_tidy %>%
  dplyr::group_by(aar_maaned, stasjon, felt) %>%
  dplyr::summarise(trafikkvolum_alle = sum(trafikk))

feltkoder <- tibble::enframe(unique(all_data_monthly$felt))
stasjoner <- tibble::enframe(unique(all_data_monthly$stasjon))

all_data_monthly_by_class <- all_data_tidy %>%
  dplyr::group_by(aar_maaned, stasjon, felt, klasse) %>%
  dplyr::summarise(trafikkvolum = sum(trafikk)) %>%
  dplyr::left_join(all_data_monthly, by = c("aar_maaned" = "aar_maaned",
                                            "stasjon" = "stasjon",
                                            "felt" = "felt")) %>%
  dplyr::mutate(trafikkvolum_andel = round(100 * trafikkvolum / trafikkvolum_alle,
                                           digits = 2))

write.csv2(all_data_monthly_by_class,
           file = "H:/Programmering/R/byindeks/data_index_raw/bomdata_trondheim_maanedlig.csv",
           row.names = FALSE)
