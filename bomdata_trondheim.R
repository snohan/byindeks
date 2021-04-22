library(tidyverse)
library(lubridate)
library(readxl)

# Vask alle bomdata ####
# Alle filer fra og med 2017 er på samme format.
data_files <- list.files("H:/Programmering/R/byindeks/bomdata_trondheim/raw_new",
                         all.files = TRUE, no.. = TRUE,
                         full.names = TRUE)

all_data <- do.call(bind_rows,
                    lapply(data_files,
                           readxl::read_xlsx))

all_data_tidy <- all_data %>%
  dplyr::select(1,4:7) %>%
  dplyr::filter(Dato != "29.02.2016") %>%
  dplyr::filter(Dato != "29.02.2020") %>%
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

#feltkoder <- tibble::enframe(unique(all_data_monthly$felt))
#stasjoner <- tibble::enframe(unique(all_data_monthly$stasjon))

all_data_monthly_by_class <- all_data_tidy %>%
  dplyr::group_by(aar_maaned, stasjon, felt, klasse) %>%
  dplyr::summarise(trafikkvolum = sum(trafikk)) %>%
  dplyr::left_join(all_data_monthly, by = c("aar_maaned" = "aar_maaned",
                                            "stasjon" = "stasjon",
                                            "felt" = "felt")) %>%
  dplyr::mutate(trafikkvolum_andel = round(100 * trafikkvolum / trafikkvolum_alle,
                                           digits = 2))

write.csv2(all_data_monthly_by_class,
           file = "H:/Programmering/R/byindeks/data_index_raw/bomdata_trondheim_maanedlig_new.csv",
           row.names = FALSE)


# Evt. henter inn ferdig vasket bomdata ####
# all_data_monthly_by_class <- read_csv2(
#   file = "H:/Programmering/R/byindeks/data_index_raw/bomdata_trondheim_maanedlig_new.csv",
#   locale = readr::locale(encoding = "latin1")
# )

# Ekskluderinger ####
# Tungasletta høy andel ukjente
# tungasletta <- all_data_monthly_by_class %>%
#   dplyr::filter(stasjon == "Tungasletta",
#                 aar_maaned > "2018-05-31",
#                 aar_maaned < "2019-01-01")
# Juli og aug 2018 må ekskluderes da ukjentandelen er over 30 %!

all_data_monthly_by_class_excluded <- all_data_monthly_by_class %>%
  dplyr::select(-trafikkvolum_andel, -trafikkvolum_alle) %>%
  dplyr::filter(!(felt == "TUNGA" & aar_maaned == "2018-07-01")) %>%
  dplyr::filter(!(felt == "TUNGA" & aar_maaned == "2018-08-01"))

all_data_monthly_by_class_excluded_total <-
  all_data_monthly_by_class_excluded %>%
  dplyr::group_by(aar_maaned, stasjon, felt) %>%
  dplyr::summarise(trafikkvolum = sum(trafikkvolum)) %>%
  dplyr::mutate(klasse = "Alle") %>%
  dplyr::ungroup()

all_data_monthly_by_all_classes <- all_data_monthly_by_class_excluded_total %>%
  dplyr::bind_rows(all_data_monthly_by_class_excluded) %>%
  dplyr::mutate(year = lubridate::year(aar_maaned),
                month = lubridate::month(aar_maaned))

# Legger til stasjonkoder slik de er i NVDB
bomfeltkoder <- read.csv2(
  "H:/Programmering/R/byindeks/bomdata_trondheim/bomfeltkoder.csv")

felt_og_stasjon <- all_data_monthly_by_all_classes %>%
  dplyr::distinct_at(vars(stasjon, felt)) %>%
  dplyr::left_join(bomfeltkoder)

write.csv2(felt_og_stasjon,
           file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_felt_og_stasjon.csv",
           row.names = F)

# Månedsindeks ####
calculate_monthly_index_for_tolling_stations <- function(monthly_class_data, baseyear) {
    basedata <- monthly_class_data %>%
      dplyr::filter(year == baseyear)

    calcdata <- monthly_class_data %>%
      dplyr::filter(year == baseyear + 1)

    indexdata <- dplyr::inner_join(basedata,
                                   calcdata,
                                   by = c("month", "felt", "klasse"),
                                   suffix = c("_base", "_calc"),
                                   ) %>%
      dplyr::group_by(felt, klasse, month) %>%
      dplyr::summarise(monthly_volume_base = sum(trafikkvolum_base),
                       monthly_volume_calc = sum(trafikkvolum_calc)) %>%
      dplyr::mutate(indeks = (monthly_volume_calc /
                                monthly_volume_base - 1) * 100,
                    aar_maaned = lubridate::ymd(
                      paste(baseyear + 1, month, "1", sep = "-")))
}

bomindeks_2017 <- all_data_monthly_by_all_classes %>%
  dplyr:: select(-stasjon, -aar_maaned) %>%
  #dplyr::filter(klasse == "Liten_bil") %>%
  calculate_monthly_index_for_tolling_stations(2016)

bomindeks_2018 <- all_data_monthly_by_all_classes %>%
  dplyr:: select(-stasjon, -aar_maaned) %>%
  #dplyr::filter(klasse == "Liten_bil") %>%
  calculate_monthly_index_for_tolling_stations(2017)

bomindeks_2019 <- all_data_monthly_by_all_classes %>%
  dplyr:: select(-stasjon, -aar_maaned) %>%
  #dplyr::filter(klasse == "Liten_bil") %>%
  calculate_monthly_index_for_tolling_stations(2018)

bomindeks_2020 <- all_data_monthly_by_all_classes %>%
  dplyr:: select(-stasjon, -aar_maaned) %>%
  #dplyr::filter(klasse == "Liten_bil") %>%
  calculate_monthly_index_for_tolling_stations(2019)

bomindeks_2021 <- all_data_monthly_by_all_classes %>%
  dplyr:: select(-stasjon, -aar_maaned) %>%
  #dplyr::filter(klasse == "Liten_bil") %>%
  calculate_monthly_index_for_tolling_stations(2020)

maanedsindekser <- bind_rows(bomindeks_2017,
                             bomindeks_2018,
                             bomindeks_2019,
                             bomindeks_2020,
                             bomindeks_2021) %>%
  dplyr::left_join(felt_og_stasjon)

write.csv2(maanedsindekser,
           file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser.csv",
           row.names = F)

# TODO: Dekningsgrad for antall måneder
aarsindekser <- maanedsindekser %>%
  dplyr::mutate(year = year(aar_maaned)) %>%
  dplyr::group_by(felt, year, klasse) %>%
  dplyr::summarise(base_volume = sum(monthly_volume_base),
                   calc_volume = sum(monthly_volume_calc),
                   indeks = (calc_volume /
                               base_volume - 1) * 100) %>%
  dplyr::left_join(felt_og_stasjon)

write.csv2(aarsindekser,
           file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.csv",
           row.names = F)

# Se plott for å se etter avvik i bomdata_trondheim.Rmd
city_monthly_toll_indeces <- maanedsindekser %>%
  dplyr::group_by(felt, aar_maaned, klasse) %>%
  dplyr::summarise(base_volume = sum(monthly_volume_base),
                   calc_volume = sum(monthly_volume_calc),
                   indeks = (calc_volume /
                               base_volume - 1) * 100) %>%
  dplyr::left_join(felt_og_stasjon)

write.csv2(city_monthly_toll_indeces,
           file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_bymaanedsindekser.csv",
           row.names = F)
