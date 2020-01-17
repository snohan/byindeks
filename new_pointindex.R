# Testing new pointindex
source("rmd_setup.R")
source("get_from_trafficdata_api.R")
source("get_from_trp_api.R")
library(tictoc)

#trps <- get_trp_for_vti()
trps <- read.csv2("trp_for_vti_2020.csv") %>%
  dplyr::select(county_name, trp_id, legacyNortrafMpn, name, road_reference,
                road_category, latest_data_by_hour, adt,
                coverage,valid_length_percent)

trp_old_pointindex <- read.csv2("pointindex-2019-12_2018-12.csv") %>%
  dplyr::filter(periode == "Desember",
                lengdeklasse == "Alle",
                døgn == "Alle") %>%
  dplyr::select(msnr, indeks, dekning, trafikkmengde.indeksår,
                trafikkmengde.basisår) %>%
  dplyr::rename(legacyNortrafMpn = msnr,
                indeksvolum = trafikkmengde.indeksår,
                basisvolum = trafikkmengde.basisår) %>%
  dplyr::mutate(indeks = as.numeric(decimal_point(indeks)),
                dekning = as.numeric(decimal_point(dekning)),
                indeksvolum = as.numeric(as.character(indeksvolum)),
                basisvolum = as.numeric(as.character(basisvolum)))


#trp_ids <- '23392V625266", "44656V72812'

trp_ids_clean <- trps %>%
  dplyr::mutate(trp_id = as.character(trp_id)) #%>%
  #dplyr::filter(trp_id != "90390V443603")

trp_ids <- trp_ids_clean$trp_id %>%
  stringr::str_c(collapse = "\", \"")

# Print to screen for use in GraphiQL
#cat(trps$trp_id[1:2], sep = '", "')

tic()
trp_index <- get_pointindices(trp_ids,
  #"90390V443603",
  "2019")
toc()

trp_index_filtered <- trp_index %>%
  dplyr::filter(length_range == "[..,..)",
                month == 12) %>%
  dplyr::select(trp_id, base_volume, calculation_volume,
                index_p, coverage_percentage)

trp_with_index <- trps %>%
  dplyr::left_join(trp_index_filtered) %>%
  dplyr::left_join(trp_old_pointindex) %>%
  #dplyr::filter(!is.na(index_p)) %>%
  #dplyr::filter(trp_id %in% trp_ids_clean$trp_id) %>%
  dplyr::filter(!is.na(indeks)) %>%
  dplyr::mutate(index_diff = index_p - indeks,
                coverage_diff = coverage_percentage - dekning,
                base_diff = base_volume - basisvolum,
                calc_diff = calculation_volume - indeksvolum)

# Eksempel Berge øst 54608V320601

hour_volumes_base <- getHourlytraffic("54608V320601",
                                      "2018-12-01T00:00:00+01:00",
                                      "2019-01-01T00:00:00+01:00")

hour_volumes_calc <- getHourlytraffic("54608V320601",
                                      "2019-12-01T00:00:00+01:00",
                                      "2020-01-01T00:00:00+01:00")
