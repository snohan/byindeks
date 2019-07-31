# Gathering info on all points and indexes

# Setup ####

# Packages are to be loaded through sourcing rmd_setup.R in the Rmd report file.
source("rmd_setup.R")

# Traffic Data API calls to get points metadata and aadt
source("get_from_trafficdata_api.R")

# If necessary, get all TRPs from TRP API
# TRPs without commissions are not i TD-API!
source("get_from_trp_api.R")

# NVDB API calls to get tolling stations
source("get_from_nvdb_api.R")

# Points ####

# Points used in each city
cities_points <- read_csv2("data_points_raw/cities_points.csv")
cities_points_unestablished <-
  read_csv2("data_points_raw/points_unestablished.csv")

# All points from Traffic Data API
points <- getPoints()

# All points from TRP API (if needed)
points_trp <- getPointsFromTRPAPI()

# Oslo og Akershus 2019 ####
oslopunkter <- cities_points %>%
  dplyr::filter(city_area_name == "Oslo og Akershus",
                agreement_start == 2019) %>%
  dplyr::mutate(established = "Ja" )

# Adding metadata
indekspunkter_oslo <- dplyr::left_join(oslopunkter, points) %>%
  dplyr::select(1:5, 7:11, 6)

# Legger inn uetablerte punkter
indekspunkter_oslo_uetablerte <-
  read.csv2("points_not_yet_established.csv") %>%
  dplyr::filter(city_area_name == "Oslo og Akershus",
                agreement_start == 2019) %>%
  dplyr::mutate(established = "Nei" )

indekspunktene_oslo <- bind_rows(indekspunkter_oslo,
                                 indekspunkter_oslo_uetablerte)

write.csv2(indekspunktene_oslo,
           file = "data_indexpoints_tidy/indekspunktene_oslo_2019.csv",
           row.names = F)


# ADT
oslo_adt <- getAdtForpoints(indekspunktene_oslo$trp_id)
# TODO: filtrere ut 2018
# TODO: join
# TODO: hente fra NVDB de som mangler?

# Test
#uten_adt <- getTrpAadt("32135V604101")


# Trondheim 2017 ####
trp_trondheim_2017_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Trondheim",
                agreement_start == 2017) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn)

# Adding metadata
# Bruker trp fra trp-api, da noen mangler igangsetting.
trp_trondheim_2017 <- dplyr::left_join(trp_trondheim_2017_ids, points_trp) %>%
  # Må ta vekk ene punktet i Strindheimtunnelen
  dplyr::filter(trp_id != "21571V2394246") %>%
  # Må skille trs fra bom
  dplyr::mutate(station_type = "TRS")

# Bompunkter i Trondheim
kommunenr <- "5001"
kommunenavn <- hent_kommune(kommunenr)[[1]]

kommune_bomer <-
  get_tolling_stations(kommunenr) %>%
  dplyr::mutate(station_type = "Bom") %>%
  dplyr::select(-kommune, -road) %>%
  dplyr::mutate(trp_id = msnr,
                msnr = as.numeric(msnr)) %>%
  dplyr::select(trp_id, everything()) %>%
  dplyr::filter(trp_id %in% c("51", "52", "53", "54", "55", "56", "58",
                              "59", "60", "61", "62", "64", "65", "66",
                              "67", "68", "69", "85", "86")) %>%
  dplyr::mutate(name = str_sub(name, 1, -10))

# Må endre navn på Kroppan bru, dvs. ta bor retningsangivelse, da de to
# bomstasjonene er slått sammen i indeksberegningen.
kommune_bomer$name[kommune_bomer$trp_id == 56] <- "Kroppan bru"

# Må legge til for 52 som er borte fra API-et.
bom_52 <- data.frame(
  "trp_id" = "52",
  "msnr" = 52,
  "name" = "Klett - E6, S-snitt",
  "road_reference" = "Ev6 hp9 m1252",
  "lat" = 63.32590,
  "lon" = 10.32702,
  "station_type" = "Bom",
  stringsAsFactors = F)

trp_trondheim_2017_alle <-
  bind_rows(trp_trondheim_2017, kommune_bomer, bom_52)

# Add index results from CSV-files
pointindex_17_18 <-
  read.csv2("data_index_raw/punktindeks_trondheim_alle_punkter_jan-des18.csv") %>%
  mutate(trs = as.numeric(msnr),
         trs = if_else(trs > 9916000, trs - 9916000, trs)) %>%
  select(-msnr) %>%
  group_by(trs) %>%
  summarise(trafikkmengde_basisaar = sum(trafikkmengde.basisaar),
            trafikkmengde_indeksaar = sum(trafikkmengde.indeksaar),
            index_17_18 = round((trafikkmengde_indeksaar/
                             trafikkmengde_basisaar - 1) * 100,
                          digits = 1)) %>%
  rename(msnr = trs) %>%
  select(msnr, index_17_18)

pointindex_18_19 <-
  read.csv2("data_index_raw/punktindeks_trondheim_alle_punkter_jan-apr19.csv") %>%
  mutate(trs = as.numeric(msnr),
         trs = if_else(trs > 9916000, trs - 9916000, trs)) %>%
  select(-msnr) %>%
  group_by(trs) %>%
  summarise(trafikkmengde_basisaar = sum(trafikkmengde.basisaar),
            trafikkmengde_indeksaar = sum(trafikkmengde.indeksaar),
            index_18_19 = round((trafikkmengde_indeksaar/
                             trafikkmengde_basisaar - 1) * 100,
                          digits = 1)) %>%
  rename(msnr = trs) %>%
  select(msnr, index_18_19)

# Get AADT for reference year with coverage from TD-API.
# TODO: Add coverage when available through API!
adt <- getAdtForpoints(trp_trondheim_2017$trp_id) %>%
  dplyr::filter(year == 2017) %>%
  dplyr::select(-year)

# adt <- read.csv2("adt_2017_nortraf.csv") %>%
#   filter(Felt == "R0") %>%
#   mutate(Stasjonnr = as.character(Tellepunkt),
#          ADT = round(ADT, digits = -2)) %>%
#   select(Stasjonnr, ADT)

index_converter <- function(index) {
  ifelse(
    is.na(index),
    1,
    index/100 + 1)
}

# Final table
trp_trondheim_2017_alle_adt <- trp_trondheim_2017_alle %>%
  left_join(adt) %>%
  left_join(pointindex_17_18) %>%
  left_join(pointindex_18_19) %>%
  mutate(index_17_18_i = index_converter(index_17_18),
         index_18_19_i = index_converter(index_18_19)) %>%
  # TODO: keep original columns
  mutate(index = 100 * (index_17_18_i * index_18_19_i - 1)) %>%
  mutate(index = ifelse(index == 0, NA, index)) %>%
  select(-index_17_18_i, -index_18_19_i)

# TODO: If NA all years, the result must be NA.
# Lazy solution: if equals 0, then NA!

# Must supply missing AADTs from NVDB based on road reference
missing_aadt <- trp_trondheim_2017_alle_adt %>%
  dplyr::filter(adt == 0 | is.na(adt)) %>%
  dplyr::mutate(roadref_short = str_remove_all(road_reference, " "),
                adt = mapply(getAadtByRoadReference, roadref_short)) %>%
  dplyr::select(-roadref_short)

with_aadt <- trp_trondheim_2017_alle_adt %>%
  dplyr::filter(adt > 0)

trp_trondheim_2017_final <- bind_rows(with_aadt, missing_aadt) %>%
  dplyr::arrange(road_reference)

write.csv2(trp_trondheim_2017_final,
           file = "data_indexpoints_tidy/indekspunkt_trondheim_2017.csv",
           row.names = F)

# City index
trondheim_2018 <- read.csv2("data_index_raw/byindeks_trondheim_2018.csv") %>%
  mutate(index_period = "2017-2018")
trondheim_2019 <- read.csv2("data_index_raw/byindeks_trondheim_201904.csv") %>%
  mutate(index_period = "2018-2019")
city_index_trondheim <- bind_rows(trondheim_2018, trondheim_2019) %>%
  mutate(index_i = index_converter(indeks))

city_index_trondheim_across_years <-
  100 * (prod(city_index_trondheim$index_i) - 1) %>%
  as.data.frame() %>%
  rename(indeks = 1)

city_index_trondheim_across_years$index_period <- "2017-2019"

city_index_trondheim_all <- city_index_trondheim %>%
  select(indeks, index_period) %>%
  bind_rows(city_index_trondheim_across_years)

# TODO: find the compound CI (later)

# Grenland 2017 ####
trp_grenland_2017_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Grenland",
                agreement_start == 2017) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn)

# Adding metadata
# Bruker trp fra trp-api, da noen mangler igangsetting.
trp_grenland_2017 <- dplyr::left_join(trp_grenland_2017_ids, points) %>%
  filter(!is.na(road_reference))

# Add index results from CSV-files
pointindex_grenland_18_19 <-
  read.csv2("data_index_raw/pointindex_grenland-2019-06_2018.csv") %>%
  filter(døgn == "Alle",
         lengdeklasse == "< 5,6m",
         periode == "Hittil i år") %>%
  mutate(trs = as.numeric(msnr),
         trafikkmengde.basisaar = as.numeric(
           as.character(trafikkmengde.basisår)),
         trafikkmengde.indeksaar = as.numeric(
           as.character(trafikkmengde.indeksår))) %>%
  group_by(trs) %>%
  summarise(trafikkmengde_basisaar = sum(trafikkmengde.basisaar),
            trafikkmengde_indeksaar = sum(trafikkmengde.indeksaar),
            index_18_19 = round((trafikkmengde_indeksaar/
                                   trafikkmengde_basisaar - 1) * 100,
                                digits = 1)) %>%
  rename(msnr = trs) %>%
  select(msnr, index_18_19)

adt <- getAdtForpoints(trp_grenland_2017$trp_id) %>%
  dplyr::filter(year == 2017) %>%
  dplyr::select(-year)

# Final table
trp_grenland_2017_alle_adt <- trp_grenland_2017 %>%
  left_join(adt) %>%
  #left_join(pointindex_17_18) %>%
  left_join(pointindex_grenland_18_19)

# Must supply missing AADTs from NVDB based on road reference
missing_aadt <- trp_grenland_2017_alle_adt %>%
  dplyr::filter(adt == 0 | is.na(adt)) %>%
  dplyr::mutate(roadref_short = str_remove_all(road_reference, " "),
                adt = mapply(getAadtByRoadReference, roadref_short)) %>%
  dplyr::select(-roadref_short)

with_aadt <- trp_grenland_2017_alle_adt %>%
  dplyr::filter(adt > 0)

trp_grenland_2017_final <- bind_rows(with_aadt, missing_aadt) %>%
  dplyr::arrange(road_reference)

write.csv2(trp_grenland_2017_final,
           file = "data_indexpoints_tidy/indekspunkt_grenland_2017.csv",
           row.names = F)
