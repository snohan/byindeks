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

# Functions ####

readPointindexCSV <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename) %>%
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
              index = round((trafikkmengde_indeksaar/
                                  trafikkmengde_basisaar - 1) * 100,
                                  digits = 1)) %>%
    rename(msnr = trs) %>%
    select(msnr, index)
}

read_bikepointindex_csv <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename) %>%
    filter(døgn == "Alle",
           periode == "Hittil i år") %>%
    mutate(msnr = as.numeric(msnr),
           index = as.numeric(str_replace(indeks, ",", "."))) %>%
    select(msnr, index)
}

read_city_index_csv <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename) %>%
    filter(Vegkategori == "E+R+F+K",
           døgn == "Alle",
           lengdeklasse == "< 5,6m",
           periode == "Hittil i år") %>%
    mutate(index = as.numeric(str_replace(indeks, ",", ".")),
           dekning = as.numeric(str_replace(dekning, ",", ".")),
           standardavvik = as.numeric(as.character(standardavvik)),
           konfidensintervall = as.numeric(as.character(konfidensintervall))) %>%
    select(index, dekning, standardavvik, konfidensintervall) %>%
    as_tibble()
}

read_bike_index_csv <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename) %>%
    filter(Vegkategori == "E+R+F+K",
           døgn == "Alle",
           lengdeklasse == "Alle",
           periode == "Hittil i år") %>%
    mutate(index = as.numeric(str_replace(indeks, ",", ".")),
           dekning = as.numeric(str_replace(dekning, ",", ".")),
           standardavvik = as.numeric(as.character(standardavvik)),
           konfidensintervall = as.numeric(as.character(konfidensintervall))) %>%
    select(index, dekning, standardavvik, konfidensintervall) %>%
    as_tibble()
}


index_converter <- function(index) {
  ifelse(
    is.na(index),
    1,
    index/100 + 1)
}

# TODO: get a compound ci, need to iterate pairwise through the years!
# I.e. make accumulated index for one more year
#index_from_refyear <- 100*(prod(city_index_grenland$index_i)-1)

# Need number of points each year
calculate_two_year_index <- function(city_index_df) {

  two_years <- city_index_df %>%
    select(index, index_i, dekning, variance, n_points) %>%
    slice(1:2)

  year_one <- str_sub(city_index_df$year[1], 1, 4)
  year_two <- str_sub(city_index_df$year[2], 6, 9)

  two_years_to_one <- list(
    index = 100 * (prod(two_years$index_i) - 1),
    index_i = prod(two_years$index_i),
    year = paste0(year_one, "-", year_two),
    dekning = mean(two_years$dekning),
    # Using Goodman's unbiased estimate (cannot use exact formula as we are
    # sampling)
    variance =
      two_years$index[1]^2 * two_years$variance[2] / two_years$n_points[2] +
      two_years$index[2]^2 * two_years$variance[1] / two_years$n_points[1] -
      two_years$variance[1] * two_years$variance[2] /
      (two_years$n_points[1] * two_years$n_points[2]),
    n_points = max(two_years$n_points)
  ) %>%
    as_tibble() %>%
    dplyr::mutate(standardavvik = sqrt(variance),
                  konfidensintervall = 1.96 * sqrt(variance) /
                    sqrt(2))
}

# Points ####

# Points used in each city
cities_points <- read.csv2("data_points_raw/cities_points.csv")
cities_points_unestablished <-
  read_csv2("data_points_raw/points_unestablished.csv")

# All points from Traffic Data API
points <- getPoints() %>%
  #dplyr::select(trp_id, name, road_reference, lat, lon) %>%
  dplyr::distinct(trp_id, .keep_all = T)

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
# Point index
trp_grenland_2017_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Grenland",
                agreement_start == 2017) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn)

# Adding metadata
# Bruker trp fra trp-api, da noen mangler igangsetting.
trp_grenland_2017 <- dplyr::left_join(trp_grenland_2017_ids, points_trp) %>%
  filter(!is.na(road_reference))

# Add index results from CSV-files
pointindex_grenland_16_17 <-
  readPointindexCSV("data_index_raw/pointindex_grenland-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

pointindex_grenland_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_grenland-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

pointindex_grenland_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_grenland-2019-06_2018.csv") %>%
  rename(index_18_19 = index)

adt <- getAdtForpoints(trp_grenland_2017$trp_id) %>%
  dplyr::filter(year == 2016) %>%
  dplyr::select(-year)

# Final table
trp_grenland_2017_alle_adt <- trp_grenland_2017 %>%
  left_join(adt) %>%
  left_join(pointindex_grenland_16_17) %>%
  left_join(pointindex_grenland_17_18) %>%
  left_join(pointindex_grenland_18_19)

# Must supply missing AADTs from NVDB based on road reference
missing_aadt <- trp_grenland_2017_alle_adt %>%
  dplyr::filter(adt == 0 | is.na(adt)) %>%
  # TODO: change lookup variable to roadlinkposition
  dplyr::mutate(#roadref_short = str_remove_all(road_reference, " "),
                adt = mapply(getAadtByRoadlinkposition, road_link_position))# %>%
  #dplyr::select(-roadref_short)

with_aadt <- trp_grenland_2017_alle_adt %>%
  dplyr::filter(adt > 0)

trp_grenland_2017_final <- bind_rows(with_aadt, missing_aadt) %>%
  dplyr::arrange(road_reference) %>%
  dplyr::select(-road_link_position, -sd)

# Index from refyear
refyear <- trp_grenland_2017_final %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_grenland_2017_final_all <- trp_grenland_2017_final %>%
  bind_cols(refyear)

write.csv2(trp_grenland_2017_final_all,
           file = "data_indexpoints_tidy/indekspunkt_grenland_2017.csv",
           row.names = F)

# City index
grenland_2017 <-
  read_city_index_csv("data_index_raw/Grenland-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
grenland_2018 <-
  read_city_index_csv("data_index_raw/Grenland-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
grenland_2019 <-
  read_city_index_csv("data_index_raw/Grenland-2019-06_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_grenland <- bind_rows(grenland_2017,
                                 grenland_2018,
                                 grenland_2019) %>%
  mutate(index_i = index_converter(index),
         variance = (konfidensintervall / 1.96)^2)

first_two_years <- calculate_two_year_index(city_index_grenland)
next_two_years <- bind_rows(first_two_years, slice(city_index_grenland, 3)) %>%
  calculate_two_year_index() %>%
  mutate(dekning = mean(city_index_grenland$dekning),
         year = "2016-2019",
         konfidensintervall = 1.96 * sqrt(variance))

city_index_grenland_all <- city_index_grenland %>%
  select(-standardavvik) %>%
  bind_rows(next_two_years)

write.csv2(city_index_grenland_all,
           file = "data_indexpoints_tidy/byindeks_grenland_2017.csv",
           row.names = F)

# Grenland sykkel 2016 ####
# Point index
bike_trp_grenland_2016_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Grenland sykkel",
                agreement_start == 2016) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn)

bike_trp_grenland_2016 <- dplyr::left_join(bike_trp_grenland_2016_ids,
                                           points_trp) %>%
  filter(!is.na(road_reference))

# Add index results from CSV-files
bike_pointindex_grenland_16_17 <-
  read_bikepointindex_csv(
    "data_index_raw/bikepointindex_grenland-2017-12_2016-12.csv") %>%
  rename(index_16_17 = index)

bike_pointindex_grenland_17_18 <-
  read_bikepointindex_csv(
    "data_index_raw/bikepointindex_grenland-2018-12_2017-12.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- bike_pointindex_grenland_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

bike_pointindex_grenland_18_19 <-
  read_bikepointindex_csv(
    "data_index_raw/bikepointindex_grenland-2019-06_2018-06.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- bike_pointindex_grenland_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints(bike_trp_grenland_2016$trp_id) %>%
  dplyr::filter(year >= 2017) %>%
  dplyr::filter(coverage > 90) %>%
  dplyr::group_by(trp_id) %>%
  # Pick the oldest AADT available:
  dplyr::slice(which.min(year))

# Missing AADTs: no bike-AADTs in NVDB where we don't have it in TD-API!
# Must manually add missing ones by looking up in Kibana.
adt_manual <- data.frame(
  trp_id = c("57192B1687248", "57692B1981530", "21538B1865084",
             "67987B1846201", "40000B1846336", "47211B491325"),
  sd = c(NA, NA, NA, NA, NA, NA),
  adt = c(80, 15, 80, 150, 60, 80),
  year = c(rep.int(2017, 6))
)

adt_all <- bind_rows(adt, adt_manual)

bike_trp_grenland_2016_all_adt <- bike_trp_grenland_2016 %>%
  left_join(adt_all) %>%
#  left_join(bike_pointindex_grenland_16_17) %>%
  left_join(bike_pointindex_grenland_17_18) %>%
  left_join(bike_pointindex_grenland_18_19)

# Index from refyear
refyear <- bike_trp_grenland_2016_all_adt %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

bike_trp_grenland_2016_final <- bike_trp_grenland_2016_all_adt %>%
  bind_cols(refyear)

write.csv2(bike_trp_grenland_2016_final,
           file = "data_indexpoints_tidy/sykkelindekspunkt_grenland_2016.csv",
           row.names = F)

# City index
grenland_bike_2017 <-
  read_bike_index_csv("data_index_raw/bike_Grenland-2017-12_2016-12.csv") %>%
  mutate(year = "2016-2017")
grenland_bike_2018 <-
  read_bike_index_csv("data_index_raw/bike_Grenland-2018-12_2017-12.csv") %>%
  mutate(year = "2017-2018")
grenland_bike_2019 <-
  read_bike_index_csv("data_index_raw/bike_Grenland-2019-06_2018-06.csv") %>%
  mutate(year = "2018-2019")

bike_index_grenland <- bind_rows(#grenland_bike_2017,
                                 grenland_bike_2018,
                                 grenland_bike_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(n_17_18, n_18_19))

# TODO: get a compound ci
# Skipping 2016
# first_two_years <- calculate_two_year_index(bike_index_grenland)
# next_two_years <- bind_rows(first_two_years, slice(bike_index_grenland, 3)) %>%
#   calculate_two_year_index() %>%
#   mutate(dekning = mean(bike_index_grenland$dekning),
#          year = "2016-2019",
#          konfidensintervall = 1.96 * sqrt(variance))

first_two_years <- calculate_two_year_index(bike_index_grenland)

bike_index_grenland_all <- bike_index_grenland %>%
  dplyr::bind_rows(first_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(bike_index_grenland_all,
           file = "data_indexpoints_tidy/sykkelindeks_grenland_2016.csv",
           row.names = F)

# Nedre Glomma 2016 ####
# Point index
trp_glomma_2016_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Nedre Glomma",
                agreement_start == 2016) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn)

# Adding metadata
trp_glomma_2016 <- dplyr::left_join(trp_glomma_2016_ids, points)

# Add index results from CSV-files
pointindex_glomma_16_17 <-
  readPointindexCSV("data_index_raw/pointindex_nedre-glomma-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

pointindex_glomma_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_nedre-glomma-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

pointindex_glomma_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_nedre-glomma-2019-08_2018.csv") %>%
  rename(index_18_19 = index)

adt <- getAdtForpoints_by_length(trp_glomma_2016$trp_id) %>%
  dplyr::filter(year == 2016) %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::select(trp_id, aadt_length_range) %>%
  dplyr::rename(adt = 2)

# Final table
trp_glomma_2016_adt <- trp_glomma_2016 %>%
  left_join(adt) %>%
  left_join(pointindex_glomma_16_17) %>%
  left_join(pointindex_glomma_17_18) %>%
  left_join(pointindex_glomma_18_19)

# Index from refyear
refyear <- trp_glomma_2016_adt %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_glomma_2016_final <- trp_glomma_2016_adt %>%
  bind_cols(refyear)

write.csv2(trp_glomma_2016_final,
           file = "data_indexpoints_tidy/indekspunkt_nedre-glomma_2016.csv",
           row.names = F)

# City index
glomma_2017 <-
  read_city_index_csv("data_index_raw/Nedre_Glomma-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
glomma_2018 <-
  read_city_index_csv("data_index_raw/Nedre_Glomma-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
glomma_2019 <-
  read_city_index_csv("data_index_raw/Nedre_Glomma-2019-08_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_glomma <- bind_rows(glomma_2017,
                               glomma_2018,
                               glomma_2019) %>%
  mutate(index_i = index_converter(index),
         variance = (konfidensintervall / 1.96)^2)

first_two_years <- calculate_two_year_index(city_index_glomma)
next_two_years <- bind_rows(first_two_years, slice(city_index_glomma, 3)) %>%
  calculate_two_year_index() %>%
  mutate(dekning = mean(city_index_glomma$dekning),
         year = "2016-2019",
         konfidensintervall = 1.96 * sqrt(variance))

city_index_glomma_all <- city_index_glomma %>%
  select(-standardavvik) %>%
  bind_rows(next_two_years)

write.csv2(city_index_glomma_all,
           file = "data_indexpoints_tidy/byindeks_nedre-glomma_2016.csv",
           row.names = F)

# Nord-Jæren 2016 ####
# Point index
trp_jaeren_2016_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Nord-Jæren",
                agreement_start == 2016) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn)

# Adding metadata
trp_jaeren_2016 <- dplyr::left_join(trp_jaeren_2016_ids, points)

# Add index results from CSV-files
pointindex_jaeren_16_17 <-
  readPointindexCSV("data_index_raw/pointindex_nord-jaeren-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

n_16_17 <- pointindex_jaeren_16_17 %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

pointindex_jaeren_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_nord-jaeren-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- pointindex_jaeren_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_jaeren_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_nord-jaeren-2019-08_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_jaeren_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_jaeren_2016$trp_id) %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 95) %>%
  dplyr::filter(coverage > 90) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("66678V320582", "43296V319721", "21556V319919",
             "17949V320695"),
  adt = c(55000, 15000, 8700, 15000),
  year = c(2017, 2016, 2016, 2016)
)

adt_all <- bind_rows(adt, adt_manual)

# Final table
trp_jaeren_2016_adt <- trp_jaeren_2016 %>%
  left_join(adt_all) %>%
  left_join(pointindex_jaeren_16_17) %>%
  left_join(pointindex_jaeren_17_18) %>%
  left_join(pointindex_jaeren_18_19)

# Index from refyear
refyear <- trp_jaeren_2016_adt %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_jaeren_2016_final <- trp_jaeren_2016_adt %>%
  bind_cols(refyear)

write.csv2(trp_jaeren_2016_final,
           file = "data_indexpoints_tidy/indekspunkt_nord-jaeren_2016.csv",
           row.names = F)

# City index
jaeren_2017 <-
  read_city_index_csv("data_index_raw/Nord-Jaeren-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
jaeren_2018 <-
  read_city_index_csv("data_index_raw/Nord-Jaeren-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
jaeren_2019 <-
  read_city_index_csv("data_index_raw/Nord-Jaeren-2019-08_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_jaeren <- bind_rows(jaeren_2017,
                               jaeren_2018,
                               jaeren_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(n_16_17, n_17_18, n_18_19))

first_two_years <- calculate_two_year_index(city_index_jaeren)
next_two_years <- bind_rows(first_two_years, slice(city_index_jaeren, 3)) %>%
  calculate_two_year_index()

city_index_jaeren_all <- city_index_jaeren %>%
  bind_rows(next_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_jaeren_all,
           file = "data_indexpoints_tidy/byindeks_nord-jaeren_2016.csv",
           row.names = F)


# Buskerudbyen 2016 ####
# Point index
trp_buskerud_2016_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Buskerudbyen",
                agreement_start == 2016) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn)

# Adding metadata
# Bruker trp fra trp-api, da noen mangler igangsetting.
trp_buskerud_2016 <- dplyr::left_join(trp_buskerud_2016_ids, points_trp) %>%
  filter(!is.na(road_reference))

# Add index results from CSV-files
pointindex_buskerud_16_17 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

n_16_17 <- pointindex_buskerud_16_17 %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

pointindex_buskerud_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- pointindex_buskerud_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_buskerud_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen-2019-09_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_buskerud_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_buskerud_2016$trp_id) %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

# Final table
trp_buskerud_2016_adt <- trp_buskerud_2016 %>%
  left_join(adt) %>%
  left_join(pointindex_buskerud_16_17) %>%
  left_join(pointindex_buskerud_17_18) %>%
  left_join(pointindex_buskerud_18_19)

# Must supply missing AADTs from NVDB based on road reference
missing_aadt <- trp_buskerud_2016_adt %>%
  dplyr::filter(adt == 0 | is.na(adt)) %>%
  dplyr::mutate(
    adt = mapply(getAadtByRoadlinkposition, road_link_position)) %>%
  dplyr::mutate(year = 2018)

with_aadt <- trp_buskerud_2016_adt %>%
  dplyr::filter(adt > 0)

trp_buskerud_2016_adt_final <- bind_rows(with_aadt, missing_aadt) %>%
  dplyr::arrange(road_reference)

# Index from refyear
refyear <- trp_buskerud_2016_adt_final %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_buskerud_2016_adt_final_all <- trp_buskerud_2016_adt_final %>%
  bind_cols(refyear)

write.csv2(trp_buskerud_2016_adt_final_all,
           file = "data_indexpoints_tidy/indekspunkt_buskerudbyen_2016.csv",
           row.names = F)

# City index
