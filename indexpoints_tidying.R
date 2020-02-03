# Gathering info on all points and indexes
# Wrangle one city at a time!

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

# Functions
# source TAKLER IKKE Ø som brukes i kolonneoverskrift i csv-ene!
source("indexpoints_tidying_functions.R")

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

# Oslo og Akershus 2016 ####
trp_oslo_2016_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Oslo og Akershus",
                agreement_start == 2017) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn)

trp_oslo_2016 <- dplyr::left_join(trp_oslo_2016_ids, points)

# Add index results from CSV-files
pointindex_oslo_16_17 <-
  readPointindexCSV("data_index_raw/pointindex_oslo-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

n_16_17 <- pointindex_oslo_16_17 %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

pointindex_oslo_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_oslo-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- pointindex_oslo_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_oslo_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_oslo-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_oslo_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_oslo_2016$trp_id)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 20) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("29403V625517", "48379V625405", "60739V625530",
             "33808V625649", "48300V443646", "80141V444220",
             "88537V444232"),
  adt = c(11500, 53000, 22000, 52000, 20000, 51000, 72000),
  year = c(2017, 2016, 2018, 2016, 2016, 2016, 2017)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

# Final table
trp_oslo_2016_adt <- trp_oslo_2016 %>%
  left_join(adt_all) %>%
  left_join(pointindex_oslo_16_17) %>%
  left_join(pointindex_oslo_17_18) %>%
  left_join(pointindex_oslo_18_19)

# Index from refyear
refyear <- trp_oslo_2016_adt %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_oslo_2016_final <- trp_oslo_2016_adt %>%
  bind_cols(refyear)

write.csv2(trp_oslo_2016_final,
           file = "data_indexpoints_tidy/indekspunkt_oslo_2016.csv",
           row.names = F)

# City index
oslo_2017 <-
  read_city_index_csv("data_index_raw/Oslo-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
oslo_2018 <-
  read_city_index_csv("data_index_raw/Oslo-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
oslo_2019 <-
  read_city_index_csv("data_index_raw/Oslo-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_oslo <- bind_rows(
  oslo_2017,
  oslo_2018,
  oslo_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_16_17,
           n_17_18,
           n_18_19))

first_two_years <- calculate_two_year_index(city_index_oslo)
next_two_years <- bind_rows(first_two_years, slice(city_index_oslo, 3)) %>%
  calculate_two_year_index()
last_two_years <- calculate_two_year_index(slice(city_index_oslo, 2:3))

city_index_oslo_all <- city_index_oslo %>%
  bind_rows(next_two_years) %>%
  bind_rows(first_two_years) %>%
  bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_oslo_all,
           file = "data_indexpoints_tidy/byindeks_oslo_2016.csv",
           row.names = F)

# Monthly city index
oslo_2017_monthly <-
  monthly_city_index("data_index_raw/Oslo-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
oslo_2018_monthly <-
  monthly_city_index("data_index_raw/Oslo-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
oslo_2019_monthly <-
  monthly_city_index("data_index_raw/Oslo-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

oslo_monthly <- bind_rows(
  oslo_2017_monthly,
  oslo_2018_monthly,
  oslo_2019_monthly)

write.csv2(oslo_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_oslo_2016.csv",
           row.names = F)


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
# Trps
trp_trondheim_2017_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Trondheim",
                agreement_start == 2017) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn)

# Adding metadata to trps
# Bruker trp fra trp-api, da noen mangler igangsetting.
trp_trondheim_2017 <- dplyr::left_join(trp_trondheim_2017_ids, points_trp) %>%
  # Må ta vekk ene punktet i Strindheimtunnelen
  dplyr::filter(trp_id != "21571V2394246") %>%
  # Må skille trs fra bom
  dplyr::mutate(station_type = "TRS") %>%
  dplyr::select(-legacyNortrafMpn)

# Tolling stations with metadata
kommunenr <- "5001"
kommunenavn <- hent_kommune(kommunenr)[[1]]

kommune_bomer_uttak <-
  get_tolling_stations_v3(kommunenr)

kommune_bomer <- kommune_bomer_uttak %>%
  dplyr::mutate(station_type = "Bom") %>%
  dplyr::mutate(trp_id = msnr,
                msnr = as.numeric(msnr)) %>%
  dplyr::select(trp_id, everything()) %>%
  dplyr::filter(trp_id %in% c("51", "52", "53", "54", "55", "56", "58",
                              "59", "60", "61", "62", "64", "65", "66",
                              "67", "68", "69", "85", "86", "72"))

# Names from toll data files
bom_felt_og_stasjon <- read.csv2(
  "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_felt_og_stasjon.csv"
) %>%
  dplyr::select(-felt) %>%
  dplyr::rename(name = stasjon)

trh_bomer <- kommune_bomer %>%
  dplyr::select(-name) %>%
  dplyr::left_join(bom_felt_og_stasjon, by = c("msnr" = "kode")) %>%
  dplyr::select(trp_id, msnr, name, dplyr::everything())

# All indexpoints
trp_trondheim_2017_alle <-
  bind_rows(trp_trondheim_2017, trh_bomer)

# Pointindices
pointindex_16_17 <-
  read_pointindex_csv_with_volumes(
    "data_index_raw/pointindex_trondheim-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

pointindex_17_18 <-
  read_pointindex_csv_with_volumes(
    "data_index_raw/pointindex_trondheim-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

pointindex_18_19 <-
  read_pointindex_csv_with_volumes(
    "data_index_raw/pointindex_trondheim-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

tollpointindex <- read.csv2(
  "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.csv") %>%
  dplyr::rename(msnr = kode,
                index = indeks) %>%
  dplyr::select(-felt, -stasjon) %>%
  dplyr::select(msnr, base_volume, calc_volume, index, year)

pointindex_16_17_all <- tollpointindex %>%
  dplyr::filter(year == 2017) %>%
  dplyr::mutate(index_16_17 = round(index, digits = 1)) %>%
  dplyr::select(msnr, base_volume, calc_volume, index_16_17) %>%
  dplyr::bind_rows(pointindex_16_17)

pointindex_17_18_all <- tollpointindex %>%
  dplyr::filter(year == 2018) %>%
  dplyr::mutate(index_17_18 = round(index, digits = 1)) %>%
  dplyr::select(msnr, base_volume, calc_volume, index_17_18) %>%
  dplyr::bind_rows(pointindex_17_18)

pointindex_18_19_all <- tollpointindex %>%
  dplyr::filter(year == 2019) %>%
  dplyr::mutate(index_18_19 = round(index, digits = 1)) %>%
  dplyr::select(msnr, base_volume, calc_volume, index_18_19) %>%
  dplyr::bind_rows(pointindex_18_19)

# Number of points each year
n_16_17 <- pointindex_16_17_all %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

n_17_18 <- pointindex_17_18_all %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

n_18_19 <- pointindex_18_19_all %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

# Adding pointindices to all points

# HIT
# ADT

adt <- getAdtForpoints_by_length(trp_trondheim_2017$trp_id)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

# TODO: adt_bom

adt_all <- bind_rows(adt_filtered, adt_manual)

# Get AADT for reference year with coverage from TD-API.
# TODO: Add coverage when available through API!
# adt <- getAdtForpoints(trp_trondheim_2017$trp_id) %>%
#   dplyr::filter(year == 2017) %>%
#   dplyr::select(-year)

# TODO: ADD pointinfo with ADt and Read datainn and bomindeks files and bind them here

# Add index results from CSV-files
pointindex_trondheim_17_18 <-
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

n_17_18 <- pointindex_trondheim_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_trondheim_18_19 <-
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

n_18_19 <- pointindex_trondheim_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

# Final table
trp_trondheim_2017_alle_adt <- trp_trondheim_2017_alle %>%
  left_join(adt) %>%
  left_join(pointindex_trondheim_17_18) %>%
  left_join(pointindex_trondheim_18_19) %>%
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
trp_grenland_2017 <- dplyr::left_join(trp_grenland_2017_ids, points_trp) #%>%
  #filter(!is.na(road_reference))

# Bambletunnelen syd har fått utdatert veglenke - kanskje flytte den til
# 0.01@2823121. I mellomtiden:
trp_grenland_2017$road_reference[4] <- "EV18 S26D30 m87"
trp_grenland_2017 <- trp_grenland_2017 %>%
  filter(!is.na(road_reference))

# Add index results from CSV-files
pointindex_grenland_16_17 <-
  readPointindexCSV("data_index_raw/pointindex_grenland-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

n_16_17 <- pointindex_grenland_16_17 %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

pointindex_grenland_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_grenland-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- pointindex_grenland_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_grenland_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_grenland-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_grenland_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_grenland_2017$trp_id) #%>%
  #dplyr::filter(year == 2016) %>%
  #dplyr::select(-year)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 10) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("32825V521440"),
  adt = c(9000),
  year = c(2016)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

# Final table
trp_grenland_2017_alle_adt <- trp_grenland_2017 %>%
  left_join(adt_all) %>%
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
refyear <- trp_grenland_2017_alle_adt %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_grenland_2017_final_all <- trp_grenland_2017_alle_adt %>%
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
  read_city_index_csv("data_index_raw/Grenland-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_grenland <- bind_rows(grenland_2017,
                                 grenland_2018,
                                 grenland_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_16_17,
           n_17_18,
           n_18_19))

first_two_years <- calculate_two_year_index(city_index_grenland)
next_two_years <- bind_rows(first_two_years, slice(city_index_grenland, 3)) %>%
  calculate_two_year_index()
last_two_years <- calculate_two_year_index(slice(city_index_grenland, 2:3))

city_index_grenland_all <- city_index_grenland %>%
  bind_rows(next_two_years) %>%
  bind_rows(first_two_years) %>%
  bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_grenland_all,
           file = "data_indexpoints_tidy/byindeks_grenland_2017.csv",
           row.names = F)

# Monthly city index
grenland_2017_monthly <-
  monthly_city_index("data_index_raw/Grenland-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
grenland_2018_monthly <-
  monthly_city_index("data_index_raw/Grenland-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
grenland_2019_monthly <-
  monthly_city_index("data_index_raw/Grenland-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

grenland_monthly <- bind_rows(
  grenland_2017_monthly,
  grenland_2018_monthly,
  grenland_2019_monthly)

write.csv2(grenland_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_grenland_2016.csv",
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

n_16_17 <- pointindex_glomma_16_17 %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

pointindex_glomma_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_nedre-glomma-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- pointindex_glomma_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_glomma_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_nedre-glomma-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_glomma_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_glomma_2016$trp_id)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 20) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("08132V1984223"),
  adt = c(9200),
  year = c(2017)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

# Final table
trp_glomma_2016_adt <- trp_glomma_2016 %>%
  left_join(adt_all) %>%
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
  read_city_index_csv("data_index_raw/Nedre_Glomma-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_glomma <- bind_rows(
  glomma_2017,
  glomma_2018,
  glomma_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_16_17,
           n_17_18,
           n_18_19))

first_two_years <- calculate_two_year_index(city_index_glomma)
next_two_years <- bind_rows(first_two_years, slice(city_index_glomma, 3)) %>%
  calculate_two_year_index()
last_two_years <- calculate_two_year_index(slice(city_index_glomma, 2:3))

city_index_glomma_all <- city_index_glomma %>%
  bind_rows(next_two_years) %>%
  bind_rows(first_two_years) %>%
  bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

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
# pointindex_jaeren_16_17 <-
#   readPointindexCSV("data_index_raw/pointindex_nord-jaeren-2017-12_2016.csv") %>%
#   rename(index_16_17 = index)
#
# n_16_17 <- pointindex_jaeren_16_17 %>%
#   dplyr::filter(!is.na(index_16_17)) %>%
#   nrow()

pointindex_jaeren_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_nord-jaeren-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- pointindex_jaeren_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_jaeren_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_nord-jaeren-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_jaeren_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_jaeren_2016$trp_id) %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 95) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2017) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("66678V320582", "43296V319721", "21556V319919",
             "17949V320695", "67511V319880", "68351V319882"),
  adt = c(55000, 15000, 8500, 15000, 20000, 32000),
  year = c(2017, 2017, 2017, 2017, 2017, 2017)
)

adt_all <- bind_rows(adt, adt_manual)

# Final table
trp_jaeren_2016_adt <- trp_jaeren_2016 %>%
  left_join(adt_all) %>%
  #left_join(pointindex_jaeren_16_17) %>%
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
# jaeren_2017 <-
#   read_city_index_csv("data_index_raw/Nord-Jaeren-2017-12_2016.csv") %>%
#   mutate(year = "2016-2017")
jaeren_2018 <-
  read_city_index_csv("data_index_raw/Nord-Jaeren-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
jaeren_2019 <-
  read_city_index_csv("data_index_raw/Nord-Jaeren-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_jaeren <- bind_rows(#jaeren_2017,
                               jaeren_2018,
                               jaeren_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(#n_16_17,
                      n_17_18, n_18_19))

first_two_years <- calculate_two_year_index(city_index_jaeren)
# next_two_years <- bind_rows(first_two_years, slice(city_index_jaeren, 3)) %>%
#   calculate_two_year_index()

city_index_jaeren_all <- city_index_jaeren %>%
  #bind_rows(next_two_years) %>%
  bind_rows(first_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_jaeren_all,
           file = "data_indexpoints_tidy/byindeks_nord-jaeren_2016.csv",
           row.names = F)

# Monthly city index
jaeren_2018_monthly <-
  monthly_city_index("data_index_raw/Nord-Jaeren-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
jaeren_2019_monthly <-
  monthly_city_index("data_index_raw/Nord-Jaeren-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

jaeren_monthly <- bind_rows(jaeren_2018_monthly, jaeren_2019_monthly)

write.csv2(jaeren_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_nord-jaeren_2016.csv",
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
trp_buskerud_2016 <-
  dplyr::left_join(trp_buskerud_2016_ids, points_trp) %>%
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
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_buskerud_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_buskerud_2016$trp_id)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 20) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

# Final table
trp_buskerud_2016_adt <- trp_buskerud_2016 %>%
  left_join(adt_filtered) %>%
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
buskerud_2017 <-
  read_city_index_csv("data_index_raw/Buskerudbyen-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
buskerud_2018 <-
  read_city_index_csv("data_index_raw/Buskerudbyen-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
buskerud_2019 <-
  read_city_index_csv("data_index_raw/Buskerudbyen-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_buskerud <- bind_rows(buskerud_2017,
                                 buskerud_2018,
                                 buskerud_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(n_16_17, n_17_18, n_18_19))

first_two_years <- calculate_two_year_index(city_index_buskerud)
next_two_years <- bind_rows(first_two_years, slice(city_index_buskerud, 3)) %>%
  calculate_two_year_index()
last_two_years <- calculate_two_year_index(slice(city_index_buskerud, 2:3))

city_index_buskerud_all <- city_index_buskerud %>%
  bind_rows(next_two_years) %>%
  bind_rows(first_two_years) %>%
  bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_buskerud_all,
           file = "data_indexpoints_tidy/byindeks_buskerudbyen_2016.csv",
           row.names = F)

# Monthly city index
buskerud_2017_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
buskerud_2018_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
buskerud_2019_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

buskerud_monthly <- bind_rows(
  buskerud_2017_monthly,
  buskerud_2018_monthly,
  buskerud_2019_monthly)

write.csv2(buskerud_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_buskerudbyen_2016.csv",
           row.names = F)

# City index without E18
# Must have the points to get the n's!
pointindex_buskerud_uten_e18_16_17 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen_uten_e18-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

n_uten_e18_16_17 <- pointindex_buskerud_uten_e18_16_17 %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

pointindex_buskerud_uten_e18_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen_uten_e18-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_uten_e18_17_18 <- pointindex_buskerud_uten_e18_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_buskerud_uten_e18_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen_uten_e18-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

n_uten_e18_18_19 <- pointindex_buskerud_uten_e18_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

buskerud_uten_e18_2017 <-
  read_city_index_csv("data_index_raw/Buskerudbyen_uten_e18-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
buskerud_uten_e18_2018 <-
  read_city_index_csv("data_index_raw/Buskerudbyen_uten_e18-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
buskerud_uten_e18_2019 <-
  read_city_index_csv("data_index_raw/Buskerudbyen_uten_e18-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_buskerud_uten_e18 <- bind_rows(buskerud_uten_e18_2017,
                                 buskerud_uten_e18_2018,
                                 buskerud_uten_e18_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(n_uten_e18_16_17, n_uten_e18_17_18, n_uten_e18_18_19))

first_two_years_uten_e18 <- calculate_two_year_index(city_index_buskerud_uten_e18)
next_two_years_uten_e18 <-
  bind_rows(first_two_years_uten_e18,
            slice(city_index_buskerud_uten_e18, 3)) %>%
    calculate_two_year_index()
last_two_years_uten_e18 <-
  calculate_two_year_index(slice(city_index_buskerud_uten_e18, 2:3))

city_index_buskerud_uten_e18_all <- city_index_buskerud_uten_e18 %>%
  bind_rows(next_two_years_uten_e18) %>%
  bind_rows(first_two_years_uten_e18) %>%
  bind_rows(last_two_years_uten_e18) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_buskerud_uten_e18_all,
           file = "data_indexpoints_tidy/byindeks_buskerudbyen_uten_e18_2016.csv",
           row.names = F)

# Monthly city index
buskerud_uten_e18_2017_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen_uten_e18-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
buskerud_uten_e18_2018_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen_uten_e18-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
buskerud_uten_e18_2019_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen_uten_e18-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

buskerud_uten_e18_monthly <- bind_rows(
  buskerud_uten_e18_2017_monthly,
  buskerud_uten_e18_2018_monthly,
  buskerud_uten_e18_2019_monthly)

write.csv2(buskerud_uten_e18_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_buskerudbyen_uten_e18_2016.csv",
           row.names = F)

# Bergen 2016 ####
# Point index
trp_bergen_2016_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Bergen",
                agreement_start == 2016) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn) %>%
  dplyr::filter(!is.na(msnr)) %>%
  dplyr::filter(!is.na(trp_id))

# Adding metadata
trp_bergen_2016 <- dplyr::left_join(trp_bergen_2016_ids, points)

# Add index results from CSV-files
pointindex_bergen_16_17 <-
   readPointindexCSV("data_index_raw/pointindex_bergen-2017-12_2016.csv") %>%
   rename(index_16_17 = index)

n_16_17 <- pointindex_bergen_16_17 %>%
   dplyr::filter(!is.na(index_16_17)) %>%
   nrow()

pointindex_bergen_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_bergen-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- pointindex_bergen_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_bergen_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_bergen-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_bergen_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_bergen_2016$trp_id)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("20642V805115", "94767V804742",
             "13600V805722", "52794V805054"),
  adt = c(9000, 9000, 19000, 16000),
  year = c(2018, 2017, 2017, 2016)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

# Final table
trp_bergen_2016_adt <- trp_bergen_2016 %>%
  left_join(adt_all) %>%
  left_join(pointindex_bergen_16_17) %>%
  left_join(pointindex_bergen_17_18) %>%
  left_join(pointindex_bergen_18_19)

# Index from refyear
refyear <- trp_bergen_2016_adt %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_bergen_2016_final <- trp_bergen_2016_adt %>%
  bind_cols(refyear)

write.csv2(trp_bergen_2016_final,
           file = "data_indexpoints_tidy/indekspunkt_bergen_2016.csv",
           row.names = F)

# City index
bergen_2017 <-
   read_city_index_csv("data_index_raw/Bergen-2017-12_2016.csv") %>%
   mutate(year = "2016-2017")
bergen_2018 <-
  read_city_index_csv("data_index_raw/Bergen-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
bergen_2019 <-
  read_city_index_csv("data_index_raw/Bergen-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_bergen <- bind_rows(
  bergen_2017,
  bergen_2018,
  bergen_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_16_17,
           n_17_18,
           n_18_19))

first_two_years <- calculate_two_year_index(city_index_bergen)
next_two_years <- bind_rows(first_two_years, slice(city_index_bergen, 3)) %>%
   calculate_two_year_index()
last_two_years <- calculate_two_year_index(slice(city_index_bergen, 2:3))

city_index_bergen_all <- city_index_bergen %>%
  bind_rows(next_two_years) %>%
  bind_rows(first_two_years) %>%
  bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_bergen_all,
           file = "data_indexpoints_tidy/byindeks_bergen_2016.csv",
           row.names = F)

# Monthly city index
bergen_2017_monthly <-
  monthly_city_index("data_index_raw/Bergen-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
bergen_2018_monthly <-
  monthly_city_index("data_index_raw/Bergen-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
bergen_2019_monthly <-
  monthly_city_index("data_index_raw/Bergen-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

bergen_monthly <- bind_rows(
  bergen_2017_monthly,
  bergen_2018_monthly,
  bergen_2019_monthly)

write.csv2(bergen_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_bergen_2016.csv",
           row.names = F)

# Kristiansand 2016 ####
trp_krs_2016_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Kristiansand",
                agreement_start == 2017) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn) %>%
  dplyr::filter(!is.na(msnr)) %>%
  dplyr::filter(!is.na(trp_id))

# Adding metadata
trp_krs_2016 <- dplyr::left_join(trp_krs_2016_ids, points)

# Add index results from CSV-files
pointindex_krs_16_17 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

n_16_17 <- pointindex_krs_16_17 %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

pointindex_krs_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- pointindex_krs_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_krs_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_krs_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_krs_2016$trp_id)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("33412V121301", "43920V121762", "45924V1955584",
             "47254V121508"),
  adt = c(40000, 3000, 21500, 8000),
  year = c(2017, 2016, 2017, 2017)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

# Final table
trp_krs_2016_adt <- trp_krs_2016 %>%
  left_join(adt_all) %>%
  left_join(pointindex_krs_16_17) %>%
  left_join(pointindex_krs_17_18) %>%
  left_join(pointindex_krs_18_19)

# Index from refyear
refyear <- trp_krs_2016_adt %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_krs_2016_final <- trp_krs_2016_adt %>%
  bind_cols(refyear)

write.csv2(trp_krs_2016_final,
           file = "data_indexpoints_tidy/indekspunkt_kristiansand_2016.csv",
           row.names = F)

# City index
krs_2017 <-
  read_city_index_csv("data_index_raw/Kristiansand-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
krs_2018 <-
  read_city_index_csv("data_index_raw/Kristiansand-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
krs_2019 <-
  read_city_index_csv("data_index_raw/Kristiansand-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_krs <- bind_rows(
  krs_2017,
  krs_2018,
  krs_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_16_17,
           n_17_18,
           n_18_19))

first_two_years <- calculate_two_year_index(city_index_krs)
next_two_years <- bind_rows(first_two_years, slice(city_index_krs, 3)) %>%
  calculate_two_year_index()
last_two_years <- calculate_two_year_index(slice(city_index_krs, 2:3))

city_index_krs_all <- city_index_krs %>%
  bind_rows(next_two_years) %>%
  bind_rows(first_two_years) %>%
  bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_krs_all,
           file = "data_indexpoints_tidy/byindeks_kristiansand_2016.csv",
           row.names = F)

# Monthly city index
krs_2017_monthly <-
  monthly_city_index("data_index_raw/Kristiansand-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
krs_2018_monthly <-
  monthly_city_index("data_index_raw/Kristiansand-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
krs_2019_monthly <-
  monthly_city_index("data_index_raw/Kristiansand-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

krs_monthly <- bind_rows(
  krs_2017_monthly,
  krs_2018_monthly,
  krs_2019_monthly)

write.csv2(krs_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_kristiansand_2016.csv",
           row.names = F)


# Kristiansand kommune 2016 ####
trp_krs_kommune_2016_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Kristiansand kommune",
                agreement_start == 2017) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn) %>%
  dplyr::filter(!is.na(msnr)) %>%
  dplyr::filter(!is.na(trp_id))

# Adding metadata
trp_krs_kommune_2016 <- dplyr::left_join(trp_krs_kommune_2016_ids, points)

# Add index results from CSV-files
pointindex_krs_kommune_16_17 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand_kommune-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

n_16_17 <- pointindex_krs_kommune_16_17 %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

pointindex_krs_kommune_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand_kommune-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- pointindex_krs_kommune_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_krs_kommune_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand_kommune-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_krs_kommune_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_krs_kommune_2016$trp_id)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("45924V1955584",
             "47254V121508"),
  adt = c(21500, 8000),
  year = c(2017, 2017)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

# Final table
trp_krs_kommune_2016_adt <- trp_krs_kommune_2016 %>%
  left_join(adt_all) %>%
  left_join(pointindex_krs_kommune_16_17) %>%
  left_join(pointindex_krs_kommune_17_18) %>%
  left_join(pointindex_krs_kommune_18_19)

# Index from refyear
refyear <- trp_krs_kommune_2016_adt %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_krs_kommune_2016_final <- trp_krs_kommune_2016_adt %>%
  bind_cols(refyear)

write.csv2(trp_krs_kommune_2016_final,
           file = "data_indexpoints_tidy/indekspunkt_kristiansand_kommune_2016.csv",
           row.names = F)

# City index
krs_kommune_2017 <-
  read_city_index_csv("data_index_raw/Kristiansand_kommune-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
krs_kommune_2018 <-
  read_city_index_csv("data_index_raw/Kristiansand_kommune-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
krs_kommune_2019 <-
  read_city_index_csv("data_index_raw/Kristiansand_kommune-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_krs_kommune <- bind_rows(
  krs_kommune_2017,
  krs_kommune_2018,
  krs_kommune_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_16_17,
           n_17_18,
           n_18_19))

first_two_years <- calculate_two_year_index(city_index_krs_kommune)
next_two_years <- bind_rows(first_two_years, slice(city_index_krs_kommune, 3)) %>%
  calculate_two_year_index()
last_two_years <- calculate_two_year_index(slice(city_index_krs_kommune, 2:3))

city_index_krs_kommune_all <- city_index_krs_kommune %>%
  bind_rows(next_two_years) %>%
  bind_rows(first_two_years) %>%
  bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_krs_kommune_all,
           file = "data_indexpoints_tidy/byindeks_kristiansand_kommune_2016.csv",
           row.names = F)

# Monthly city index
krs_kommune_2017_monthly <-
  monthly_city_index("data_index_raw/Kristiansand_kommune-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
krs_kommune_2018_monthly <-
  monthly_city_index("data_index_raw/Kristiansand_kommune-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
krs_kommune_2019_monthly <-
  monthly_city_index("data_index_raw/Kristiansand_kommune-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

krs_kommune_monthly <- bind_rows(
  krs_kommune_2017_monthly,
  krs_kommune_2018_monthly,
  krs_kommune_2019_monthly)

write.csv2(krs_kommune_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_kristiansand_kommune_2016.csv",
           row.names = F)

# Tromsø 2016 ####
# Point index
trp_tromso_2016_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Tromsø",
                agreement_start == 2017) %>%
  dplyr::select(trp_id, legacyNortrafMpn) %>%
  dplyr::rename(msnr = legacyNortrafMpn) %>%
  dplyr::filter(!is.na(msnr)) %>%
  dplyr::filter(!is.na(trp_id))

# Adding metadata
trp_tromso_2016 <- dplyr::left_join(trp_tromso_2016_ids, points)

# Add index results from CSV-files
pointindex_tromso_16_17 <-
  readPointindexCSV("data_index_raw/pointindex_tromso-2017-12_2016.csv") %>%
  rename(index_16_17 = index)

n_16_17 <- pointindex_tromso_16_17 %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

pointindex_tromso_17_18 <-
  readPointindexCSV("data_index_raw/pointindex_tromso-2018-12_2017.csv") %>%
  rename(index_17_18 = index)

n_17_18 <- pointindex_tromso_17_18 %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

pointindex_tromso_18_19 <-
  readPointindexCSV("data_index_raw/pointindex_tromso-2019-12_2018.csv") %>%
  rename(index_18_19 = index)

n_18_19 <- pointindex_tromso_18_19 %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

adt <- getAdtForpoints_by_length(trp_tromso_2016$trp_id)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("52043V1664653", "49212V1126027", "71291V1125935"),
  adt = c(15000, 12000, 10000),
  year = c(2019, 2017, 2019)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

# Final table
trp_tromso_2016_adt <- trp_tromso_2016 %>%
  left_join(adt_all) %>%
  left_join(pointindex_tromso_16_17) %>%
  left_join(pointindex_tromso_17_18) %>%
  left_join(pointindex_tromso_18_19)

# Index from refyear
refyear <- trp_tromso_2016_adt %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_tromso_2016_final <- trp_tromso_2016_adt %>%
  bind_cols(refyear)

write.csv2(trp_tromso_2016_final,
           file = "data_indexpoints_tidy/indekspunkt_tromso_2016.csv",
           row.names = F)

# City index
tromso_2017 <-
  read_city_index_csv("data_index_raw/Tromso-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
tromso_2018 <-
  read_city_index_csv("data_index_raw/Tromso-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
tromso_2019 <-
  read_city_index_csv("data_index_raw/Tromso-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_index_tromso <- bind_rows(
  tromso_2017,
  tromso_2018,
  tromso_2019) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_16_17,
           n_17_18,
           n_18_19))

first_two_years <- calculate_two_year_index(city_index_tromso)
next_two_years <- bind_rows(first_two_years, slice(city_index_tromso, 3)) %>%
  calculate_two_year_index()
last_two_years <- calculate_two_year_index(slice(city_index_tromso, 2:3))

city_index_tromso_all <- city_index_tromso %>%
  bind_rows(next_two_years) %>%
  bind_rows(first_two_years) %>%
  bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_tromso_all,
           file = "data_indexpoints_tidy/byindeks_tromso_2016.csv",
           row.names = F)

# Monthly city index
tromso_2017_monthly <-
  monthly_city_index("data_index_raw/Tromso-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
tromso_2018_monthly <-
  monthly_city_index("data_index_raw/Tromso-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
tromso_2019_monthly <-
  monthly_city_index("data_index_raw/Tromso-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

tromso_monthly <- bind_rows(
  tromso_2017_monthly,
  tromso_2018_monthly,
  tromso_2019_monthly)

write.csv2(tromso_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_tromso_2016.csv",
           row.names = F)
#
