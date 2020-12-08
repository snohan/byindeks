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

# NVDB API calls to get tolling stations or supply missing AADTs
source("get_from_nvdb_api.R")

# Functions
# source TAKLER IKKE Ø som brukes i kolonneoverskrift i csv-ene!
source("indexpoints_tidying_functions.R")

# Points ####

# Points used in each city
# TODO: When published index, use points from there? Must still supply unestablished ones.
cities_points <- read.csv2("data_points_raw/cities_points.csv")
cities_points_unestablished <-
  read_csv2("data_points_raw/points_unestablished.csv")

# All points from Traffic Data API
points <- get_points() %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::select(trp_id, name, road_reference, county_name,
                municipality_name, lat, lon, road_link_position) %>%
  split_road_system_reference() %>%
  dplyr::select(trp_id, name, road_reference, road_category, road_number,
                road_category_and_number,
                section_number, subsection_number, meter,
                intersection_part_number, intersection_meter,
                county_name, municipality_name, lat, lon, road_link_position)

# All points from TRP API (if needed)
points_trp <- get_points_from_trpapi_httr() %>%
  split_road_system_reference() #%>%
  # dplyr::select(trp_id, name, road_reference, road_category, road_number,
  #               road_category_and_number,
  #               section_number, subsection_number, meter,
  #               intersection_part_number, intersection_meter,
  #               county_name, municipality_name, lat, lon, road_link_position)

###
###
###

# Still need to specify csv-files




# Bergen 2017 points ####
# Using the same set of trps as with refyear 2016.
# ØKV say we must use refyear 2017 now.
this_citys_trps <- choose_city_trp_ids("Bergen", 2016) %>%
  dplyr::left_join(points) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, msnr, name, road_reference, road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position)

# Index results from CSV-files
pointindex_18 <-
  readPointindexCSV("data_index_raw/pointindex_bergen-2018-12_2017.csv") %>%
  rename(index_18 = index)

pointindex_19 <-
  readPointindexCSV("data_index_raw/pointindex_bergen-2019-12_2018.csv") %>%
  rename(index_19 = index)

pointindex_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_bergen-2020-04.csv") %>%
  rename(index_20 = index)

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20 %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

adt <- get_aadt_by_length_for_trp_list(this_citys_trps$trp_id)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2017) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("20642V805115", "25132V805616",
             "22439V804830"),
  adt = c(9000, 8800, 6800),
  year = c(2018, 2017, 2017)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

# Final table
this_citys_trp_index <- this_citys_trps %>%
  left_join(adt_all) %>%
  left_join(pointindex_18) %>%
  left_join(pointindex_19) %>%
  left_join(pointindex_20)

# Index from refyear
refyear <- this_citys_trp_index %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

this_citys_trp_index_refyear <- this_citys_trp_index %>%
  bind_cols(refyear)

# TODO: 3 year rolling index, but not now - only for the city

write.csv2(this_citys_trp_index_refyear,
           file = "data_indexpoints_tidy/indekspunkt_bergen_2017.csv",
           row.names = F)


# Bergen 2017 city ####
city_18 <-
  read_city_index_csv("data_index_raw/Bergen-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19 <-
  read_city_index_csv("data_index_raw/Bergen-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20 <-
  read_city_index_csv("data_index_raw/Bergen-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_index <- bind_rows(
  city_18,
  city_19,
  city_20) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_18,
           n_19,
           n_20))

# Accumulated index
# TODO: Functionize!
years_1_2 <- calculate_two_year_index(city_index)
years_1_3 <- bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()
# years_1_4 <- bind_rows(years_1_3, slice(city_index, 4)) %>%
#   calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index %>%
  bind_rows(years_1_3) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_bergen_2017.csv",
           row.names = F)


# Bergen 2017 monthly ####
city_18_monthly <-
  monthly_city_index("data_index_raw/Bergen-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19_monthly <-
  monthly_city_index("data_index_raw/Bergen-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20_monthly <-
  monthly_city_index("data_index_raw/Bergen-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_monthly <- bind_rows(
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_bergen_2017.csv",
           row.names = F)



# Buskerudbyen 2016 points ####
this_citys_trps <- choose_city_trp_ids("Buskerudbyen", 2016) %>%
  dplyr::left_join(points_split_reference) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, msnr, name, road_reference, road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position)

# Adding info from trp api
uncommissioned_trps <- data.frame(
  trp_id = c("95313V180820", "06687V181318"),
  msnr = c(600005, 600013),
  name = c("Bergsenga", "Rosenkrantzgata"),
  road_reference = c("E18 S46D1 m126", "FV283 S1D1 m3359"),
  road_category_and_number = c("Ev18", "Fv283"),
  county_name = c("Viken", "Viken"),
  municipality_name = c("Drammen", "Drammen"),
  lat = c(59.667946, 59.749321),
  lon = c(10.238477, 10.179452),
  road_link_position = c("0.95313@180820", "0.06687@181318")
)

this_citys_trps <- this_citys_trps %>%
  filter(!is.na(name)) %>%
  bind_rows(uncommissioned_trps)

# Add index results from CSV-files
pointindex_17 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen-2017-12_2016.csv") %>%
  rename(index_17 = index)

pointindex_18 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen-2018-12_2017.csv") %>%
  rename(index_18 = index)

pointindex_19 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen-2019-12_2018.csv") %>%
  rename(index_19 = index)

pointindex_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_buskerudbyen-2020-04.csv") %>%
  rename(index_20 = index)

n_17 <- pointindex_17 %>%
  dplyr::filter(!is.na(index_17)) %>%
  nrow()

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20 %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

adt <- get_aadt_by_length_for_trp_list(this_citys_trps$trp_id)

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

this_citys_trp_index <- this_citys_trps %>%
  left_join(adt_filtered) %>%
  left_join(pointindex_17) %>%
  left_join(pointindex_18) %>%
  left_join(pointindex_19) %>%
  left_join(pointindex_20)


# Must supply missing AADTs from NVDB based on road reference
missing_aadt <- this_citys_trp_index %>%
  dplyr::filter(adt == 0 | is.na(adt)) %>%
  dplyr::mutate(
    adt = mapply(getAadtByRoadlinkposition, road_link_position)) %>%
  dplyr::mutate(year = 2019)

with_aadt <- this_citys_trp_index %>%
  dplyr::filter(adt > 0)

this_citys_trp_index_final <- bind_rows(with_aadt, missing_aadt) %>%
  dplyr::arrange(road_reference)

# Index from refyear
refyear <- this_citys_trp_index_final %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

this_citys_trp_index_refyear <- this_citys_trp_index_final %>%
  bind_cols(refyear)

# TODO: 3 year rolling index, but not now - only for the city

write.csv2(this_citys_trp_index_refyear,
           file = "data_indexpoints_tidy/indekspunkt_buskerudbyen_2016.csv",
           row.names = F)


# Buskerudbyen 2016 city ####
city_17 <-
  read_city_index_csv("data_index_raw/Buskerudbyen-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18 <-
  read_city_index_csv("data_index_raw/Buskerudbyen-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19 <-
  read_city_index_csv("data_index_raw/Buskerudbyen-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20 <-
  read_city_index_csv("data_index_raw/Buskerudbyen-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_index <- bind_rows(
  city_17,
  city_18,
  city_19,
  city_20) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_17,
           n_18,
           n_19,
           n_20))

# Accumulated index
# TODO: Functionize!
years_1_2 <- calculate_two_year_index(city_index)
years_1_3 <- bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()
years_1_4 <- bind_rows(years_1_3, slice(city_index, 4)) %>%
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index %>%
  bind_rows(years_1_4) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_buskerudbyen_2016.csv",
           row.names = F)

# Buskerudbyen 2016 monthly ####
city_17_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_monthly <- bind_rows(
  city_17_monthly,
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_buskerudbyen_2016.csv",
           row.names = F)

# Buskerudbyen 2016 three year ####
# No use in calculating this before 37 months are available
# The first 36 month index is equal to the first three whole year index!

# TODO: 36 month rolling index with sd and ci

all_possible_36_month_indexes <-
  calculate_all_possible_36_month_indexes(city_monthly)

write.csv2(all_possible_36_month_indexes,
           file = "data_indexpoints_tidy/byindeks_36_maaneder_buskerudbyen_2016.csv",
           row.names = F)


# Buskerudbyen without E18 city ####
# Must have the points to get the n's!
n_uten_e18_17 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen_uten_e18-2017-12_2016.csv") %>%
  rename(index_16_17 = index) %>%
  dplyr::filter(!is.na(index_16_17)) %>%
  nrow()

n_uten_e18_18 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen_uten_e18-2018-12_2017.csv") %>%
  rename(index_17_18 = index) %>%
  dplyr::filter(!is.na(index_17_18)) %>%
  nrow()

n_uten_e18_19 <-
  readPointindexCSV("data_index_raw/pointindex_buskerudbyen_uten_e18-2019-12_2018.csv") %>%
  rename(index_18_19 = index) %>%
  dplyr::filter(!is.na(index_18_19)) %>%
  nrow()

n_uten_e18_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_buskerudbyen_uten_e18-2020-04.csv") %>%
  rename(index_20 = index) %>%
  dplyr::filter(!is.na(index_20)) %>%
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
buskerud_uten_e18_2020 <-
  read_city_index_csv("data_index_raw/Buskerudbyen_uten_e18-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_index <- bind_rows(buskerud_uten_e18_2017,
                                          buskerud_uten_e18_2018,
                                          buskerud_uten_e18_2019,
                                          buskerud_uten_e18_2020) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(n_uten_e18_17, n_uten_e18_18, n_uten_e18_19,
                      n_uten_e18_20))

# Accumulated index
# TODO: Functionize!
years_1_2 <- calculate_two_year_index(city_index)
years_1_3 <- bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()
years_1_4 <- bind_rows(years_1_3, slice(city_index, 4)) %>%
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index %>%
  bind_rows(years_1_4) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_buskerudbyen_uten_e18_2016.csv",
           row.names = F)


# Buskerudbyen without E18 city ####
city_17_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen_uten_e18-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen_uten_e18-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen_uten_e18-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20_monthly <-
  monthly_city_index("data_index_raw/Buskerudbyen_uten_e18-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_monthly <- bind_rows(
  city_17_monthly,
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_buskerudbyen_uten_e18_2016.csv",
           row.names = F)

# Buskerudbyen without E18 2016 three year ####
# No use in calculating this before 37 months are available
# The first 36 month index is equal to the first three whole year index!

# TODO: 36 month rolling index with sd and ci

all_possible_36_month_indexes <-
  calculate_all_possible_36_month_indexes(city_monthly)

write.csv2(all_possible_36_month_indexes,
           file = "data_indexpoints_tidy/byindeks_36_maaneder_buskerudbyen_uten_e18_2016.csv",
           row.names = F)



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


# Oslo 2017 points ####
# Basisår 2017, nytt sett punkter med basis 2019. Må ha med to sett punkter.
this_citys_trps_old <- choose_city_trp_ids("Oslo og Akershus", 2017) %>%
  dplyr::left_join(points_split_reference) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, msnr, name, road_reference, road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position)

this_citys_trps <- choose_new_city_trp_ids("Oslo og Akershus", 2019) %>%
  dplyr::left_join(points_split_reference)



# old
# oslopunkter <- cities_points %>%
#   dplyr::filter(city_area_name == "Oslo og Akershus",
#                 agreement_start == 2019) %>%
#   dplyr::mutate(established = "Ja" )

# old Adding metadata
# indekspunkter_oslo <- dplyr::left_join(oslopunkter, points) %>%
#   dplyr::select(trp_id, name, road_reference,
#                 county_name, municipality_name,
#                 lat, lon, road_link_position)

# Legger inn uetablerte punkter, mangler bare 2 per 31.03.2020
# indekspunkter_oslo_uetablerte <-
#   read.csv2("points_not_yet_established.csv") %>%
#   dplyr::filter(city_area_name == "Oslo og Akershus",
#                 agreement_start == 2019) %>%
#   dplyr::mutate(established = "Nei" )
#
# indekspunktene_oslo <- bind_rows(indekspunkter_oslo,
#                                  indekspunkter_oslo_uetablerte)
#
# write.csv2(indekspunktene_oslo,
#            file = "data_indexpoints_tidy/indekspunktene_oslo_2019.csv",
#            row.names = F)

pointindex_18 <-
  readPointindexCSV("data_index_raw/pointindex_oslo-2018-12_2017.csv") %>%
  rename(index_18 = index)

pointindex_19 <-
  readPointindexCSV("data_index_raw/pointindex_oslo-2019-12_2018.csv") %>%
  rename(index_19 = index)

pointindex_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_oslo-2020-04.csv") %>%
  rename(index_20 = index)

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20 %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

adt_old <- get_aadt_by_length_for_trp_list(this_citys_trps_old$trp_id)
adt <- get_aadt_by_length_for_trp_list(this_citys_trps$trp_id)

adt_filtered_old <- adt_old %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

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


this_citys_trp_index_old <- this_citys_trps_old %>%
  left_join(adt_filtered_old) %>%
  left_join(pointindex_18) %>%
  left_join(pointindex_19)

this_citys_trp_index <- this_citys_trps %>%
  left_join(adt_filtered) %>%
  left_join(pointindex_20)

# Fetch missing adts!
missing_adt_old <- this_citys_trp_index_old %>%
  dplyr::filter(is.na(adt)) %>%
  dplyr::mutate(adt = mapply(getAadtByRoadlinkposition, road_link_position))

missing_adt_small_cars_old <- missing_adt_old %>%
  dplyr::mutate(adt = round(0.9 * adt, digits = -2),
                year = 2019)

missing_adt <- this_citys_trp_index %>%
  dplyr::filter(is.na(adt)) %>%
  dplyr::mutate(adt = mapply(getAadtByRoadlinkposition, road_link_position))

missing_adt_small_cars <- missing_adt %>%
  dplyr::mutate(adt = round(0.9 * adt, digits = -2),
                year = 2019)

# Finally all adt
this_citys_trp_index_all_old <- this_citys_trp_index_old %>%
  dplyr::filter(!is.na(adt)) %>%
  dplyr::bind_rows(missing_adt_small_cars_old)

# %>%
#   dplyr::arrange(road_category, road_number,
#                  section_number, subsection_number, meter,
#                  intersection_part_number, intersection_meter) %>%
#   dplyr::select(trp_id, name, road_reference, road_category_and_number,
#                 county_name, municipality_name,
#                 lat, lon, road_link_position, adt, year, starts_with("index"))

this_citys_trp_index_all <- this_citys_trp_index %>%
  dplyr::filter(!is.na(adt)) %>%
  dplyr::bind_rows(missing_adt_small_cars)


# Index from refyear
 refyear_old <- this_citys_trp_index_all_old %>%
   select(starts_with("index")) %>%
   mutate_all(list(index_converter)) %>%
   transmute(index = purrr::pmap_dbl(., prod)) %>%
   # Lazily changing from 1 to NA (risky?)
   mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                        digits = 1))

 this_citys_trp_index_refyear_old <- this_citys_trp_index_all_old %>%
   bind_cols(refyear_old)

 refyear <- this_citys_trp_index_all %>%
   select(starts_with("index")) %>%
   mutate_all(list(index_converter)) %>%
   transmute(index = purrr::pmap_dbl(., prod)) %>%
   # Lazily changing from 1 to NA (risky?)
   mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                        digits = 1))

 this_citys_trp_index_refyear <- this_citys_trp_index_all %>%
   bind_cols(refyear)



# TODO: 3 year rolling index, but not now - only for the city

write.csv2(this_citys_trp_index_refyear_old,
            file = "data_indexpoints_tidy/indekspunkt_oslo_2017.csv",
            row.names = F)

write.csv2(this_citys_trp_index_refyear,
           file = "data_indexpoints_tidy/indekspunkt_oslo_2019.csv",
           row.names = F)


# Oslo 2017 city ####
city_18 <-
  read_city_index_csv("data_index_raw/Oslo-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")

city_19 <-
  read_city_index_csv("data_index_raw/Oslo-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")

city_20 <-
  read_city_index_csv("data_index_raw/Oslo-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_index <- bind_rows(
  city_18,
  city_19,
  city_20) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_18,
           n_19,
           n_20))

# Accumulated index
# TODO: Functionize!
years_1_2 <- calculate_two_year_index(city_index)
years_1_3 <- bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()
# years_1_4 <- bind_rows(years_1_3, slice(city_index, 4)) %>%
#   calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index %>%
  bind_rows(years_1_2) %>% # need 2017-2019 here
  bind_rows(years_1_3) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_oslo_2019.csv",
           row.names = F)


# Oslo 2017 monthly ####
city_18_monthly <-
  monthly_city_index("data_index_raw/Oslo-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19_monthly <-
  monthly_city_index("data_index_raw/Oslo-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20_monthly <-
  monthly_city_index("data_index_raw/Oslo-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_monthly <- bind_rows(
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_oslo_2019.csv",
           row.names = F)




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
  dplyr::mutate(station_type = "TRP") %>%
  dplyr::select(-legacyNortrafMpn)

# Endrer navn
trp_trondheim_2017$name[trp_trondheim_2017$msnr == 1600149] <- "Strindheimtunnelen"

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
# NB! 2019 includes more points than agreed

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

# ADT
adt_trp_id <- trp_trondheim_2017_alle %>%
  dplyr::filter(station_type == "TRP")

adt_trp <- getAdtForpoints_by_length(adt_trp_id$trp_id)

adt_trp_filtered <- adt_trp %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

trp_trondheim_2017_alle_adt_trp <- trp_trondheim_2017_alle %>%
  dplyr::left_join(adt_trp_filtered)

missing_adt <- trp_trondheim_2017_alle_adt_trp %>%
  dplyr::filter(is.na(adt)) %>%
  dplyr::mutate(adt = mapply(getAadtByRoadlinkposition, road_link_position))

missing_adt_small_cars <- missing_adt %>%
  dplyr::mutate(adt = round(0.9 * adt, digits = -2),
                year = 2018)

# Correcting wrong values
missing_adt_small_cars$adt[missing_adt_small_cars$trp_id == 56] <- 44000

# Finally all adt
trp_trondheim_2017_alle_adt <- trp_trondheim_2017_alle_adt_trp %>%
  dplyr::filter(!is.na(adt)) %>%
  dplyr::bind_rows(missing_adt_small_cars)

# Adding pointindices to all points
trp_trondheim_2017_alle_adt_index <- trp_trondheim_2017_alle_adt %>%
  dplyr::left_join(select(pointindex_16_17_all, 1, 4)) %>%
  dplyr::left_join(select(pointindex_17_18_all, 1, 4)) %>%
  dplyr::left_join(select(pointindex_18_19_all, 1, 4))

# Index from refyear
refyear <- trp_trondheim_2017_alle_adt_index %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

trp_trondheim_2017_alle_adt_index_final <- trp_trondheim_2017_alle_adt_index %>%
  bind_cols(refyear)

write.csv2(trp_trondheim_2017_alle_adt_index_final,
           file = "data_indexpoints_tidy/indekspunkt_trondheim_2017.csv",
           row.names = F)

# City index
# Must calculate based on all pointindices
city_index_2017 <- pointindex_16_17_all %>%
  dplyr::summarise(base_volume_all = sum(base_volume),
                   calc_volume_all = sum(calc_volume),
                   index = (calc_volume_all / base_volume_all - 1 ) * 100,
                   n_points = n(),
                   year = "2016-2017")

# To find weighted variance and ci
pointindex_16_17_all_sd <- pointindex_16_17_all %>%
  dplyr::mutate(city_index = city_index_2017$index,
                city_base_volume = city_index_2017$base_volume_all,
                diff = (base_volume / city_base_volume) *
                  (index_16_17 - city_index)^2,
                weight = (base_volume / city_base_volume)^2) %>%
  dplyr::summarise(standardavvik = sqrt((1 / (1 - sum(weight) )) * sum(diff) ))

city_index_2017_sd <- city_index_2017 %>%
  dplyr::bind_cols(pointindex_16_17_all_sd) %>%
  dplyr::mutate(variance = standardavvik^2,
                konfidensintervall = qt(0.975, n_points - 1) * standardavvik /
                  sqrt(n_points))

city_index_2018 <- pointindex_17_18_all %>%
  dplyr::summarise(base_volume_all = sum(base_volume),
                   calc_volume_all = sum(calc_volume),
                   index = (calc_volume_all / base_volume_all - 1 ) * 100,
                   n_points = n(),
                   year = "2017-2018")

pointindex_17_18_all_sd <- pointindex_17_18_all %>%
  dplyr::mutate(city_index = city_index_2018$index,
                city_base_volume = city_index_2018$base_volume_all,
                diff = (base_volume / city_base_volume) *
                  (index_17_18 - city_index)^2,
                weight = (base_volume / city_base_volume)^2) %>%
  dplyr::summarise(standardavvik = sqrt((1 / (1 - sum(weight) )) * sum(diff) ))

city_index_2018_sd <- city_index_2018 %>%
  dplyr::bind_cols(pointindex_17_18_all_sd) %>%
  dplyr::mutate(variance = standardavvik^2,
                konfidensintervall = qt(0.975, n_points - 1) * standardavvik /
                  sqrt(n_points))

city_index_2019 <- pointindex_18_19_all %>%
  dplyr::summarise(base_volume_all = sum(base_volume),
                   calc_volume_all = sum(calc_volume),
                   index = (calc_volume_all / base_volume_all - 1 ) * 100,
                   n_points = n(),
                   year = "2018-2019")

pointindex_18_19_all_sd <- pointindex_18_19_all %>%
  dplyr::mutate(city_index = city_index_2019$index,
                city_base_volume = city_index_2018$base_volume_all,
                diff = (base_volume / city_base_volume) *
                  (index_18_19 - city_index)^2,
                weight = (base_volume / city_base_volume)^2) %>%
  dplyr::summarise(standardavvik = sqrt((1 / (1 - sum(weight) )) * sum(diff) ))

city_index_2019_sd <- city_index_2019 %>%
  dplyr::bind_cols(pointindex_18_19_all_sd) %>%
  dplyr::mutate(variance = standardavvik^2,
                konfidensintervall = qt(0.975, n_points - 1) * standardavvik /
                  sqrt(n_points))


city_index <- bind_rows(city_index_2017_sd,
                        city_index_2018_sd,
                        city_index_2019_sd) %>%
  dplyr::select(-base_volume_all, -calc_volume_all) %>%
  dplyr::mutate(index_i = index_converter(index),
                # TODO: True coverage
                dekning = 100)

first_two_years <- calculate_two_year_index(city_index)
next_two_years <- bind_rows(first_two_years, slice(city_index, 3)) %>%
  calculate_two_year_index()
last_two_years <- calculate_two_year_index(slice(city_index, 2:3))

city_index_all <- city_index %>%
  bind_rows(next_two_years) %>%
  bind_rows(first_two_years) %>%
  bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_trondheim_2017.csv",
           row.names = F)

# Monthly city index
# TODO: worth it? Skip until someone asks for it! :)

# Trondheim 2019 points ####
this_citys_trps <- choose_new_city_trp_ids("Trondheim", 2019) %>%
  dplyr::left_join(points_trp) %>%
  dplyr::mutate(station_type = "Trafikkregistrering") %>%
  dplyr::select(-legacyNortrafMpn, -county_name) # %>%
  # dplyr::arrange(road_category, road_number,
  #                section_number, subsection_number, meter,
  #                intersection_part_number, intersection_meter) %>%
  # dplyr::select(trp_id, name, road_reference, road_category_and_number,
  #               county_name, municipality_name,
  #               lat, lon, road_link_position)


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
  dplyr::select(-msnr) %>%
  #dplyr::select(trp_id, name, dplyr::everything()) %>%
  split_road_system_reference() %>%
  dplyr::mutate(municipality_name = "Trondheim")

# All points
this_citys_trps_all <- bind_rows(this_citys_trps, trh_bomer) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, name, road_reference,
                road_category_and_number,
                municipality_name, lat, lon, road_link_position,
                station_type)

# Index results from CSV-files
pointindex_20 <-
  read_new_pointindex_csv_with_volumes("data_index_raw/punktindeks_trondheim-2020-04.csv") %>%
  rename(index_20 = index)

tollpointindex <- read.csv2(
  "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.csv") %>%
  dplyr::rename(trp_id = kode,
                index = indeks) %>%
  dplyr::mutate(trp_id = as.character(trp_id)) %>%
  dplyr::select(-felt, -stasjon) %>%
  dplyr::select(trp_id, base_volume, calc_volume, index, year)

pointindex_20_all <- tollpointindex %>%
  dplyr::filter(year == 2020) %>%
  dplyr::mutate(index_20 = round(index, digits = 1)) %>%
  dplyr::select(trp_id, base_volume, calc_volume, index_20) %>%
  dplyr::bind_rows(pointindex_20)

n_20 <- pointindex_20_all %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

# ADT
adt_trp_id <- this_citys_trps_all %>%
  dplyr::filter(station_type == "Trafikkregistrering")

adt_trp <- get_aadt_by_length_for_trp_list(adt_trp_id$trp_id)

adt_trp_filtered <- adt_trp %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2019) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

this_citys_trps_all_adt <- this_citys_trps_all %>%
  dplyr::left_join(adt_trp_filtered)

missing_adt <- this_citys_trps_all_adt %>%
  dplyr::filter(is.na(adt)) %>%
  dplyr::mutate(adt = mapply(getAadtByRoadlinkposition, road_link_position))

missing_adt_small_cars <- missing_adt %>%
  dplyr::mutate(adt = round(0.9 * adt, digits = -2),
                year = 2019)

# Correcting wrong values
missing_adt_small_cars$adt[missing_adt_small_cars$trp_id == 56] <- 44000

# Finally all adt
this_citys_trps_all_adt_final <- this_citys_trps_all_adt %>%
  dplyr::filter(!is.na(adt)) %>%
  dplyr::bind_rows(missing_adt_small_cars)

# Adding pointindices to all points
this_citys_trps_all_adt_final_index <- this_citys_trps_all_adt_final %>%
  dplyr::left_join(select(pointindex_20_all, 1, 4))

# Index from refyear
# Not relevant before 2021
# refyear <- this_citys_trps_all_adt_final_index %>%
#   select(starts_with("index")) %>%
#   mutate_all(list(index_converter)) %>%
#   transmute(index = purrr::pmap_dbl(., prod)) %>%
#   # Lazily changing from 1 to NA (risky?)
#   mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
#                        digits = 1))
#
# trp_trondheim_2017_alle_adt_index_final <- trp_trondheim_2017_alle_adt_index %>%
#   bind_cols(refyear)

write.csv2(this_citys_trps_all_adt_final_index,
           file = "data_indexpoints_tidy/indekspunkt_trondheim_2019.csv",
           row.names = F)


# City index
# Must calculate based on all pointindices
city_index_20 <- pointindex_20_all %>%
  dplyr::summarise(base_volume_all = sum(base_volume),
                   calc_volume_all = sum(calc_volume),
                   index = (calc_volume_all / base_volume_all - 1 ) * 100,
                   n_points = n(),
                   year = "2019-2020")

# To find weighted variance and ci
pointindex_20_all_sd <- pointindex_20_all %>%
  dplyr::filter(!is.na(index_20)) %>%
  dplyr::mutate(city_index = city_index_20$index,
                city_base_volume = city_index_20$base_volume_all,
                diff = (base_volume / city_base_volume) *
                  (index_20 - city_index)^2,
                weight = (base_volume / city_base_volume)^2) %>%
  dplyr::summarise(standardavvik = sqrt((1 / (1 - sum(weight) )) * sum(diff) ))

city_index_20_sd <- city_index_20 %>%
  dplyr::bind_cols(pointindex_20_all_sd) %>%
  dplyr::mutate(variance = standardavvik^2,
                konfidensintervall = qt(0.975, n_points - 1) * standardavvik /
                  sqrt(n_points))

city_index <- bind_rows(city_index_20_sd#,
                        #city_index_21_sd,
                        #city_index_22_sd
                        ) %>%
  dplyr::select(-base_volume_all, -calc_volume_all) %>%
  dplyr::mutate(index_i = index_converter(index),
                # TODO: True coverage
                dekning = 100)

#first_two_years <- calculate_two_year_index(city_index)
#next_two_years <- bind_rows(first_two_years, slice(city_index, 3)) %>%
#  calculate_two_year_index()
#last_two_years <- calculate_two_year_index(slice(city_index, 2:3))

city_index_all <- city_index %>%
 #bind_rows(next_two_years) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_trondheim_2019.csv",
           row.names = F)

# Monthly city index
# TODO: worth it? Skip until someone asks for it! :)



# Grenland 2016 points ####
this_citys_trps <- choose_city_trp_ids("Grenland", 2017) %>%
  dplyr::left_join(points_split_reference) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, msnr, name, road_reference, road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position)

# Bruker trp fra trp-api, da noen mangler igangsetting.
#trp_grenland_2017 <- dplyr::left_join(trp_grenland_2017_ids, points_trp) #%>%
  #filter(!is.na(road_reference))

# Bambletunnelen syd har fått utdatert veglenke - kanskje flytte den til
# 0.01@2823121. I mellomtiden:
# trp_grenland_2017$road_reference[4] <- "EV18 S26D30 m87"
# trp_grenland_2017 <- trp_grenland_2017 %>%
#   filter(!is.na(road_reference))

# Add index results from CSV-files
pointindex_17 <-
  readPointindexCSV("data_index_raw/pointindex_grenland-2017-12_2016.csv") %>%
  rename(index_17 = index)

pointindex_18 <-
  readPointindexCSV("data_index_raw/pointindex_grenland-2018-12_2017.csv") %>%
  rename(index_18 = index)

pointindex_19 <-
  readPointindexCSV("data_index_raw/pointindex_grenland-2019-12_2018.csv") %>%
  rename(index_19 = index)

pointindex_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_grenland-2020-04.csv") %>%
  rename(index_20 = index)

n_17 <- pointindex_17 %>%
  dplyr::filter(!is.na(index_17)) %>%
  nrow()

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20 %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

adt <- get_aadt_by_length_for_trp_list(this_citys_trps$trp_id)

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
  trp_id = c("26489V521174", "20789V521466"),
  adt = c(9600, 2400),
  year = c(2018, 2019)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

this_citys_trp_index <- this_citys_trps %>%
  left_join(adt_all) %>%
  left_join(pointindex_17) %>%
  left_join(pointindex_18) %>%
  left_join(pointindex_19) %>%
  left_join(pointindex_20)

# Must supply missing AADTs from NVDB based on road reference
# missing_aadt <- trp_grenland_2017_alle_adt %>%
#   dplyr::filter(adt == 0 | is.na(adt)) %>%
#   # TODO: change lookup variable to roadlinkposition
#   dplyr::mutate(#roadref_short = str_remove_all(road_reference, " "),
#                 adt = mapply(getAadtByRoadlinkposition, road_link_position))# %>%
#   #dplyr::select(-roadref_short)
#
# with_aadt <- trp_grenland_2017_alle_adt %>%
#   dplyr::filter(adt > 0)
#
# trp_grenland_2017_final <- bind_rows(with_aadt, missing_aadt) %>%
#   dplyr::arrange(road_reference) %>%
#   dplyr::select(-road_link_position, -sd)

# Index from refyear
refyear <- this_citys_trp_index %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

this_citys_trp_index_refyear <- this_citys_trp_index %>%
  bind_cols(refyear)

# TODO: 3 year rolling index, but not now - only for the city

write.csv2(this_citys_trp_index_refyear,
           file = "data_indexpoints_tidy/indekspunkt_grenland_2016.csv",
           row.names = F)


# Grenland 2016 city ####
city_17 <-
  read_city_index_csv("data_index_raw/Grenland-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18 <-
  read_city_index_csv("data_index_raw/Grenland-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19 <-
  read_city_index_csv("data_index_raw/Grenland-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20 <-
  read_city_index_csv("data_index_raw/Grenland-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_index <- bind_rows(
  city_17,
  city_18,
  city_19,
  city_20) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_17,
           n_18,
           n_19,
           n_20))

# Accumulated index
# TODO: Functionize!
years_1_2 <- calculate_two_year_index(city_index)
years_1_3 <- bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()
years_1_4 <- bind_rows(years_1_3, slice(city_index, 4)) %>%
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index %>%
  bind_rows(years_1_4) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_grenland_2016.csv",
           row.names = F)


# Grenland 2016 monthly ####
city_17_monthly <-
  monthly_city_index("data_index_raw/Grenland-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18_monthly <-
  monthly_city_index("data_index_raw/Grenland-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19_monthly <-
  monthly_city_index("data_index_raw/Grenland-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20_monthly <-
  monthly_city_index("data_index_raw/Grenland-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_monthly <- bind_rows(
  city_17_monthly,
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_grenland_2016.csv",
           row.names = F)

# Grenland 2016 three year ####
# No use in calculating this before 37 months are available
# The first 36 month index is equal to the first three whole year index!

# TODO: 36 month rolling index with sd and ci

all_possible_36_month_indexes <-
  calculate_all_possible_36_month_indexes(city_monthly)

write.csv2(all_possible_36_month_indexes,
           file = "data_indexpoints_tidy/byindeks_36_maaneder_grenland_2016.csv",
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



# Nedre Glomma 2016 points ####
this_citys_trps <- choose_city_trp_ids("Nedre Glomma", 2016) %>%
  dplyr::left_join(points_split_reference) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, msnr, name, road_reference, road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position)

# Simo has no road reference - we should support it!
this_citys_trps$road_reference[14] <- "RV110 S3D1 m2716"
this_citys_trps$road_category_and_number[14] <- "Rv110"


# Index results from CSV-files
pointindex_17 <-
  readPointindexCSV("data_index_raw/pointindex_nedre-glomma-2017-12_2016.csv") %>%
  rename(index_17 = index)

pointindex_18 <-
  readPointindexCSV("data_index_raw/pointindex_nedre-glomma-2018-12_2017.csv") %>%
  rename(index_18 = index)

pointindex_19 <-
  readPointindexCSV("data_index_raw/pointindex_nedre-glomma-2019-12_2018.csv") %>%
  rename(index_19 = index)

pointindex_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_nedre-glomma-2020-04.csv") %>%
  rename(index_20 = index)

n_17 <- pointindex_17 %>%
  dplyr::filter(!is.na(index_17)) %>%
  nrow()

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20 %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

adt <- get_aadt_by_length_for_trp_list(this_citys_trps$trp_id)

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

# Final table
this_citys_trp_index <- this_citys_trps %>%
  left_join(adt_filtered) %>%
  left_join(pointindex_17) %>%
  left_join(pointindex_18) %>%
  left_join(pointindex_19) %>%
  left_join(pointindex_20)


# Index from refyear
refyear <- this_citys_trp_index %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

this_citys_trp_index_refyear <- this_citys_trp_index %>%
  bind_cols(refyear)

# TODO: 3 year rolling index, but not now - only for the city

write.csv2(this_citys_trp_index_refyear,
           file = "data_indexpoints_tidy/indekspunkt_nedre-glomma_2016.csv",
           row.names = F)


# Nedre Glomma 2016 city ####
city_17 <-
  read_city_index_csv("data_index_raw/Nedre_Glomma-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18 <-
  read_city_index_csv("data_index_raw/Nedre_Glomma-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19 <-
  read_city_index_csv("data_index_raw/Nedre_Glomma-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20 <-
  read_city_index_csv("data_index_raw/Nedre_Glomma-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_index <- bind_rows(
  city_17,
  city_18,
  city_19,
  city_20) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_17,
           n_18,
           n_19,
           n_20))

# Accumulated index
# TODO: Functionize!
years_1_2 <- calculate_two_year_index(city_index)
years_1_3 <- bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()
years_1_4 <- bind_rows(years_1_3, slice(city_index, 4)) %>%
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index %>%
  bind_rows(years_1_4) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_nedre-glomma_2016.csv",
           row.names = F)
#
# first_two_years <- calculate_two_year_index(city_index_glomma)
# next_two_years <- bind_rows(first_two_years, slice(city_index_glomma, 3)) %>%
#   calculate_two_year_index()
# last_two_years <- calculate_two_year_index(slice(city_index_glomma, 2:3))


# Nedre Glomma 2016 monthly ####
city_17_monthly <-
  monthly_city_index("data_index_raw/Nedre_Glomma-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18_monthly <-
  monthly_city_index("data_index_raw/Nedre_Glomma-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19_monthly <-
  monthly_city_index("data_index_raw/Nedre_Glomma-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20_monthly <-
  monthly_city_index("data_index_raw/Nedre_Glomma-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_monthly <- bind_rows(
  city_17_monthly,
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_nedre_glomma_2016.csv",
           row.names = F)


# Nedre Glomma 2016 three year ####
# No use in calculating this before 37 months are available
# The first 36 month index is equal to the first three whole year index!

# TODO: 36 month rolling index with sd and ci

all_possible_36_month_indexes <-
  calculate_all_possible_36_month_indexes(city_monthly)

write.csv2(all_possible_36_month_indexes,
           file = "data_indexpoints_tidy/byindeks_36_maaneder_nedre_glomma_2016.csv",
           row.names = F)





# Nord-Jæren 2017 points ####


# trp_jaeren_2016_ids <- cities_points %>%
#   dplyr::filter(city_area_name == "Nord-Jæren",
#                 agreement_start == 2016) %>%
#   dplyr::select(trp_id, legacyNortrafMpn) %>%
#   dplyr::rename(msnr = legacyNortrafMpn)

# Adding metadata
#trp_jaeren_2016 <- dplyr::left_join(trp_jaeren_2016_ids, points)

this_citys_trps <- choose_city_trp_ids("Nord-Jæren", 2016) %>%
  dplyr::left_join(points_split_reference) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, msnr, name, road_reference, road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position)

pointindex_18 <-
  readPointindexCSV("data_index_raw/pointindex_nord-jaeren-2018-12_2017.csv") %>%
  rename(index_18 = index)

pointindex_19 <-
  readPointindexCSV("data_index_raw/pointindex_nord-jaeren-2019-12_2018.csv") %>%
  rename(index_19 = index)

pointindex_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_nord-jaeren-2020-04.csv") %>%
  rename(index_20 = index)

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20 %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

adt <- get_aadt_by_length_for_trp_list(this_citys_trps$trp_id)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2017) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)

adt_manual <- data.frame(
  trp_id = c("68351V319882"),
  adt = c(31500),
  year = c(2017)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

# Final table
this_citys_trp_index <- this_citys_trps %>%
  left_join(adt_all) %>%
  left_join(pointindex_18) %>%
  left_join(pointindex_19) %>%
  left_join(pointindex_20)

# Index from refyear
refyear <- this_citys_trp_index %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

this_citys_trp_index_refyear <- this_citys_trp_index %>%
  bind_cols(refyear)

# TODO: 3 year rolling index, but not now - only for the city

write.csv2(this_citys_trp_index_refyear,
           file = "data_indexpoints_tidy/indekspunkt_nord-jaeren_2017.csv",
           row.names = F)


# Nord-Jæren 2017 city ####
city_18 <-
  read_city_index_csv("data_index_raw/Nord-Jaeren-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19 <-
  read_city_index_csv("data_index_raw/Nord-Jaeren-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20 <-
  read_city_index_csv("data_index_raw/Nord-Jaeren-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_index <- bind_rows(
  city_18,
  city_19,
  city_20) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_18,
           n_19,
           n_20))

# Accumulated index
# TODO: Functionize!
years_1_2 <- calculate_two_year_index(city_index)
years_1_3 <- bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()
# years_1_4 <- bind_rows(years_1_3, slice(city_index, 4)) %>%
#   calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index %>%
  bind_rows(years_1_3) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_nord-jaeren_2017.csv",
           row.names = F)

# Nord-Jæren 2017 monthly ####
city_18_monthly <-
  monthly_city_index("data_index_raw/Nord-Jaeren-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19_monthly <-
  monthly_city_index("data_index_raw/Nord-Jaeren-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20_monthly <-
  monthly_city_index("data_index_raw/Nord-Jaeren-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_monthly <- bind_rows(
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_nord-jaeren_2017.csv",
           row.names = F)








# Kristiansand 2016 points ####
this_citys_trps <- choose_city_trp_ids("Kristiansand", 2017) %>%
  dplyr::left_join(points_split_reference) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, msnr, name, road_reference, road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position)

# Add index results from CSV-files
pointindex_17 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand-2017-12_2016.csv") %>%
  rename(index_17 = index)

pointindex_18 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand-2018-12_2017.csv") %>%
  rename(index_18 = index)

pointindex_19 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand-2019-12_2018.csv") %>%
  rename(index_19 = index)

pointindex_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_kristiansand-2020-04.csv") %>%
  rename(index_20 = index)

n_17 <- pointindex_17 %>%
  dplyr::filter(!is.na(index_17)) %>%
  nrow()

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20 %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()


adt <- get_aadt_by_length_for_trp_list(this_citys_trps$trp_id)

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
  trp_id = c("33412V121301", "40820V121304", "00000V1702725",
             "47254V121508"),
  adt = c(40000, 19500, 8500, 8000),
  year = c(2017, 2018, 2017, 2017)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

this_citys_trp_index <- this_citys_trps %>%
  left_join(adt_all) %>%
  left_join(pointindex_17) %>%
  left_join(pointindex_18) %>%
  left_join(pointindex_19) %>%
  left_join(pointindex_20)

# Index from refyear
refyear <- this_citys_trp_index %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

this_citys_trp_index_refyear <- this_citys_trp_index %>%
  bind_cols(refyear)

# TODO: 3 year rolling index, but not now - only for the city

write.csv2(this_citys_trp_index_refyear,
           file = "data_indexpoints_tidy/indekspunkt_kristiansand_2016.csv",
           row.names = F)


# Kristiansand 2016 city ####
city_17 <-
  read_city_index_csv("data_index_raw/Kristiansand-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18 <-
  read_city_index_csv("data_index_raw/Kristiansand-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19 <-
  read_city_index_csv("data_index_raw/Kristiansand-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20 <-
  read_city_index_csv("data_index_raw/Kristiansand-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_index <- bind_rows(
  city_17,
  city_18,
  city_19,
  city_20) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_17,
           n_18,
           n_19,
           n_20))

# Accumulated index
# TODO: Functionize!
years_1_2 <- calculate_two_year_index(city_index)
years_1_3 <- bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()
years_1_4 <- bind_rows(years_1_3, slice(city_index, 4)) %>%
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index %>%
  bind_rows(years_1_4) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_kristiansand_2016.csv",
           row.names = F)


# Kristiansand 2016 monthly ####
city_17_monthly <-
  monthly_city_index("data_index_raw/Kristiansand-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18_monthly <-
  monthly_city_index("data_index_raw/Kristiansand-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19_monthly <-
  monthly_city_index("data_index_raw/Kristiansand-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20_monthly <-
  monthly_city_index("data_index_raw/Kristiansand-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_monthly <- bind_rows(
  city_17_monthly,
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_kristiansand_2016.csv",
           row.names = F)

# Kristiansand 2016 three year ####
# No use in calculating this before 37 months are available
# The first 36 month index is equal to the first three whole year index!

# TODO: 36 month rolling index with sd and ci

all_possible_36_month_indexes <-
  calculate_all_possible_36_month_indexes(city_monthly)

write.csv2(all_possible_36_month_indexes,
           file = "data_indexpoints_tidy/byindeks_36_maaneder_kristiansand_2016.csv",
           row.names = F)




# Kristiansand kommune 2016 points ####
this_citys_trps <- choose_city_trp_ids("Kristiansand kommune", 2017) %>%
  dplyr::left_join(points_split_reference) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, msnr, name, road_reference, road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position)

# Add index results from CSV-files
pointindex_17 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand_kommune-2017-12_2016.csv") %>%
  rename(index_17 = index)

pointindex_18 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand_kommune-2018-12_2017.csv") %>%
  rename(index_18 = index)

pointindex_19 <-
  readPointindexCSV("data_index_raw/pointindex_kristiansand_kommune-2019-12_2018.csv") %>%
  rename(index_19 = index)

pointindex_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_kristiansand_kommune-2020-04.csv") %>%
  rename(index_20 = index)

n_17 <- pointindex_17 %>%
  dplyr::filter(!is.na(index_17)) %>%
  nrow()

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20 %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

adt <- get_aadt_by_length_for_trp_list(this_citys_trps$trp_id)

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
  trp_id = c("00000V1702725",
             "47254V121508"),
  adt = c(8500, 8000),
  year = c(2017, 2017)
)

adt_all <- bind_rows(adt_filtered, adt_manual)

this_citys_trp_index <- this_citys_trps %>%
  left_join(adt_all) %>%
  left_join(pointindex_17) %>%
  left_join(pointindex_18) %>%
  left_join(pointindex_19) %>%
  left_join(pointindex_20)

# Index from refyear
refyear <- this_citys_trp_index %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

this_citys_trp_index_refyear <- this_citys_trp_index %>%
  bind_cols(refyear)

# TODO: 3 year rolling index, but not now - only for the city

write.csv2(this_citys_trp_index_refyear,
           file = "data_indexpoints_tidy/indekspunkt_kristiansand_kommune_2016.csv",
           row.names = F)


# Kristiansand kommune 2016 city ####
city_17 <-
  read_city_index_csv("data_index_raw/Kristiansand_kommune-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18 <-
  read_city_index_csv("data_index_raw/Kristiansand_kommune-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19 <-
  read_city_index_csv("data_index_raw/Kristiansand_kommune-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20 <-
  read_city_index_csv("data_index_raw/Kristiansand_kommune-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_index <- bind_rows(
  city_17,
  city_18,
  city_19,
  city_20) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_17,
           n_18,
           n_19,
           n_20))

# Accumulated index
# TODO: Functionize!
years_1_2 <- calculate_two_year_index(city_index)
years_1_3 <- bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()
years_1_4 <- bind_rows(years_1_3, slice(city_index, 4)) %>%
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index %>%
  bind_rows(years_1_4) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_kristiansand_kommune_2016.csv",
           row.names = F)


# Kristiansand kommune 2016 monthly ####
city_17_monthly <-
  monthly_city_index("data_index_raw/Kristiansand_kommune-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18_monthly <-
  monthly_city_index("data_index_raw/Kristiansand_kommune-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19_monthly <-
  monthly_city_index("data_index_raw/Kristiansand_kommune-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20_monthly <-
  monthly_city_index("data_index_raw/Kristiansand_kommune-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_monthly <- bind_rows(
  city_17_monthly,
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_kristiansand_kommune_2016.csv",
           row.names = F)

# Kristiansand kommune 2016 three year ####
# No use in calculating this before 37 months are available
# The first 36 month index is equal to the first three whole year index!

# TODO: 36 month rolling index with sd and ci

all_possible_36_month_indexes <-
  calculate_all_possible_36_month_indexes(city_monthly)

write.csv2(all_possible_36_month_indexes,
           file = "data_indexpoints_tidy/byindeks_36_maaneder_kristiansand_kommune_2016.csv",
           row.names = F)



# Tromsø 2016 points ####
this_citys_trps <- choose_city_trp_ids("Tromsø", 2017) %>%
  dplyr::left_join(points_split_reference) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, msnr, name, road_reference, road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position)

# Index results from CSV-files
pointindex_17 <-
  readPointindexCSV("data_index_raw/pointindex_tromso-2017-12_2016.csv") %>%
  rename(index_17 = index)

pointindex_18 <-
  readPointindexCSV("data_index_raw/pointindex_tromso-2018-12_2017.csv") %>%
  rename(index_18 = index)

pointindex_19 <-
  readPointindexCSV("data_index_raw/pointindex_tromso-2019-12_2018.csv") %>%
  rename(index_19 = index)

pointindex_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_tromso-2020-04.csv") %>%
  rename(index_20 = index)

n_17 <- pointindex_17 %>%
  dplyr::filter(!is.na(index_17)) %>%
  nrow()

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20 %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()


adt <- get_aadt_by_length_for_trp_list(this_citys_trps$trp_id)

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
  trp_id = c("52043V1664653", "71291V1125935"),
  adt = c(15000, 10000),
  year = c(2019, 2019)
)

adt_all <- bind_rows(adt_filtered, adt_manual)


# Final table
this_citys_trp_index <- this_citys_trps %>%
  left_join(adt_all) %>%
  left_join(pointindex_17) %>%
  left_join(pointindex_18) %>%
  left_join(pointindex_19) %>%
  left_join(pointindex_20)


# Index from refyear
refyear <- this_citys_trp_index %>%
  select(starts_with("index")) %>%
  mutate_all(list(index_converter)) %>%
  transmute(index = purrr::pmap_dbl(., prod)) %>%
  # Lazily changing from 1 to NA (risky?)
  mutate(index = round(ifelse(index == 1, NA,  100 * (index - 1)),
                       digits = 1))

this_citys_trp_index_refyear <- this_citys_trp_index %>%
  bind_cols(refyear)


write.csv2(this_citys_trp_index_refyear,
           file = "data_indexpoints_tidy/indekspunkt_tromso_2016.csv",
           row.names = F)


# Tromsø 2016 city ####
city_17 <-
  read_city_index_csv("data_index_raw/Tromso-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18 <-
  read_city_index_csv("data_index_raw/Tromso-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19 <-
  read_city_index_csv("data_index_raw/Tromso-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20 <-
  read_city_index_csv("data_index_raw/Tromso-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_index <- bind_rows(
  city_17,
  city_18,
  city_19,
  city_20) %>%
  mutate(index_i = index_converter(index),
         variance = standardavvik^2,
         n_points = c(
           n_17,
           n_18,
           n_19,
           n_20))

# Accumulated index
# TODO: Functionize!
years_1_2 <- calculate_two_year_index(city_index)
years_1_3 <- bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()
years_1_4 <- bind_rows(years_1_3, slice(city_index, 4)) %>%
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index %>%
  bind_rows(years_1_4) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ki_start = index - konfidensintervall,
                ki_slutt = index + konfidensintervall)

write.csv2(city_index_all,
           file = "data_indexpoints_tidy/byindeks_tromso_2016.csv",
           row.names = F)


# Tromsø 2016 monthly ####
city_17_monthly <-
  monthly_city_index("data_index_raw/Tromso-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
city_18_monthly <-
  monthly_city_index("data_index_raw/Tromso-2018-12_2017.csv") %>%
  mutate(year = "2017-2018")
city_19_monthly <-
  monthly_city_index("data_index_raw/Tromso-2019-12_2018.csv") %>%
  mutate(year = "2018-2019")
city_20_monthly <-
  monthly_city_index("data_index_raw/Tromso-2020-04.csv") %>%
  mutate(year = "2019-2020")

city_monthly <- bind_rows(
  city_17_monthly,
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_tromso_2016.csv",
           row.names = F)

# Tromsø 2016 three year ####
# No use in calculating this before 37 months are available
# The first 36 month index is equal to the first three whole year index!

# TODO: 36 month rolling index with sd and ci

all_possible_36_month_indexes <-
  calculate_all_possible_36_month_indexes(city_monthly)

write.csv2(all_possible_36_month_indexes,
           file = "data_indexpoints_tidy/byindeks_36_maaneder_tromso_2016.csv",
           row.names = F)
#
