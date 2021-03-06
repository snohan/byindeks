# Preparation of data for reporting
# Gathering info on all points and indexes and writing them to csv

# Setup ####
# Packages are to be loaded through sourcing rmd_setup.R in the Rmd report file.
source("rmd_setup.R")
source("get_from_trafficdata_api.R")

# If necessary, get all TRPs from TRP API
# TRPs without commissions are not i TD-API!
#source("get_from_trp_api.R")

# NVDB API calls to get tolling stations or supply missing AADTs
source("get_from_nvdb_api.R")

# Functions
# source TAKLER IKKE Ø som brukes i kolonneoverskrift i csv-ene!
source("indexpoints_tidying_functions.R")



# Points ####

# Points used in each city:
cities_points <- read.csv2("data_points_raw/cities_points.csv")

# Connection to old data:
trp_id_msnr <- cities_points %>%
  dplyr::select(trp_id, msnr = legacyNortrafMpn) %>%
  dplyr::distinct()

# Shouldn't be necessary:
#cities_points_unestablished <-
#  read_csv2("data_points_raw/points_unestablished.csv")

# All points from Traffic Data API:
points <- get_points() %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::select(trp_id, name, road_reference, county_name,
                municipality_name, lat, lon, road_link_position) %>%
  split_road_system_reference() %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no")) %>%
  dplyr::select(trp_id, name, road_reference, road_category, road_number,
                road_category_and_number,
                section_number, subsection_number, meter,
                intersection_part_number, intersection_meter,
                county_name, municipality_name, lat, lon, road_link_position)


# All points from TRP API (if needed):
#points_trp <- get_points_from_trpapi_httr() %>%
#  split_road_system_reference() #%>%
# dplyr::select(trp_id, name, road_reference, road_category, road_number,
#               road_category_and_number,
#               section_number, subsection_number, meter,
#               intersection_part_number, intersection_meter,
#               county_name, municipality_name, lat, lon, road_link_position)



# City numbers
# Bergen 8952
# Buskerudbyen 1952
# Grenland 955
# Kristiansand og omegn 957 kommune 956
# Nedre Glomma 953
# Nord-Jæren 952
# Oslo 959
# Trondheim 960
# Tromsø 961

# Choose
index_month <- 12
city_number <- 956

# Pointindices ####
# TODO: TRPs might differ from year to year!

# Fetch city indexes
# Note: not all cities use 2017
city_index_2017 <- get_published_index_for_months(city_number, 2017, 12)
city_index_2018 <- get_published_index_for_months(city_number, 2018, 12)
city_index_2019 <- get_published_index_for_months(city_number, 2019, 12)
city_index_2020 <- get_published_index_for_months(city_number, 2020, index_month)


# Still need to specify csv-files for years before 2020 to get the pointindex as they are not in API
# Note: not all cities use 2017
pointindex_17 <- readPointindexCSV(
  paste0("data_index_raw/pointindex_", city_number, "_", 2017, ".csv")
) %>%
  rename(index_17 = index)

pointindex_18 <- readPointindexCSV(
  paste0("data_index_raw/pointindex_", city_number, "_", 2018, ".csv")
  ) %>%
  rename(index_18 = index)

pointindex_19 <- readPointindexCSV(
  paste0("data_index_raw/pointindex_", city_number, "_", 2019, ".csv")
  ) %>%
  rename(index_19 = index)

# Bergen
pointindex_19_all <- get_published_pointindex_for_months(city_number, 2019, index_month)

pointindex_19 <- pointindex_19_all[[2]] %>%
  dplyr::filter(day_type == "ALL",
                is_excluded == FALSE,
                is_manually_excluded == FALSE,
                length_excluded == FALSE,
                period == "year_to_date",
                month == index_month) %>%
  dplyr::select(trp_id, #base_volume, calc_volume,
                index_19 = index_short)
# Bergen end

pointindex_20_all <- get_published_pointindex_for_months(city_number, 2020, index_month)

city_trps <- pointindex_20_all[[1]]

pointindex_20 <- pointindex_20_all[[2]] %>%
  dplyr::filter(day_type == "ALL",
                is_excluded == FALSE,
                is_manually_excluded == FALSE,
                length_excluded == FALSE,
                period == "year_to_date",
                month == index_month) %>%
  dplyr::select(trp_id, #base_volume, calc_volume,
                index_20 = index_short)

city_name <- city_index_2020$area_name[1]

n_17 <- pointindex_17 %>%
  dplyr::filter(!is.na(index_17)) %>%
  nrow()

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20  %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

# Trondheim start
# Specific code to inculde toll stations:
source("city_index_dataprep_trondheim_toll_stations")
# Skip to refyear, but not relevant before 2021
# Trondheim stop

adt <- get_aadt_by_length_for_trp_list(city_trps)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  #dplyr::filter(year >= 2019) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)


# Bergen
adt_manual <- data.frame(
  trp_id = c("20642V805115", "25132V805616",
             "22439V804830", "04939V804763", "58509V804762", "88975V805079", "08446V805082",
             "05406V805081"),
  adt = c(7700, 8100, 5250, 100, 1700, 3500, 1630, 4530),
  year = c(2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020)
)

# Nord-Jæren
adt_manual <- data.frame(
  trp_id = c("68351V319882"),
  adt = c(31500),
  year = c(2017)
)

# Buskerudbyen
adt_manual <- data.frame(
  trp_id = c("26634V181322", "06687V181318"),
  adt = c(2200, 26500),
  year = c(2019, 2019)
)

# Oslo
this_citys_trp_index_prel <- points %>%
  dplyr::filter(trp_id %in% city_trps) %>%
  split_road_system_reference() %>%
  dplyr::select(trp_id, name, road_reference,
                road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position) %>%
  dplyr::left_join(trp_id_msnr) %>%
  left_join(adt_filtered) %>%
  #left_join(pointindex_18) %>%
  left_join(pointindex_19) %>%
  left_join(pointindex_20)

missing_adt <- this_citys_trp_index_prel %>%
  dplyr::filter(is.na(adt)) %>%
  dplyr::mutate(adt = mapply(getAadtByRoadlinkposition, road_link_position))

missing_adt_small_cars <- missing_adt %>%
  dplyr::mutate(adt = round(0.9 * adt, digits = -2),
                year = 2019)

this_citys_trp_index <- this_citys_trp_index_prel %>%
  dplyr::filter(!is.na(adt)) %>%
  dplyr::bind_rows(missing_adt_small_cars) %>%
  split_road_system_reference()
# Oslo end, skip to refyear

# Grenland
adt_manual <- data.frame(
  trp_id = c("26489V521174"),
  adt = c(9600),
  year = c(2018)
)

# Tromsø
adt_manual <- data.frame()

# Kristiansand
adt_manual <- data.frame(
  trp_id = c("33412V121301", "40820V121304", "00000V1702725",
             "47254V121508"),
  adt = c(40000, 19500, 8500, 8000),
  year = c(2017, 2018, 2017, 2017)
)

# Nedre Glomma
adt_manual <- data.frame()

# All
adt_all <- bind_rows(adt_filtered,
                     adt_manual
                     )

# Final table
this_citys_trp_index <- points %>%
  dplyr::filter(trp_id %in% city_trps) %>%
  split_road_system_reference() %>%
  dplyr::select(trp_id, name, road_reference,
                road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position) %>%
  dplyr::left_join(trp_id_msnr) %>%
  left_join(adt_all) %>%
  left_join(pointindex_17) %>%
  left_join(pointindex_18) %>%
  left_join(pointindex_19) %>%
  left_join(pointindex_20)

# Index from refyear
trp_index_from_refyear <- this_citys_trp_index %>%
  dplyr::select(trp_id, tidyselect::starts_with("index")) %>%
  dplyr::filter(
    dplyr::across(
      .cols = tidyselect::starts_with("index"),
      .fns = ~ !is.na(.x)
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::starts_with("index"),
      .fns = ~ index_converter(.))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(index = prod(c_across(tidyselect::starts_with("index")))) %>%
  dplyr::mutate(index = round(100 * (index - 1), digits = 1)) %>%
  dplyr::select(trp_id, index)

this_citys_trp_index_refyear <- this_citys_trp_index %>%
  dplyr::left_join(trp_index_from_refyear)

# TODO: include coverage
# TODO: 3 year rolling index, but not now - only for the city

write.csv2(this_citys_trp_index_refyear,
           file = paste0("data_indexpoints_tidy/indekspunkt_", city_number, ".csv"),
           row.names = F)



# City index ####
city_year_to_date_17 <- city_index_2017 %>%
  dplyr::filter(month == 12,
                road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
                length_range == "[..,5.6)",
                period == "year_to_date")

city_year_to_date_18 <- city_index_2018 %>%
  dplyr::filter(month == 12,
                road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
                length_range == "[..,5.6)",
                period == "year_to_date")

city_year_to_date_19 <- city_index_2019 %>%
  dplyr::filter(month == 12,
                road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
                length_range == "[..,5.6)",
                period == "year_to_date")

city_year_to_date_20 <- city_index_2020 %>%
  dplyr::filter(month == index_month,
                road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
                length_range == "[..,5.6)",
                period == "year_to_date")

city_index <- bind_rows(
  city_year_to_date_17,
  city_year_to_date_18,
  city_year_to_date_19,
  city_year_to_date_20) %>%
  select(area_name, year, period, index_p, standard_deviation, confidence_width) %>%
  mutate(year_base = year - 1,
         index_i = index_converter(index_p),
         variance = standard_deviation^2,
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
  #bind_rows(years_1_2) %>%
  #bind_rows(years_1_3) %>%
  bind_rows(years_1_4) %>%
  dplyr::mutate(year_from_to = paste0(year_base, "-", year),
                ci_start = index_p - confidence_width,
                ci_end = index_p + confidence_width,
                area_name = city_name)

write.csv2(city_index_all,
           file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".csv"),
           row.names = F)



# City index monthly ####
city_monthly <- bind_rows(
  monthly_city_index(city_index_2017),
  monthly_city_index(city_index_2018),
  monthly_city_index(city_index_2019),
  monthly_city_index(city_index_2020)) %>%
  select(area_name, year, month, period, month_object, month_name, index_p,
         standard_deviation, confidence_width, base_volume, calc_volume)

write.csv2(city_monthly,
           file = paste0("data_indexpoints_tidy/byindeks_maanedlig_", city_number, ".csv"),
           row.names = F)


# City index three year rolling ####
# No use in calculating this before 37 months are available
# The first 36 month index is equal to the first three whole year index!
# TODO: 36 month rolling index with sd and ci

all_possible_36_month_indexes <-
  calculate_all_possible_36_month_indexes(city_monthly)

write.csv2(all_possible_36_month_indexes,
           file = paste0("data_indexpoints_tidy/byindeks_36_maaneder_", city_number, ".csv"),
           row.names = F)


# E18 Buskerudbyen
trps_e18 <- c("08879V180819", "17291V181259")

point_index_e18 <- dplyr::bind_rows(
  get_pointindices_for_trp_list(trps_e18, 2017),
  get_pointindices_for_trp_list(trps_e18, 2018),
  get_pointindices_for_trp_list(trps_e18, 2019),
  get_pointindices_for_trp_list(trps_e18, 2020)
) %>%
  dplyr::filter(day_type == "ALL",
                period == "year_to_date") %>%
  dplyr::group_by(year) %>%
  dplyr::filter(month == max(month))

trps_e18_index <- points %>%
  dplyr::filter(trp_id %in% trps_e18) %>%
  split_road_system_reference() %>%
  dplyr::select(trp_id, name, road_reference,
                road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position) %>%
  dplyr::left_join(point_index_e18)

write.csv2(trps_e18_index,
           file = "data_indexpoints_tidy/buskerudbyen_e18_punktindekser.csv",
           row.names = F)
