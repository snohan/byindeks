# Gathering info on all points and indexes



# Setup ####
# Packages are to be loaded through sourcing rmd_setup.R in the Rmd report file.
source("rmd_setup.R")
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
cities_points <- read.csv2("data_points_raw/cities_points.csv")
trp_id_msnr <- cities_points %>%
  dplyr::select(trp_id, msnr = legacyNortrafMpn)
#cities_points_unestablished <-
#  read_csv2("data_points_raw/points_unestablished.csv")

# All points from Traffic Data API
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


# All points from TRP API (if needed)
#points_trp <- get_points_from_trpapi_httr() %>%
#  split_road_system_reference() #%>%
# dplyr::select(trp_id, name, road_reference, road_category, road_number,
#               road_category_and_number,
#               section_number, subsection_number, meter,
#               intersection_part_number, intersection_meter,
#               county_name, municipality_name, lat, lon, road_link_position)



# City numbers
# Bergen 958
# Buskerudbyen 1952
# Grenland 955
# Kristiansand og omegn 957
# Nedre Glomma 953
# Nord-Jæren 952
# Oslo 959
# Trondheim 960
# Tromsø 961

# Choose
index_month <- 10
city_number <- 959

# Pointindices ####
# TODO: TRPs might differ from year to year!

# Fetch
city_index_2018 <- get_published_index_for_months(city_number, 2018, 12)
city_index_2019 <- get_published_index_for_months(city_number, 2019, 12)
city_index_2020 <- get_published_index_for_months(city_number, 2020, index_month)


# Still need to specify csv-files for years before 2020 to get the pointindex as they are not in API
pointindex_18 <- readPointindexCSV(
  paste0("data_index_raw/pointindex_", city_number, "_", 2018, ".csv")
  ) %>%
  rename(index_18 = index)

pointindex_19 <- readPointindexCSV(
  paste0("data_index_raw/pointindex_", city_number, "_", 2019, ".csv")
  ) %>%
  rename(index_19 = index)

pointindex_20_all <- get_published_pointindex_for_months(city_number, 2020, index_month)

city_trps <- pointindex_20_all[[1]]

pointindex_20 <- pointindex_20_all[[2]] %>%
  dplyr::filter(day_type == "ALL",
                is_excluded == FALSE,
                is_manually_excluded == FALSE,
                length_excluded == FALSE,
                period == "year_to_date",
                month == index_month) %>%
  dplyr::select(trp_id, index_20 = index_short)

city_name <- city_index_2020$area_name[1]

n_18 <- pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <- pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <- pointindex_20  %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

adt <- get_aadt_by_length_for_trp_list(city_trps)

adt_filtered <- adt %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year >= 2019) %>%
  dplyr::filter(year == min(year)) %>%
  dplyr::select(trp_id, aadt_length_range, year) %>%
  dplyr::rename(adt = 2)


# Bergen
adt_manual <- data.frame(
  trp_id = c("20642V805115", "25132V805616",
             "22439V804830"),
  adt = c(9000, 8800, 6800),
  year = c(2018, 2017, 2017)
)

# Nord-Jæren
adt_manual <- data.frame(
  trp_id = c("68351V319882"),
  adt = c(31500),
  year = c(2017)
)

# Oslo
this_citys_trp_index_prel <- points %>%
  dplyr::filter(trp_id %in% city_trps) %>%
  split_road_system_reference() %>%
  dplyr::select(trp_id, name, road_reference,
                road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position) %>%
  left_join(adt_filtered) %>%
  left_join(pointindex_20)

missing_adt <- this_citys_trp_index_prel %>%
  dplyr::filter(is.na(adt)) %>%
  dplyr::mutate(adt = mapply(getAadtByRoadlinkposition, road_link_position))

missing_adt_small_cars <- missing_adt %>%
  dplyr::mutate(adt = round(0.9 * adt, digits = -2),
                year = 2019)

this_citys_trp_index <- this_citys_trp_index_prel %>%
  dplyr::filter(!is.na(adt)) %>%
  dplyr::bind_rows(missing_adt_small_cars)
# Oslo end, skip to refyear

adt_all <- bind_rows(adt_filtered, adt_manual)

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
           file = paste0("data_indexpoints_tidy/indekspunkt_", city_number, ".csv"),
           row.names = F)



# City index ####
city_year_to_date_18 <- city_index_2018 %>%
  dplyr::filter(month == index_month,
                road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
                length_range == "[..,5.6)",
                period == "year_to_date")

city_year_to_date_19 <- city_index_2019 %>%
  dplyr::filter(month == index_month,
                road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
                length_range == "[..,5.6)",
                period == "year_to_date")


city_year_to_date_20 <- city_index_2020 %>%
  dplyr::filter(month == index_month,
                road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
                length_range == "[..,5.6)",
                period == "year_to_date")

city_index <- bind_rows(
  city_year_to_date_18,
  city_year_to_date_19,
  city_year_to_date_20) %>%
  select(area_name, year, period, index_p, standard_deviation, confidence_width) %>%
  mutate(year_base = year - 1,
         index_i = index_converter(index_p),
         variance = standard_deviation^2,
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
  dplyr::mutate(year_from_to = paste0(year_base, "-", year),
                ci_start = index_p - confidence_width,
                ci_end = index_p + confidence_width)

write.csv2(city_index_all,
           file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".csv"),
           row.names = F)



# City index monthly ####
city_monthly <- bind_rows(
  monthly_city_index(city_index_2018),
  monthly_city_index(city_index_2019),
  monthly_city_index(city_index_2020)) %>%
  select(area_name, year, month, period, month_object, month_name, index_p,
         standard_deviation, confidence_width)

write.csv2(city_monthly,
           file = paste0("data_indexpoints_tidy/byindeks_maanedlig_", city_number, ".csv"),
           row.names = F)
