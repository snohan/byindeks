# Old versions of code

# Bergen 2016 points ####
this_citys_trps <- choose_city_trp_ids("Bergen", 2016) %>%
  dplyr::left_join(points_split_reference) %>%
  dplyr::arrange(road_category, road_number,
                 section_number, subsection_number, meter,
                 intersection_part_number, intersection_meter) %>%
  dplyr::select(trp_id, msnr, name, road_reference, road_category_and_number,
                county_name, municipality_name,
                lat, lon, road_link_position)

# Index results from CSV-files
pointindex_17 <-
  readPointindexCSV("data_index_raw/pointindex_bergen-2017-12_2016.csv") %>%
  rename(index_17 = index)

pointindex_18 <-
  readPointindexCSV("data_index_raw/pointindex_bergen-2018-12_2017.csv") %>%
  rename(index_18 = index)

pointindex_19 <-
  readPointindexCSV("data_index_raw/pointindex_bergen-2019-12_2018.csv") %>%
  rename(index_19 = index)

pointindex_20 <-
  read_new_pointindex_csv("data_index_raw/punktindeks_bergen-2020-04.csv") %>%
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
  trp_id = c("20642V805115", "25132V805616",
             "22439V804830"),
  adt = c(9000, 8800, 6800),
  year = c(2018, 2017, 2017)
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

# TODO: 3 year rolling index, but not now - only for the city

write.csv2(this_citys_trp_index_refyear,
           file = "data_indexpoints_tidy/indekspunkt_bergen_2016.csv",
           row.names = F)


# Bergen 2016 city ####
city_17 <-
  read_city_index_csv("data_index_raw/Bergen-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
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
           file = "data_indexpoints_tidy/byindeks_bergen_2016.csv",
           row.names = F)


# Bergen 2016 monthly ####
city_17_monthly <-
  monthly_city_index("data_index_raw/Bergen-2017-12_2016.csv") %>%
  mutate(year = "2016-2017")
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
  city_17_monthly,
  city_18_monthly,
  city_19_monthly,
  city_20_monthly)

write.csv2(city_monthly,
           file = "data_indexpoints_tidy/byindeks_maanedlig_bergen_2016.csv",
           row.names = F)


# Bergen 2016 three year ####
# No use in calculating this before 37 months are available
# The first 36 month index is equal to the first three whole year index!

# TODO: 36 month rolling index with sd and ci

all_possible_36_month_indexes <-
  calculate_all_possible_36_month_indexes(city_monthly)

write.csv2(all_possible_36_month_indexes,
           file = "data_indexpoints_tidy/byindeks_36_maaneder_bergen_2016.csv",
           row.names = F)