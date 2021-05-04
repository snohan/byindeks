# This script adds trp index and toll station index
# Instead of city_index_dataprep.R

# City index meta data ----
## Declare latest month ----
index_month <- 3
city_number <- 960

## City name ----
# Need just most recent city index from API, to get its offical name
city_index_from_api <- get_published_index_for_months(city_number, 2021, index_month)
city_name <- city_index_from_api$area_name[1]


# TRP index for each year ----
pointindex_20_all <- get_published_pointindex_for_months_trondheim(city_number, 2020, 12)
pointindex_21_all <- get_published_pointindex_for_months_trondheim(city_number, 2021, index_month)

pointindices_longformat <-
  dplyr::bind_rows(pointindex_20_all[[2]],
                   pointindex_21_all[[2]]) %>%
  dplyr::filter(day_type == "ALL",
                is_excluded == FALSE,
                is_manually_excluded == FALSE,
                length_excluded == FALSE)

pointindices_longformat_year_to_date <- pointindices_longformat %>%
  dplyr::group_by(year) %>%
  dplyr::filter(period == "year_to_date",
                month == max(month)) %>%
  dplyr::select(trp_id, year, month, length_range, base_volume, calc_volume, index)


# Tolling stations ----
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
  dplyr::mutate(municipality_name = "Trondheim")

# TRPs ----
# Note: "points" is made in city_index_dataprep.R
# Choosing most recent version of city trps
city_trps <- pointindex_21_all[[1]]

this_citys_trps <- points %>%
  dplyr::filter(trp_id %in% city_trps) %>%
  dplyr::select(trp_id, name, road_reference,
                municipality_name,
                lat, lon, road_link_position) %>%
  dplyr::mutate(station_type = "Trafikkregistrering")

# TRPs and tolling stations together ----
this_citys_trps_all <- bind_rows(this_citys_trps, trh_bomer) %>%
  split_road_system_reference() %>%
  dplyr::select(trp_id, name, road_reference,
                road_category_and_number,
                municipality_name, lat, lon, road_link_position,
                station_type)


# AADT ----
adt_trp_id <- this_citys_trps_all %>%
  dplyr::filter(station_type == "Trafikkregistrering")

adt_trp <- get_aadt_by_length_for_trp_list(adt_trp_id$trp_id)

adt_trp_filtered <- adt_trp %>%
  dplyr::filter(length_range == "[..,5.6)") %>%
  dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::group_by(trp_id) %>%
  #dplyr::filter(year >= 2019) %>%
  dplyr::filter(year == max(year)) %>%
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

# Finally all aadt
this_citys_trps_all_adt_final <- this_citys_trps_all_adt %>%
  dplyr::filter(!is.na(adt)) %>%
  dplyr::bind_rows(missing_adt_small_cars)


# Index results per year ----
# Note: Tolling station csv file includes data from 2017,
# but here we only want index from 2020 onwards
tollpointindex <- read.csv2(
  "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.csv") %>%
  dplyr::rename(trp_id = kode,
                index = indeks) %>%
  dplyr::mutate(trp_id = as.character(trp_id)) %>%
  dplyr::select(-felt, -stasjon) %>%
  dplyr::select(trp_id, year, month, length_range = klasse, base_volume, calc_volume, index)

pointindex_trp_toll <- tollpointindex %>%
  dplyr::filter(year >= 2020) %>%
  dplyr::filter(length_range != "Ukjent") %>%
  dplyr::mutate(index = round(index, digits = 1),
                length_range = dplyr::case_when(
                  length_range == "Alle" ~ "alle",
                  length_range == "Liten_bil" ~ "lette",
                  length_range == "Stor_bil" ~ "tunge"
                )) %>%
  dplyr::bind_rows(pointindices_longformat_year_to_date)

# Do not need these n's?
# n_20 <- pointindex_trp_toll %>%
#   dplyr::filter(year == 2020,
#                 length_range == "lette",
#                 !is.na(index)) %>%
#   nrow()
#
# n_21 <- pointindex_trp_toll %>%
#   dplyr::filter(year == 2021,
#                 length_range == "lette",
#                 !is.na(index)) %>%
#   nrow()

# Should we include all three classes in report? Not now.
pointindex_trp_toll_short <- pointindex_trp_toll %>%
  dplyr::filter(length_range == "lette") %>%
  dplyr::select(trp_id, year, index) %>%
  tidyr::pivot_wider(names_from = year, names_prefix = "index_",
                     values_from = index)


# Adding pointindices to all points
this_citys_trps_all_adt_final_index <- this_citys_trps_all_adt_final %>%
  dplyr::left_join(pointindex_trp_toll_short) %>%
  split_road_system_reference()

# TODO: include refyear from completed 2021
this_citys_trp_index_refyear <- this_citys_trps_all_adt_final_index

write.csv2(this_citys_trp_index_refyear,
           file = paste0("data_indexpoints_tidy/indekspunkt_", city_number, ".csv"),
           row.names = F)


# City index ----
# Must calculate based on all pointindices
city_index <- pointindex_trp_toll %>%
  dplyr::group_by(length_range, year, month) %>%
  dplyr::summarise(base_volume_all = sum(base_volume),
                   calc_volume_all = sum(calc_volume),
                   index_p = (calc_volume_all / base_volume_all - 1 ) * 100,
                   n_points = n()) %>%
  dplyr::mutate(year_from_to = dplyr::case_when(
      year == 2020 ~ "2019-2020",
      year == 2021 ~ "2020-2021"
  ))

city_index_short <- city_index %>%
  dplyr::filter(length_range == "lette")

# To find weighted variance and SE
pointindex_trp_toll_sd <- pointindex_trp_toll %>%
  dplyr::filter(!is.na(index)) %>%
  dplyr::left_join(city_index) %>%
  dplyr::mutate(diff = (base_volume / base_volume_all) *
                  (index - index_p)^2,
                weight = (base_volume / base_volume_all)^2) %>%
  dplyr::group_by(length_range) %>%
  dplyr::summarise(standard_deviation = sqrt((1 / (1 - sum(weight) )) * sum(diff) ))

city_index_sd <- city_index %>%
  dplyr::left_join(pointindex_trp_toll_sd) %>%
  dplyr::mutate(year_base = year - 1,
                variance = standard_deviation^2,
                standard_error = round(standard_deviation / sqrt(n_points), digits = 1)) %>%
  dplyr::select(-base_volume_all, -calc_volume_all) %>%
  dplyr::mutate(index_i = index_converter(index_p))


# TODO: fom refyear from 2021 per length_range
years_1_2 <- city_index_sd %>%
  dplyr::ungroup() %>%
  dplyr::filter(length_range == "lette") %>%
  calculate_two_year_index() %>%
  dplyr::mutate(length_range = "lette")

# Skipping intermediate years, adding just from first to last
city_index_all <- city_index_sd %>%
  bind_rows(years_1_2) %>%
  #bind_rows(years_1_3) %>%
  #bind_rows(years_1_4) %>%
  dplyr::mutate(year_from_to = paste0(year_base, "-", year),
                area_name = city_name)

write.csv2(city_index_all,
           file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".csv"),
           row.names = F)


# City index monthly ----
tollpointindex_monthly <- read.csv2(
  "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_bymaanedsindekser.csv") %>%
  dplyr::rename(trp_id = kode,
                index = indeks) %>%
  dplyr::mutate(trp_id = as.character(trp_id)) %>%
  dplyr::select(-felt, -stasjon) %>%
  dplyr::select(trp_id, length_range = klasse, base_volume, calc_volume, index, year_month = aar_maaned) %>%
  dplyr::mutate(month_object = lubridate::ymd(year_month)) %>%
  dplyr::select(trp_id, length_range, base_volume, calc_volume, index, month_object)

pointindex_monthly <- dplyr::bind_rows(pointindex_20_all[[2]],
                                       pointindex_21_all[[2]]) %>%
  dplyr::filter(day_type == "ALL",
                is_excluded == FALSE,
                is_manually_excluded == FALSE,
                length_excluded == FALSE,
                period == "month") %>%
  #dplyr::select(trp_id, length_range, base_volume, calc_volume, index, year, month) %>%
  dplyr::mutate(month_object = lubridate::make_date(year = year, month = month)) %>%
  dplyr::select(trp_id, length_range, base_volume, calc_volume, index, month_object)

pointindex_trp_toll_monthly <- tollpointindex_monthly %>%
  dplyr::filter(length_range != "Ukjent") %>%
  dplyr::mutate(index = round(index, digits = 1),
                length_range = dplyr::case_when(
                  length_range == "Alle" ~ "alle",
                  length_range == "Liten_bil" ~ "lette",
                  length_range == "Stor_bil" ~ "tunge"
                )) %>%
  dplyr::bind_rows(pointindex_monthly)

city_index_monthly <- pointindex_trp_toll_monthly %>%
  dplyr::mutate(year = lubridate::year(month_object)) %>%
  dplyr::filter(year >= 2020) %>%
  dplyr::mutate(index = round(index, digits = 1)) %>%
  dplyr::group_by(month_object, length_range) %>%
  dplyr::summarise(base_volume_all = sum(base_volume),
                   calc_volume_all = sum(calc_volume),
                   index_p = (calc_volume_all / base_volume_all - 1 ) * 100,
                   n_points = n()) %>%
  dplyr::mutate(area_name = "Trondheim",
                year = lubridate::year(month_object),
                month = lubridate::month(month_object),
                period = "month",
                month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE) %>%
                  stringr::str_to_title())

# sd and se
# To get sd, must start with pointindices and join monthly city index

pointindex_trp_toll_monthly_sd <- pointindex_trp_toll_monthly %>%
  dplyr::filter(!is.na(index)) %>%
  dplyr::left_join(city_index_monthly) %>%
  dplyr::filter(!is.na(base_volume_all)) %>%
  dplyr::mutate(diff = (base_volume / base_volume_all) *
                  (index - index_p)^2,
                weight = (base_volume / base_volume_all)^2) %>%
  dplyr::group_by(month_object, length_range) %>%
  dplyr::summarise(standard_deviation = sqrt((1 / (1 - sum(weight) )) * sum(diff) ))

city_index_monthly_sd <- city_index_monthly %>%
  dplyr::left_join(pointindex_trp_toll_monthly_sd) %>%
  dplyr::mutate(standard_error = round(standard_deviation / sqrt(n_points), digits = 1)) %>%
  dplyr::select(-base_volume_all, -calc_volume_all)

write.csv2(city_index_monthly_sd,
           file = paste0("data_indexpoints_tidy/byindeks_maanedlig_", city_number, ".csv"),
           row.names = F)
