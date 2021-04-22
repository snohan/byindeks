# This script adds trp index and toll station index
# An extra to city_index_dataprep.R

# Tolling stations with metadata ####
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

# TRPs ####
# Note: points and city_trps is made in city_index_dataprep.R
this_citys_trps <- points %>%
  dplyr::filter(trp_id %in% city_trps) %>%
  dplyr::select(trp_id, name, road_reference,
                municipality_name,
                lat, lon, road_link_position) %>%
  dplyr::mutate(station_type = "Trafikkregistrering")

# All points ####
this_citys_trps_all <- bind_rows(this_citys_trps, trh_bomer) %>%
  split_road_system_reference() %>%
  dplyr::select(trp_id, name, road_reference,
                road_category_and_number,
                municipality_name, lat, lon, road_link_position,
                station_type)


# Index results per year ####
# from CSV-files
# Pointindex for trps fetched in parent file

tollpointindex <- read.csv2(
  "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.csv") %>%
  dplyr::rename(trp_id = kode,
                index = indeks) %>%
  dplyr::mutate(trp_id = as.character(trp_id)) %>%
  dplyr::select(-felt, -stasjon) %>%
  dplyr::select(trp_id, length_range = klasse, base_volume, calc_volume, index, year)

pointindex_20_trp_toll <- tollpointindex %>%
  dplyr::filter(year == 2020) %>%
  dplyr::filter(length_range != "Ukjent") %>%
  dplyr::mutate(index = round(index, digits = 1),
                length_range = dplyr::case_when(
                  length_range == "Alle" ~ "all",
                  length_range == "Liten_bil" ~ "short",
                  length_range == "Stor_bil" ~ "long"
                )) %>%
  dplyr::select(trp_id, length_range, base_volume, calc_volume, index) %>%
  dplyr::bind_rows(pointindex_20_long_year_to_date)

n_20 <- pointindex_20_trp_toll %>%
  dplyr::filter(length_range == "short") %>%
  dplyr::filter(!is.na(index)) %>%
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

# Finally all adt
this_citys_trps_all_adt_final <- this_citys_trps_all_adt %>%
  dplyr::filter(!is.na(adt)) %>%
  dplyr::bind_rows(missing_adt_small_cars)


pointindex_20_trp_toll_short <- pointindex_20_trp_toll %>%
  dplyr::filter(length_range == "short")

# Adding pointindices to all points
this_citys_trps_all_adt_final_index <- this_citys_trps_all_adt_final %>%
  dplyr::left_join(select(pointindex_20_trp_toll_short, 1, 4)) %>%
  split_road_system_reference()

# TODO: include refyear from completed 2021
this_citys_trp_index_refyear <- this_citys_trps_all_adt_final_index

# Write file in parent script


# City index ####
# Must calculate based on all pointindices
city_index_20 <- pointindex_20_trp_toll %>%
  dplyr::group_by(length_range) %>%
  dplyr::summarise(base_volume_all = sum(base_volume),
                   calc_volume_all = sum(calc_volume),
                   index_p = (calc_volume_all / base_volume_all - 1 ) * 100,
                   n_points = n(),
                   year_from_to = "2019-2020")

city_index_20_short <- city_index_20 %>%
  dplyr::filter(length_range == "short")

# To find weighted variance and ci
pointindex_20_trp_toll_sd <- pointindex_20_trp_toll %>%
  dplyr::filter(!is.na(index)) %>%
  dplyr::left_join(city_index_20) %>%
  dplyr::mutate(diff = (base_volume / base_volume_all) *
                  (index - index_p)^2,
                weight = (base_volume / base_volume_all)^2) %>%
  dplyr::group_by(length_range) %>%
  dplyr::summarise(standardavvik = sqrt((1 / (1 - sum(weight) )) * sum(diff) ))

city_index_20_sd <- city_index_20 %>%
  dplyr::left_join(pointindex_20_trp_toll_sd) %>%
  dplyr::mutate(variance = standardavvik^2,
                confidence_width = qt(0.975, n_points - 1) * standardavvik /
                  sqrt(n_points))

city_index <- bind_rows(city_index_20_sd#,
                        #city_index_21_sd,
                        #city_index_22_sd
) %>%
  dplyr::select(-base_volume_all, -calc_volume_all) %>%
  dplyr::mutate(index_i = index_converter(index_p),
                # TODO: True coverage
                dekning = 100)


# TODO: fom refyear from 2021
# TODO: replace ci with se
city_index_all <- city_index %>%
  #bind_rows(next_two_years) %>%
  #bind_rows(first_two_years) %>%
  #bind_rows(last_two_years) %>%
  dplyr::mutate(ci_start = index_p - confidence_width,
                ci_end = index_p + confidence_width)



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

pointindex_20_monthly <- pointindex_20_all[[2]] %>%
  dplyr::filter(day_type == "ALL",
                is_excluded == FALSE,
                is_manually_excluded == FALSE,
                length_excluded == FALSE,
                period == "month") %>%
  dplyr::select(trp_id, length_range, base_volume, calc_volume, index, year, month) %>%
  dplyr::mutate(month_object = lubridate::make_date(year = year, month = month)) %>%
  dplyr::select(trp_id, length_range, base_volume, calc_volume, index, month_object)

pointindex_20_trp_toll_monthly <- tollpointindex_monthly %>%
  dplyr::filter(length_range != "Ukjent") %>%
  dplyr::mutate(index = round(index, digits = 1),
                length_range = dplyr::case_when(
                  length_range == "Alle" ~ "all",
                  length_range == "Liten_bil" ~ "short",
                  length_range == "Stor_bil" ~ "long"
                )) %>%
  dplyr::bind_rows(pointindex_20_monthly) %>%
  dplyr::mutate(year = lubridate::year(month_object)) %>%
  dplyr::filter(year == 2020) %>%
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

# TODO: sd and ci

write.csv2(pointindex_20_trp_toll_monthly,
           file = paste0("data_indexpoints_tidy/byindeks_maanedlig_", city_number, ".csv"),
           row.names = F)
