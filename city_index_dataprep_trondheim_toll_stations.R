# This script adds trp index and toll station index
# Instead of city_index_dataprep.R

# IN
# TRP index for all years
#

# OUT
#

# City index meta data ----
## Declare latest month ----
index_month <- 12
city_number <- 960

## City name ----
# Need just most recent city index from API, to get its offical name
# city_index_from_api <-
#   get_published_index_for_months(
#     city_number,
#     2022,
#     index_month
#   )

city_name <- "Trondheim"
  #city_index_from_api$area_name[1]


# TRP index for each year ----
pointindex_20_all <-
  get_published_pointindex_for_months_trondheim(city_number, 2020, 12)
pointindex_21_all <-
  get_published_pointindex_for_months_trondheim(city_number, 2021, 12)
pointindex_22_all <-
  get_published_pointindex_for_months_trondheim(city_number, 2022, index_month)


pointindices_longformat_year_to_date <-
  dplyr::bind_rows(
    pointindex_20_all[[2]],
    pointindex_21_all[[2]],
    pointindex_22_all[[2]]
  ) %>%
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE
  ) |>
  dplyr::group_by(year) %>%
  dplyr::filter(
    period == "year_to_date",
    month == max(month)
  ) %>%
  dplyr::select(
    trp_id,
    year,
    month,
    length_range,
    base_volume,
    calc_volume,
    index
  ) |>
  dplyr::mutate(
    month = lubridate::make_date(year = year, month = month)
  )


# TRPs ----
# Note: "points" is made in city_index_dataprep.R
# Choosing most recent version of city trps
city_trps <- pointindex_22_all[[1]]

this_citys_trps <-
  points %>%
  dplyr::filter(trp_id %in% city_trps) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    municipality_name,
    lat, lon, road_link_position
  ) %>%
  dplyr::mutate(station_type = "Trafikkregistrering")

# TRPs and tolling stations together ----
# Made in bomdata_trondheim.R
kommune_bomer <-
  readr::read_rds(
    file = "bomdata_trondheim/trd_toll_stations.rds"
  )

this_citys_trps_all <-
  dplyr::bind_rows(
    this_citys_trps,
    kommune_bomer
  ) %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    road_category_and_number,
    municipality_name,
    lat, lon, road_link_position,
    station_type
  )

trd_station_type <-
  this_citys_trps_all |>
  dplyr::select(
    trp_id,
    station_type
  )

# AADT ----
# adt_trp_id <-
#   this_citys_trps_all %>%
#   dplyr::filter(station_type == "Trafikkregistrering")
#
# adt_trp <-
#   get_aadt_for_trp_list(adt_trp_id$trp_id)

#adt_trp_filtered <-
#  adt_trp %>%
  #dplyr::filter(length_range == "[..,5.6)") %>%
  #dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  #dplyr::filter(length_quality > 90) %>%
#  dplyr::filter(coverage > 50) %>%
#  dplyr::group_by(trp_id) %>%
  #dplyr::filter(year >= 2019) %>%
#  dplyr::filter(year == max(year)) %>%
#  dplyr::select(trp_id, adt, year)

# this_citys_trps_all_adt <-
#   this_citys_trps_all %>%
#   dplyr::left_join(
#     adt_trp_filtered,
#     by = "trp_id"
#   )
#
# missing_adt <-
#   this_citys_trps_all_adt %>%
#   dplyr::filter(is.na(adt)) %>%
#   dplyr::mutate(
#     adt = mapply(getAadtByRoadlinkposition, road_link_position),
#     year = 2021
#   )

# Finally all aadt
# this_citys_trps_all_adt_final <-
#   this_citys_trps_all_adt %>%
#   dplyr::filter(!is.na(adt)) %>%
#   dplyr::bind_rows(missing_adt)

this_citys_trps_all_adt_final <-
  readr::read_rds(
    file = paste0(
      "index_trp_metadata/trp_",
      city_number,
      ".rds"
    )
  ) |>
  dplyr::left_join(
    trd_station_type,
    by = "trp_id"
  )

trp_names <-
  this_citys_trps_all_adt_final |>
  dplyr::select(
    trp_id,
    name
  )

# Index results per year ----
# Note: Tolling station csv file includes data from 2017,
# but here we only want index from 2020 onwards
tollpointindex <-
  readRDS(
    "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.rds"
  ) %>%
  # read.csv2(
  #   "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.csv"
  # ) %>%
  dplyr::rename(
    index = index_p,
    length_range = class
  )

pointindex_trp_toll <-
  tollpointindex %>%
  dplyr::filter(year >= 2020) %>%
  dplyr::filter(length_range != "unknown") %>%
  dplyr::mutate(
    index = round(index, digits = 1)#,
    #length_range =
    #  dplyr::case_when(
    #    length_range == "all" ~ "alle",
    #    length_range == "light" ~ "lette",
    #    length_range == "heavy" ~ "tunge",
    #    TRUE ~ length_range
    #  )
  ) %>%
  dplyr::bind_rows(
    pointindices_longformat_year_to_date
  )

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
pointindex_trp_toll_short <-
  pointindex_trp_toll %>%
  dplyr::filter(length_range == "lette") %>%
  dplyr::select(trp_id, year, index) %>%
  tidyr::pivot_wider(
    names_from = year,
    names_prefix = "index_",
    values_from = index
  )


# Adding pointindices to all points
this_citys_trps_all_adt_final_index <-
  this_citys_trps_all_adt_final %>%
  dplyr::left_join(
    pointindex_trp_toll_short,
    by = "trp_id"
  ) %>%
  split_road_system_reference()

trp_index_refyear <-
  this_citys_trps_all_adt_final_index %>%
  dplyr::select(
    trp_id,
    tidyselect::starts_with("index")
  ) %>%
  dplyr::filter(
    dplyr::if_all(
      .cols = tidyselect::starts_with("index"),
      .fns = ~ !is.na(.x)
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::starts_with("index"),
      .fns = ~ index_converter(.))
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(index = prod(c_across(tidyselect::starts_with("index")))) %>%
  dplyr::mutate(index = round(100 * (index - 1), digits = 1)) %>%
  dplyr::select(trp_id, index)

this_citys_trp_index_refyear <-
  this_citys_trps_all_adt_final_index %>%
  dplyr::left_join(
    trp_index_refyear,
    by = "trp_id"
  )

readr::write_rds(
  this_citys_trp_index_refyear,
  file = paste0("data_indexpoints_tidy/indekspunkt_", city_number, ".rds")
)


# City index ----
# Must calculate based on all pointindices
city_index <-
  pointindex_trp_toll %>%
  dplyr::group_by(
    length_range,
    year,
    month
  ) %>%
  dplyr::summarise(
    base_volume_all = sum(base_volume),
    calc_volume_all = sum(calc_volume),
    index_p = (calc_volume_all / base_volume_all - 1 ) * 100,
    n_trp = n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    year_from_to =
      dplyr::case_when(
        year == 2020 ~ "2019-2020",
        year == 2021 ~ "2020-2021",
        year == 2022 ~ "2021-2022"
      )
  )

city_index_short <-
  city_index %>%
  dplyr::filter(length_range == "lette")

# To find weighted variance and SE
pointindex_trp_toll_sd <-
  pointindex_trp_toll %>%
  dplyr::filter(!is.na(index)) %>%
  dplyr::left_join(city_index) %>%
  dplyr::mutate(
    diff = (base_volume / base_volume_all) * (index - index_p)^2,
    weight = (base_volume / base_volume_all)^2
  ) %>%
  dplyr::group_by(length_range) %>%
  dplyr::summarise(
    standard_deviation = sqrt((1 / (1 - sum(weight) )) * sum(diff) )
  )

city_index_sd <-
  city_index %>%
  dplyr::left_join(
    pointindex_trp_toll_sd
  ) %>%
  dplyr::mutate(
    year_base = year - 1,
    variance = standard_deviation^2,
    standard_error = round(standard_deviation / sqrt(n_trp), digits = 1)
  ) %>%
  dplyr::select(
    -base_volume_all,
    -calc_volume_all
  ) %>%
  dplyr::mutate(
    index_i = index_converter(index_p)
  )

city_index_light <-
  city_index_sd %>%
  dplyr::ungroup() %>%
  dplyr::filter(length_range == "lette")

# TODO: fom refyear from 2021 per length_range
years_1_2 <-
  city_index_light %>%
  calculate_two_year_index() %>%
  dplyr::mutate(length_range = "lette")

years_1_3 <-
  bind_rows(years_1_2, slice(city_index_light, 3)) %>%
  calculate_two_year_index() %>%
  dplyr::mutate(length_range = "lette")

# Skipping intermediate years, adding just from first to last
city_index_all <-
  city_index_sd %>%
  bind_rows(years_1_2) %>%
  bind_rows(years_1_3) %>%
  #bind_rows(years_1_4) %>%
  dplyr::mutate(
    year_from_to = paste0(year_base, "-", year),
    area_name = city_name
  )

readr::write_rds(
  city_index_all,
  file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".rds")
)


# MDT ----
# Use city_index_dataprep.R

# City index monthly ----
# tollpointindex_monthly <-
#   read.csv2(
#     "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_bymaanedsindekser.csv"
#   ) %>%
#   dplyr::rename(
#     index = index_p
#   ) %>%
#   dplyr::mutate(trp_id = as.character(trp_id)) %>%
#   dplyr::select(
#     trp_id,
#     length_range = class,
#     base_volume,
#     calc_volume,
#     index,
#     month_as_date
#   ) %>%
#   dplyr::mutate(
#     month_object = lubridate::ymd(month_as_date)
#   ) %>%
#   dplyr::select(
#     trp_id,
#     length_range,
#     base_volume,
#     calc_volume,
#     index,
#     month_object
#   )
#
# pointindex_monthly <-
#   dplyr::bind_rows(
#     pointindex_20_all[[2]],
#     pointindex_21_all[[2]],
#     pointindex_22_all[[2]]
#   ) %>%
#   dplyr::filter(
#     day_type == "ALL",
#     is_excluded == FALSE,
#     is_manually_excluded == FALSE,
#     length_excluded == FALSE,
#     period == "month"
#   ) %>%
#   #dplyr::select(trp_id, length_range, base_volume, calc_volume, index, year, month) %>%
#   dplyr::mutate(
#     month_object = lubridate::make_date(year = year, month = month)
#   ) %>%
#   dplyr::select(
#     trp_id,
#     length_range,
#     base_volume,
#     calc_volume,
#     index,
#     month_object
#   )

# pointindex_trp_toll_monthly <-
#   tollpointindex_monthly %>%
#   dplyr::filter(length_range != "unknown") %>%
#   dplyr::mutate(
#     index = round(index, digits = 1),
#     length_range =
#       dplyr::case_when(
#         length_range == "all" ~ "alle",
#         length_range == "light" ~ "lette",
#         length_range == "heavy" ~ "tunge"
#       )
#   ) %>%
#   dplyr::bind_rows(pointindex_monthly)
#
# city_index_monthly <-
#   pointindex_trp_toll_monthly %>%
#   dplyr::mutate(year = lubridate::year(month_object)) %>%
#   dplyr::filter(year >= 2020) %>%
#   dplyr::mutate(index = round(index, digits = 1)) %>%
#   dplyr::group_by(
#     month_object,
#     length_range
#   ) %>%
#   dplyr::summarise(
#     base_volume_all = sum(base_volume),
#     calc_volume_all = sum(calc_volume),
#     index_p = (calc_volume_all / base_volume_all - 1 ) * 100,
#     n_points = n()
#   ) %>%
#   dplyr::mutate(
#     area_name = "Trondheim",
#     year = lubridate::year(month_object),
#     month = lubridate::month(month_object),
#     period = "month",
#     month_name =
#       lubridate::month(month_object, label = TRUE, abbr = FALSE) %>%
#       stringr::str_to_title()
#   )

# sd and se
# To get sd, must start with pointindices and join monthly city index

# pointindex_trp_toll_monthly_sd <-
#   pointindex_trp_toll_monthly %>%
#   dplyr::filter(!is.na(index)) %>%
#   dplyr::left_join(city_index_monthly) %>%
#   dplyr::filter(!is.na(base_volume_all)) %>%
#   dplyr::mutate(
#     diff = (base_volume / base_volume_all) * (index - index_p)^2,
#     weight = (base_volume / base_volume_all)^2
#   ) %>%
#   dplyr::group_by(
#     month_object,
#     length_range
#   ) %>%
#   dplyr::summarise(
#     standard_deviation = sqrt((1 / (1 - sum(weight) )) * sum(diff) )
#   )
#
# city_index_monthly_sd <-
#   city_index_monthly %>%
#   dplyr::left_join(pointindex_trp_toll_monthly_sd) %>%
#   dplyr::mutate(
#     standard_error = round(standard_deviation / sqrt(n_points), digits = 1)
#   ) %>%
#   dplyr::select(
#     -base_volume_all,
#     -calc_volume_all
#   )
#
# write.csv2(
#   city_index_monthly_sd,
#   file = paste0("data_indexpoints_tidy/byindeks_maanedlig_", city_number, ".csv"),
#   row.names = F
# )

# Check:
# pi_wide <-
#   pointindex_trp_toll_monthly %>%
#   dplyr::filter(
#     length_range == "lette",
#     month_object >= "2022-01-01"
#   )
