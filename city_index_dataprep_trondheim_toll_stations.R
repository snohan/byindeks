# Binds TRP and toll station index
# Calculates city index
# (Just for TRD - instead of the more general city_index_dataprep.R)

# IN
# TRP index monthly
# Toll station index monthly

# OUT
#

# Set manual variables ----
index_month <- 2
city_number <- 960
city_name <- "Trondheim"


# Get data ----
## TD API ----
trp_index_20 <- get_published_pointindex_for_months_trondheim(city_number, 2020, 12)
trp_index_21 <- get_published_pointindex_for_months_trondheim(city_number, 2021, 12)
trp_index_22 <- get_published_pointindex_for_months_trondheim(city_number, 2022, 12)
trp_index_23 <- get_published_pointindex_for_months_trondheim(city_number, 2023, index_month)

trp_index_all <-
  dplyr::bind_rows(
    trp_index_20[[2]],
    trp_index_21[[2]],
    trp_index_22[[2]],
    trp_index_23[[2]]
  )


## Prepared toll data ----
# Made in bomdata_trondheim.R
toll_meta_data <-
  readr::read_rds(
    file = "bomdata_trondheim/trd_toll_stations.rds"
  )

toll_index_yearly <-
  readr::read_rds(
    "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.rds"
  ) |>
  dplyr::rename(
    index = index_p,
    length_range = class
  ) |>
  dplyr::filter(length_range != "unknown") |>

toll_index_monthly <-
  readr::read_rds(
    file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser.rds",
  ) |>
  dplyr::filter(length_range != "unknown") |>
  dplyr::mutate(
    month_object = lubridate::ymd(month_calc),
    index = round(index, digits = 2)
  ) |>
  dplyr::select(
    trp_id,
    length_range = class,
    base_volume = monthly_volume_base,
    calc_volume = monthly_volume_calc,
    index = index_p,
    month_object
  )


# TRPs ----
# Choosing most recent version of city trps
city_trps <- trp_index_23[[1]]

# Removing Tungasletta Ystgaard (is removed from VTI, but is still in present in API before 2023)
#city_trps <-
#  city_trps[! city_trps %in% c("84826V42881")]

this_citys_trps <-
  # Note: "points" is made in city_index_dataprep.R
  points |>
  dplyr::filter(trp_id %in% city_trps) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    #municipality_name,
    lat, lon, road_link_position
  ) |>
  dplyr::mutate(station_type = "Trafikkregistrering")


# TRPs and tolling stations together
this_citys_trps_all <-
  dplyr::bind_rows(
    this_citys_trps,
    toll_meta_data
  ) |>
  split_road_system_reference() |>
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
# Fetch AADT in city_index_check.Rmd

trp_names <-
  this_citys_trps_all_adt_final |>
  dplyr::select(
    trp_id,
    name
  )


# TRP index yearly ----
trp_index_yearly <-
  trp_index_all |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE
  ) |>
  dplyr::group_by(year) |>
  dplyr::filter(
    period == "year_to_date",
    month == max(month)
  ) |>
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
  ) |>
  dplyr::ungroup()

trp_toll_index_yearly <-
  toll_index_yearly |>
  dplyr::mutate(
    index = round(index, digits = 1)
  ) |>
  dplyr::bind_rows(
    trp_index_yearly
  )


# TODO: All three classes in report
trp_toll_index_yearly_short <-
  trp_toll_index_yearly |>
  dplyr::filter(length_range == "lette") |>
  dplyr::select(trp_id, year, index) |>
  tidyr::pivot_wider(
    names_from = year,
    names_prefix = "index_",
    values_from = index
  )


# Binding pointindices to all points
this_citys_trps_all_adt_final_index <-
  this_citys_trps_all_adt_final |>
  dplyr::left_join(
    trp_toll_index_yearly_short,
    by = "trp_id"
  ) |>
  split_road_system_reference()

trp_index_refyear <-
  this_citys_trps_all_adt_final_index |>
  dplyr::select(
    trp_id,
    tidyselect::starts_with("index")
  ) |>
  dplyr::filter(
    dplyr::if_all(
      .cols = tidyselect::starts_with("index"),
      .fns = ~ !is.na(.x)
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::starts_with("index"),
      .fns = ~ index_converter(.))
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(index = prod(c_across(tidyselect::starts_with("index")))) |>
  dplyr::mutate(index = round(100 * (index - 1), digits = 1)) |>
  dplyr::select(trp_id, index)

this_citys_trp_index_refyear <-
  this_citys_trps_all_adt_final_index |>
  dplyr::left_join(
    trp_index_refyear,
    by = "trp_id"
  )

readr::write_rds(
  this_citys_trp_index_refyear,
  file = paste0("data_indexpoints_tidy/indekspunkt_", city_number, ".rds")
)

# Read back in
# this_citys_trp_index_refyear <-
#   readr::read_rds(
#     file = paste0("data_indexpoints_tidy/indekspunkt_", city_number, ".rds")
#   )


# City index yearly----
city_index_yearly <-
  trp_toll_index_yearly |>
  dplyr::group_by(
    length_range,
    year,
    month
  ) |>
  dplyr::summarise(
    city_base_volume = sum(base_volume),
    city_calc_volume = sum(calc_volume),
    index_p = (city_calc_volume / city_base_volume - 1 ) * 100,
    n_trp = n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    year_from_to =
      dplyr::case_when(
        year == 2020 ~ "2019-2020",
        year == 2021 ~ "2020-2021",
        year == 2022 ~ "2021-2022",
        year == 2023 ~ "2022-2023"
      )
  )


## SD and SE ----
trp_toll_index_yearly_sd <-
  trp_toll_index_yearly |>
  dplyr::filter(!is.na(index)) |>
  dplyr::left_join(city_index) |>
  dplyr::mutate(
    diff = (base_volume / city_base_volume) * (index - index_p)^2,
    weight = (base_volume / city_base_volume)^2
  ) |>
  dplyr::group_by(length_range) |>
  dplyr::summarise(
    standard_deviation = sqrt((1 / (1 - sum(weight) )) * sum(diff) )
  ) |>
  dplyr::ungroup()

city_index_yearly_sd <-
  city_index_yearly |>
  dplyr::left_join(
    trp_toll_index_yearly_sd
  ) |>
  dplyr::mutate(
    year_base = year - 1,
    variance = standard_deviation^2,
    standard_error = round(standard_deviation / sqrt(n_trp), digits = 1)
  ) |>
  dplyr::select(
    -city_base_volume,
    -city_calc_volume
  ) |>
  dplyr::mutate(
    index_i = index_converter(index_p)
  )

city_index_yearly_light <-
  city_index_yearly_sd |>
  dplyr::ungroup() |>
  dplyr::filter(length_range == "lette")


## Chaining ----
# TODO: per length_range
years_1_2 <-
  city_index_yearly_light |>
  calculate_two_year_index()

years_1_3 <-
  dplyr::bind_rows(
    years_1_2,
    dplyr::slice(city_index_yearly_light, 3)
  ) |>
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_yearly_all <-
  city_index_yearly_sd |>
  dplyr::bind_rows(
    years_1_2,
    years_1_3,
    #years_1_4
  ) |>
  dplyr::mutate(
    length_range = "lette",
    year_from_to = paste0(year_base, "-", year),
    area_name = city_name
  )

readr::write_rds(
  city_index_all,
  file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".rds")
)

# For Miljøpakken (kanskje ikke det de vil ha likevel?)
# write.csv2(
#   city_index_all,
#   file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".csv")
# )


# City index monthly ----
trp_index_monthly <-
  trp_index_all |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "month"
  ) |>
  dplyr::mutate(
    month_object = lubridate::make_date(year = year, month = month)
  ) |>
  dplyr::select(
    trp_id,
    length_range,
    base_volume,
    calc_volume,
    index,
    month_object
  )

trp_toll_index_monthly <-
  toll_index_monthly |>
  dplyr::bind_rows(trp_index_monthly) |>
  dplyr::filter(
    length_range == "lette"
  ) |>
  dplyr::mutate(
    year = lubridate::year(month_object),
    month = lubridate::month(month_object)
  )

n_points_per_month <-
  trp_toll_index_monthly |>
  dplyr::group_by(
    year,
    month
  ) |>
  dplyr::summarise(n_trp = n())


# Sidetrack: for making Excel file in city_index_dataprep.R
trp_index_monthly_wide <-
  # Exception from naming convention to match same df from other cities
  trp_toll_index_monthly |>
  tidyr::complete(
    trp_id,
    year,
    month
  ) |>
  dplyr::mutate(
    month_label = lubridate::make_date(
      year = 2000,
      month = month,
      day = 1
    ) |>
      lubridate::month(label = TRUE)
  ) |>
  dplyr::select(
    trp_id,
    year,
    month_label,
    index
  ) |>
  tidyr::pivot_wider(
    names_from = "month_label",
    values_from = "index"
  ) |>
  dplyr::left_join(
    city_trps_meta,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_category_and_number,
    year,
    jan:des
  ) |>
  dplyr::arrange(
    name,
    trp_id,
    year
  )

# Back on track
city_index_monthly <-
  trp_toll_index_monthly |>
  dplyr::mutate(year = lubridate::year(month_object)) |>
  dplyr::filter(year >= 2020) |>
  dplyr::mutate(index = round(index, digits = 1)) |>
  dplyr::group_by(
    month_object,
    length_range
  ) |>
  dplyr::summarise(
    city_base_volume = sum(base_volume),
    city_calc_volume = sum(calc_volume),
    index_p = (city_calc_volume / city_base_volume - 1 ) * 100,
    n_points = n()
  ) |>
  dplyr::mutate(
    area_name = "Trondheim",
    year = lubridate::year(month_object),
    month = lubridate::month(month_object),
    period = "month",
    month_name =
      lubridate::month(month_object, label = TRUE, abbr = FALSE) |>
      stringr::str_to_title()
  )


## SD and SE ----
# To get sd, must start with pointindices and join monthly city index
pointindex_trp_toll_monthly_sd <-
  trp_toll_index_monthly |>
  #dplyr::filter(!is.na(index)) |>
  dplyr::left_join(city_index_monthly) |>
  #dplyr::filter(!is.na(base_volume_all)) |>
  dplyr::mutate(
    diff = (base_volume / city_base_volume) * (index - index_p)^2,
    weight = (base_volume / city_base_volume)^2
  ) |>
  dplyr::group_by(
    month_object,
    length_range
  ) |>
  dplyr::summarise(
    standard_deviation = sqrt((1 / (1 - sum(weight) )) * sum(diff) )
  )

city_index_monthly_sd <-
  city_index_monthly |>
  dplyr::left_join(pointindex_trp_toll_monthly_sd) |>
  dplyr::mutate(
    standard_error = round(standard_deviation / sqrt(n_points), digits = 1)
  ) |>
  dplyr::select(
    -city_base_volume,
    -city_calc_volume
  )

write.csv2(
  city_index_monthly_sd,
  file = paste0("data_indexpoints_tidy/byindeks_maanedlig_", city_number, ".csv"),
  row.names = F
)

# Check:
# pi_wide <-
#   pointindex_trp_toll_monthly |>
#   dplyr::filter(
#     length_range == "lette",
#     month_object >= "2022-01-01"
#   )


# City index so far ----
# E.g. Q1 chained through all years
trp_index_so_far <-
  trp_index_all |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "month",
    month <= index_month
  ) |>
  dplyr::mutate(
    month_object = lubridate::make_date(year = year, month = month)
  ) |>
  dplyr::select(
    trp_id,
    length_range,
    base_volume,
    calc_volume,
    index,
    month_object
  )

# Bind with toll point index and calculate so far index per point
trp_toll_index_so_far <-
  toll_index_monthly |>
  dplyr::mutate(
    month = lubridate::month(month_object)
  ) |>
  dplyr::filter(
    month <= index_month
  ) |>
  dplyr::select(
    -month
  ) |>
  dplyr::bind_rows(trp_index_so_far) |>
  dplyr::filter(
    length_range == "lette"
  ) |>
  dplyr::mutate(
    year = lubridate::year(month_object)
  ) |>
  # Summarise per TRP to get n TRP correct
  dplyr::group_by(
    trp_id,
    year,
    length_range
  ) |>
  dplyr::summarise(
    trp_base_volume = sum(base_volume),
    trp_calc_volume = sum(calc_volume),
    trp_index_p = (trp_calc_volume / trp_base_volume - 1 ) * 100,
    trp_month_object = max(month_object),
    .groups = "drop"
  )

city_index_so_far <-
  trp_toll_index_so_far |>
  dplyr::group_by(
    year,
    length_range
  ) |>
  dplyr::summarise(
    city_base_volume = sum(trp_base_volume),
    city_calc_volume = sum(trp_calc_volume),
    city_index_p = (city_calc_volume / city_base_volume - 1 ) * 100,
    n_trp = n(),
    city_month_object = max(trp_month_object),
    .groups = "drop"
  )


## SD and SE ----
# To get sd, must start with pointindices and join monthly city index
trp_index_so_far_sd <-
  trp_toll_index_so_far |>
  dplyr::filter(!is.na(trp_index_p)) |>
  dplyr::left_join(
    city_index_so_far,
    by = dplyr::join_by(year, length_range)
  ) |>
  dplyr::mutate(
    diff = (trp_base_volume / city_base_volume) * (trp_index_p - city_index_p)^2,
    weight = (trp_base_volume / city_base_volume)^2
  ) |>
  dplyr::group_by(
    city_month_object,
    length_range
  ) |>
  dplyr::summarise(
    variance = (1 / (1 - sum(weight) )) * sum(diff),
    standard_deviation = sqrt(variance),
    .groups = "drop"
  )

city_index_so_far_sd <-
  city_index_so_far |>
  dplyr::left_join(
    trp_index_so_far_sd,
    by = dplyr::join_by(length_range, city_month_object)
  ) |>
  dplyr::mutate(
    year_base = year - 1,
    month = lubridate::month(city_month_object),
    index_i = index_converter(city_index_p),
    standard_error = round(standard_deviation / sqrt(n_trp), digits = 1)
  ) |>
  dplyr::select(
    year_base,
    year,
    month,
    length_range,
    #month_object = city_month_object,
    variance,
    standard_deviation,
    n_trp,
    standard_error,
    index_i,
    index_p = city_index_p
  )


## Chaining ----
so_far_years_1_2 <-
  city_index_so_far_sd |>
  calculate_two_year_index()

so_far_years_1_3 <-
  dplyr::bind_rows(
    so_far_years_1_2,
    dplyr::slice(city_index_so_far_sd, 3)
  ) |>
  calculate_two_year_index()

so_far_years_1_4 <-
  dplyr::bind_rows(
    so_far_years_1_3,
    dplyr::slice(city_index_so_far_sd, 4)
  ) |>
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_so_far_all <-
  city_index_so_far_sd |>
  dplyr::bind_rows(
    so_far_years_1_2,
    so_far_years_1_3,
    so_far_years_1_4
  ) |>
  dplyr::mutate(
    length_range = "lette",
    year_from_to = paste0(year_base, "-", year),
    area_name = "Trondheim",
    month_object = lubridate::make_date(year = year, month = month),
    period = "year_to_date",
    month_name =
      lubridate::month(month_object, label = TRUE, abbr = FALSE) |>
      stringr::str_to_title()
  )

readr::write_rds(
  city_index_so_far_all,
  file = paste0("data_indexpoints_tidy/city_index_so_far_", city_number, ".rds")
)

# Husk å lage fil for Miljøpakken
# TODO: include in Excel file for all cities
write.csv2(
  city_index_so_far_all,
  file = paste0("data_indexpoints_tidy/city_index_so_far_", city_number, ".csv"),
  row.names = F
)


# MDT ----
# Use city_index_dataprep.R
