# Binds TRP and toll station index
# Calculates city index
# (Just for TRD - instead of the more general city_index_dataprep.R)

# IN
# TRP index monthly
# Toll station index monthly

# OUT
# City index
# Excel file

city_name <- "Trondheim"

# Get data ----
{
trp_index_20 <- get_published_pointindex_for_months_trondheim(city_number, 2020, 12)
trp_index_21 <- get_published_pointindex_for_months_trondheim(city_number, 2021, 12)
trp_index_22 <- get_published_pointindex_for_months_trondheim(city_number, 2022, 12)
trp_index_23 <- get_published_pointindex_for_months_trondheim(city_number, 2023, 12)
trp_index_24 <- get_published_pointindex_for_months_trondheim(city_number, 2024, 12)
trp_index_25 <- get_published_pointindex_for_months_trondheim(city_number, 2025, index_month)

trp_index_all <-
  dplyr::bind_rows(
    trp_index_20[[2]],
    trp_index_21[[2]],
    trp_index_22[[2]],
    trp_index_23[[2]],
    trp_index_24[[2]],
    trp_index_25[[2]]
  )
}

# Prepared toll data
# Made in bomdata_trondheim.R
{
toll_meta_data <-
  readr::read_rds(
    file = "bomdata_trondheim/trd_toll_stations.rds"
  )
# NVDB-ID
# Autopass-ID

toll_index_yearly <-
  readr::read_rds(
    "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.rds"
  ) |>
  dplyr::rename(
    index = index_p,
    length_range = class
  ) |>
  dplyr::filter(length_range != "unknown")
# Autopass-ID

toll_index_monthly <-
  readr::read_rds(
    file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser.rds",
  ) |>
  dplyr::mutate(
    month_object = lubridate::ymd(month_calc)
  ) |>
  dplyr::select(
    trp_id,
    length_range = class,
    base_volume = monthly_volume_base,
    calc_volume = monthly_volume_calc,
    index = index_p,
    month_object
  ) |>
  dplyr::filter(length_range != "unknown")
}
# Autopass-ID


# TRPs ----
# Choosing most recent version of city trps
city_trps <- trp_index_25[[1]]

# Removing Tungasletta Ystgaard (is removed from VTI, but is still in present in API before 2023)
#city_trps <-
#  city_trps[! city_trps %in% c("84826V42881")]

this_citys_trps <-
  # Note: "points" is made in city_index_check.R
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
# Autopass-ID

trd_station_type <-
  dplyr::bind_rows(
    this_citys_trps,
    toll_meta_data |>
      dplyr::mutate(
        nvdb_id = as.character(nvdb_id)
      ) |>
      dplyr::rename(
        autopass_id = trp_id,
        trp_id = nvdb_id
      )
  ) |>
  dplyr::select(
    trp_id,
    station_type
  )
# NVDB-ID

trd_station_type |>
  readr::write_rds("trd_station_type.rds")

# To match ids in MDT data
trd_toll_station_id <-
  dplyr::bind_rows(
    this_citys_trps,
    toll_meta_data
  ) |>
  dplyr::select(
    trp_id,
    name
  ) |>
  dplyr::mutate(
    trp_id = dplyr::case_when(
      name == "Ranheim" ~ "72",
      TRUE ~ trp_id
    )
  )
# Autopass-ID

trd_toll_station_id |>
  readr::write_rds("trd_toll_station_id.rds")


# AADT ----
# Fetch AADT in city_index_check.Rmd

this_citys_trps_all_adt_final <-
  readr::read_rds("index_trp_metadata/trp_960.rds") |>
  dplyr::mutate(
    trp_autopass_id =
      dplyr::case_when(
        is.na(autopass_id) ~ trp_id,
        TRUE ~ autopass_id
      )
  )
# NVDB-ID
# and hybrid columns

trp_names <-
  this_citys_trps_all_adt_final |>
  dplyr::select(
    trp_id,
    name
  )


# TRP index yearly ----
trp_toll_index_yearly <-
  trp_index_all |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE
  ) |>
  # Need latest value per year
  dplyr::group_by(year) |>
  dplyr::filter(
    period == "year_to_date",
    month == max(month)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    trp_id,
    year,
    month,
    length_range,
    base_volume,
    calc_volume,
    coverage,
    index
  ) |>
  dplyr::mutate(
    month = lubridate::make_date(year = year, month = month)
  ) |>
  dplyr::bind_rows(
    toll_index_yearly
  )
# Autopass-ID

 #  dplyr::mutate(
 #    index = round(index, digits = 1)
 #  )


# Sidetrack
# For through traffic
# trp_toll_index_yearly_through_traffic <-
#   trp_toll_index_yearly |>
#   dplyr::filter(length_range == "lette")
#
# readr::write_rds(
#   trp_toll_index_yearly_through_traffic,
#   file = "data_indexpoints_tidy/trp_index_960.rds"
# )

# End of sidetrack

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
# Autopass-ID


# Binding pointindices to all points
this_citys_trps_all_adt_final_index <-
  this_citys_trps_all_adt_final |>
  dplyr::left_join(
    trp_toll_index_yearly_short,
    by = dplyr::join_by(trp_autopass_id == trp_id)
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
  dplyr::filter(!is.na(index)) |>
  dplyr::group_by(
    length_range,
    year,
    month
  ) |>
  calculate_area_index() |>
  dplyr::mutate(
    year_base = year - 1,
    year_from_to = paste0(year - 1, "-", year),
    index_i = index_converter(index_p),
    month = lubridate::month(month)
  )


# Toy example of the weigths compared to n in calculation of standard error
# toy <-
#   tibble::tibble(
#     weights = c(0.001, 0.001, 0.997, 0.001)
#     #weights = c(0.25, 0.25, 0.25, 0.25)
#   ) |>
#   dplyr::mutate(
#     sum_of_weights = sum(weights),
#     sqrt_of_sum_of_squared_weights = sqrt(sum(weights^2)),
#     one_through_sqrt_of_n = 1 / sqrt(length(weights))
#   )
# Conclusion: The minimum value of sqrt(sum(weights^2)) is when all weights are equal and then it is the same
# as one_through_sqrt_of_n.
# Its maximum value is when one of the weights outweigh all others and its value gets close to 1.


city_index_yearly_light <-
  city_index_yearly |>
  dplyr::ungroup() |>
  dplyr::filter(length_range == "lette") |>
  dplyr::select(-standard_deviation)


## Chaining ----
# TODO: per length_range
years_1_2 <-
  city_index_yearly_light |>
  calculate_two_year_index() |>
  dplyr::mutate(index_type = "chained")

years_1_3 <-
  dplyr::bind_rows(
    years_1_2,
    dplyr::slice(city_index_yearly_light, 3)
  ) |>
  calculate_two_year_index() |>
  dplyr::mutate(index_type = "chained")

years_1_4 <-
  dplyr::bind_rows(
    years_1_3,
    dplyr::slice(city_index_yearly_light, 4)
  ) |>
  calculate_two_year_index() |>
  dplyr::mutate(index_type = "chained")

years_1_5 <-
  dplyr::bind_rows(
    years_1_4,
    dplyr::slice(city_index_yearly_light, 5)
  ) |>
  calculate_two_year_index() |>
  dplyr::mutate(index_type = "chained")

# Skipping intermediate years, adding just from first to last
city_index_yearly_all <-
  city_index_yearly_light |>
  dplyr::mutate(index_type = "direct") |>
  dplyr::bind_rows(
    years_1_2,
    years_1_3,
    years_1_4,
    years_1_5
  ) |>
  dplyr::mutate(
    length_range = "lette",
    year_from_to = paste0(year_base, "-", year),
    area_name = city_name,
    month_name_short = lubridate::month(month, label = TRUE),
    period = paste0("jan-", month_name_short),
    #ci_lower = round(index_p + stats::qt(0.025, n_trp) * standard_error, 1),
    #ci_upper = round(index_p - stats::qt(0.025, n_trp) * standard_error, 1),
    ci_lower = round(index_p - 1.96 * standard_error, 1),
    ci_upper = round(index_p + 1.96 * standard_error, 1)
  )

readr::write_rds(
  city_index_yearly_all,
  file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".rds")
)

# Read back in
# city_index_yearly_all <-
#   readr::read_rds(
#     file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".rds")
#   )

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
  dplyr::filter(length_range == "lette") |>
  dplyr::mutate(
    year = lubridate::year(month_object),
    month = lubridate::month(month_object)
  )

# n_points_per_month <-
#   trp_toll_index_monthly |>
#   dplyr::group_by(
#     year,
#     month
#   ) |>
#   dplyr::summarise(n_trp = n())


# Sidetrack: for Excel file in city_index_dataprep.R
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
    this_citys_trps_all,
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
  dplyr::filter(
    !is.na(index),
    year >= 2020
  ) |>
  dplyr::group_by(
    month_object,
    length_range
  ) |>
  calculate_area_index() |>
  dplyr::mutate(
    area_name = "Trondheim",
    year = lubridate::year(month_object),
    month = lubridate::month(month_object),
    period = "month",
    month_name =
      lubridate::month(month_object, label = TRUE, abbr = FALSE) |>
      stringr::str_to_title()
  )

# write.csv2(
#   city_index_monthly,
#   file = paste0("data_indexpoints_tidy/byindeks_maanedlig_", city_number, ".csv"),
#   row.names = F
# )

readr::write_rds(
  city_index_monthly,
  file = paste0("data_indexpoints_tidy/byindeks_maanedlig_", city_number, ".rds")
)

# Read back in



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
    base_volume = sum(base_volume),
    calc_volume = sum(calc_volume),
    index = (sum(calc_volume) / sum(base_volume) - 1 ) * 100,
    trp_month_object = max(month_object),
    .groups = "drop"
  )

city_index_so_far <-
  trp_toll_index_so_far |>
  dplyr::group_by(
    year,
    length_range
  ) |>
  dplyr::mutate(
    weight = (base_volume / sum(base_volume))
  ) |>
  dplyr::summarise(
    city_base_volume = sum(base_volume),
    city_calc_volume = sum(calc_volume),
    index_p = (city_calc_volume / city_base_volume - 1 ) * 100,
    n_trp = n(),
    standard_deviation = sqrt((1 / (1 - sum(weight^2) )) * sum(weight * (index - index_p)^2) ),
    standard_error = sqrt(sum(weight^2) * standard_deviation^2),
    city_month_object = max(trp_month_object),
    .groups = "drop"
  )|>
  dplyr::select(
    -city_base_volume,
    -city_calc_volume
  ) |>
  dplyr::mutate(
    year_base = year - 1,
    month = lubridate::month(city_month_object),
    index_i = index_converter(index_p)
  ) |>
  dplyr::select(
    year_base,
    year,
    month,
    length_range,
    #month_object = city_month_object,
    n_trp,
    standard_deviation,
    standard_error,
    index_i,
    index_p
  )


## Chaining ----
so_far_years_1_2 <-
  city_index_so_far |>
  calculate_two_year_index()

so_far_years_1_3 <-
  dplyr::bind_rows(
    so_far_years_1_2,
    dplyr::slice(city_index_so_far, 3)
  ) |>
  calculate_two_year_index()

so_far_years_1_4 <-
  dplyr::bind_rows(
    so_far_years_1_3,
    dplyr::slice(city_index_so_far, 4)
  ) |>
  calculate_two_year_index()

so_far_years_1_5 <-
  dplyr::bind_rows(
    so_far_years_1_4,
    dplyr::slice(city_index_so_far, 5)
  ) |>
  calculate_two_year_index()

so_far_years_1_6 <-
  dplyr::bind_rows(
    so_far_years_1_5,
    dplyr::slice(city_index_so_far, 6)
  ) |>
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_so_far_all <-
  city_index_so_far |>
  dplyr::bind_rows(
    so_far_years_1_2,
    so_far_years_1_3,
    so_far_years_1_4,
    so_far_years_1_5,
    so_far_years_1_6
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


# MDT ----
# Already done in city_index_dataprep.R!


# Write Excel ----
source("city_index_to_excel.R")