# Preparation of data for checking and reporting

# IN
# TRP metadata (Traffic data API) made in city_index_check.Rmd
# City index for all years (Traffic data API, local CSVs for older data)
# TRP index for all years (Traffic data API, local CSVs for older data)
# MDT (Traffic data API)

# OUT
# City index
# Chained city index
# City 36 month index based on MDT
# Excel file with all data


# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
  #source("get_from_nvdb_api.R")
  source("indexpoints_tidying_functions.R")
  library(viridis)
  library(writexl)
  library(readxl)
  options(warn = -1)
  svv_background_color <- "#F5F5F5"
}


## Connection to old data ----
# Points used in each city:
# (This is only used to match old index data from before 2020)
cities_points <- read.csv2("data_points_raw/cities_points.csv")

trp_id_msnr <-
  cities_points %>%
  dplyr::select(trp_id, msnr = legacyNortrafMpn) %>%
  dplyr::distinct()


## City IDs ----
# Bergen 8952
# Buskerudbyen 1952
# Grenland 955
# Kristiansand og omegn 957 kommune 956
# Nedre Glomma 953
# Nord-Jæren 952
# Oslo 959
# Trondheim 960
# Tromsø 961
# Tromsø 2022 16952

## Trondheim has its own script, all inclusive except for MDT 36 index
#source("city_index_dataprep_trondheim_toll_stations")
# Trondheim stop

## Choose publish month ----
{
present_year <- 2023
index_month <- 12 # the one to be published now
city_number <- 952
}
# End choose

## Set time references ----
{
source("city_reference_year.R")

years_from_reference_to_today <-
  base::seq(reference_year, present_year)

last_year_month <-
  lubridate::as_date(
    paste0(
      present_year,
      "-",
      index_month,
      "-01"
    )
  )

if(!(city_number %in% c(960, 16952))){
  index_years_pre_2020 <- base::seq.int(reference_year + 1, 2019, 1)
}else{
  index_years_pre_2020 <- NULL
}

if(city_number == 8952){
  index_years_from_2020 <- base::seq.int(2019, present_year, 1)
}else{
  index_years_from_2020 <- base::seq.int(2020, present_year, 1)
}

if(city_number == 16952){
  index_years_from_2020 <- base::seq.int(2023, present_year, 1)
}

index_months_from_2020 <-
  c(
    base::rep(12, base::length(index_years_from_2020) - 1),
    index_month
  )

index_years <-
  base::seq.int(reference_year + 1, present_year, 1)

index_months <-
  c(
    base::rep(12, base::length(index_years) - 1),
    index_month
  )
}

# Fetch city indexes ----
{
city_indexes <-
  purrr::map2(
    index_years,
    index_months,
    ~ get_published_index_for_months(city_number, .x, .y)
  ) |>
  purrr::list_rbind()

# TODO: TRPs might differ from year to year!
city_trps <-
  get_published_pointindex_for_months(city_number, max(index_years), index_month)[[1]] |>
  base::sort()

city_name <- city_indexes$area_name[nrow(city_indexes)]
}

# TODO: fetch for so far this year by index month


# TRP index ----
## So far by December ----
# Still need to specify csv-files for years before 2020 to get the pointindex as they are not in API
if(!(city_number %in% c(8952, 16952))){
  trp_index_so_far_by_dec_pre_2020 <-
    purrr::map(
      index_years_pre_2020,
      ~ read_pointindex_CSV(
        paste0("data_index_raw/pointindex_", city_number, "_", .x, ".csv")
      ) |>
        dplyr::rename(
          index = 2
        ) |>
        dplyr::mutate(
          year = .x,
          month = 12
        )
    ) |>
    purrr::list_rbind() |>
    dplyr::left_join(
      trp_id_msnr,
      by = "msnr"
    ) |>
    dplyr::select(-msnr)
}

trp_index_from_2020 <-
  purrr::map2(
    index_years_from_2020,
    index_months_from_2020,
    ~ get_published_pointindex_for_months(city_number, .x, .y)[[2]]
  ) |>
  purrr::list_rbind()

trp_index_so_far_by_dec_from_2020 <-
  trp_index_from_2020 |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "year_to_date"
  ) |>
  dplyr::slice_max(
    order_by = month,
    by = c(trp_id, year)
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    base_volume,
    index = index_short
  )

if(city_number %in% c(8952, 16952)){
  trp_index_year_to_date_dec_bind <-
    dplyr::bind_rows(
      trp_index_so_far_by_dec_from_2020
    )
}else{
  trp_index_year_to_date_dec_bind <-
    dplyr::bind_rows(
      trp_index_so_far_by_dec_pre_2020,
      trp_index_so_far_by_dec_from_2020
    )
}

trp_index_year_to_date_dec <-
  trp_index_year_to_date_dec_bind |>
  dplyr::filter(!is.na(base_volume)) |>
  dplyr::group_by(
    year
  ) |>
  dplyr::mutate(
    city_base_volume = sum(base_volume),
    squared_weight = (base_volume / sum(base_volume))^2
  ) |>
  dplyr::summarise(
    n_trp = n(),
    sum_of_squared_weights = sum(squared_weight)
  )

# n_points_per_month <-
#   trp_index_monthly %>%
#   dplyr::group_by(
#     year,
#     month
#   ) %>%
#   dplyr::summarise(n_trp = n())


## So far by index month ----
# Only relevant if index month is not 12!
# TODO: chained index - later!
# TODO: OLD VTI VERSION: fetch file given month for so far this year index
# As of now the csv files are implicitly containing index values for December's "so far".

# TODO: trp_index_so_far_by_index_month_pre_2020

trp_index_so_far_by_index_month_from_2020 <-
  trp_index_from_2020 |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "year_to_date",
    month == index_month
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    base_volume,
    index = index_short
  )

trp_index_year_to_date_by_index_month <-
  dplyr::bind_rows(
    #trp_index_so_far_by_index_month_pre_2020,
    trp_index_so_far_by_index_month_from_2020
  ) |>
  dplyr::filter(!is.na(base_volume)) |>
  dplyr::group_by(
    year
  ) |>
  dplyr::mutate(
    city_base_volume = sum(base_volume),
    squared_weight = (base_volume / sum(base_volume))^2
  ) |>
  dplyr::summarise(
    n_trp = n(),
    sum_of_squared_weights = sum(squared_weight)
  )


## Monthly ----
# For Excel report
if(!(city_number %in% c(8952, 16952))){
  trp_index_monthly_pre_2020 <-
    purrr::map_dfr(
      index_years_pre_2020,
      ~ read_old_pointindex_csv_monthly(
        paste0("data_index_raw/pointindex_", city_number, "_", .x, ".csv")
      ) |>
      dplyr::mutate(
        year = .x
      )
    ) |>
    dplyr::left_join(
      trp_id_msnr,
      by = "msnr"
    ) |>
    dplyr::select(-msnr)
}

trp_index_monthly_from_2020 <-
  trp_index_from_2020 |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "month"
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    index = index_short
  )

if(city_number %in% c(8952, 16952)){
  trp_index_monthly <-
    dplyr::bind_rows(
      trp_index_monthly_from_2020
    )
}else{
  trp_index_monthly <-
    dplyr::bind_rows(
      trp_index_monthly_pre_2020,
      trp_index_monthly_from_2020
    )
}

# Solely for Excel
trp_index_monthly_wide <-
  trp_index_monthly |>
  #trp_toll_index_monthly |>
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
    -month
  ) |>
  tidyr::pivot_wider(
    names_from = "month_label",
    values_from = "index"
  ) |>
  dplyr::left_join(
    points,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_category_and_number,
    year,
    jan:des
    #jan:okt
  ) |>
  dplyr::arrange(
    name,
    trp_id,
    year
  )


## TRP names ----
this_citys_trps_all_adt_final <-
  readr::read_rds(
    file = paste0(
      "index_trp_metadata/trp_",
      city_number,
      ".rds"
    )
  )

trp_names <-
  this_citys_trps_all_adt_final |>
  dplyr::select(
    trp_id,
    name,
    municipality_name
  )

sub_areas <-
  trp_names |>
  dplyr::select(
    trp_id,
    sub_area = municipality_name
  )


## Chained pointindex from reference year
# TODO: drop this
# trp_index_from_refyear <-
#   this_citys_trp_index %>%
#   dplyr::select(
#     trp_id,
#     tidyselect::starts_with("index")
#   ) %>%
#   dplyr::filter(
#     dplyr::if_all(
#       .cols = tidyselect::starts_with("index"),
#       .fns = ~ !is.na(.x)
#     )
#   ) %>%
#   dplyr::mutate(
#     dplyr::across(
#       .cols = tidyselect::starts_with("index"),
#       .fns = ~ index_converter(.))
#   ) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(index = prod(c_across(tidyselect::starts_with("index")))) %>%
#   dplyr::mutate(index = round(100 * (index - 1), digits = 1)) %>%
#   dplyr::select(trp_id, index)
#
# this_citys_trp_index_refyear <-
#   this_citys_trp_index %>%
#   dplyr::left_join(trp_index_from_refyear)


# readr::write_rds(
#   #this_citys_trp_index_refyear,
#   this_citys_trp_index,
#   file = paste0("data_indexpoints_tidy/indekspunkt_", city_number, ".rds")
# )


# City index ----
city_index_full_years <-
  city_indexes |>
  dplyr::filter(
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    period == "year_to_date"
  ) |>
  dplyr::slice_max(
    order_by = month,
    by = year
  ) |>
  dplyr::left_join(
    trp_index_year_to_date_dec,
    by = "year"
  ) |>
  dplyr::mutate(
    year_base = year - 1,
    index_i = index_converter(index_p),
    variance = standard_deviation^2,
    #standard_error = base::round(sqrt(sum_of_squared_weights) * standard_deviation, digits = 1)
    standard_error = sqrt(sum_of_squared_weights) * standard_deviation
  ) %>%
  dplyr::select(
    year_base,
    year,
    month,
    index_p,
    index_i,
    standard_deviation,
    variance,
    n_trp,
    standard_error,
    sum_of_squared_weights
  ) %>%
  dplyr::arrange(year)


## Chained city index ----
# TODO: Functionize!
#calculate_each_index_from_reference_year <- function(city_index_df) {
  # In order to calculate SD and SE, do it iteratively for two years at a time
#  city_index_n_years <- nrow(city_index)
#  ref_year <- min(city_index$year_base)
#}

years_1_2 <-
  calculate_two_year_index(city_index_full_years)

years_1_3 <-
  bind_rows(years_1_2, slice(city_index_full_years, 3)) %>%
  calculate_two_year_index()

years_1_4 <-
  bind_rows(years_1_3, slice(city_index_full_years, 4)) %>%
   calculate_two_year_index()

years_1_5 <-
  bind_rows(years_1_4, slice(city_index_full_years, 5)) %>%
  calculate_two_year_index()

years_1_6 <-
  bind_rows(years_1_5, slice(city_index_full_years, 6)) %>%
  calculate_two_year_index()

years_1_7 <-
  bind_rows(years_1_6, slice(city_index_full_years, 7)) %>%
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last?
city_index_yearly_all <-
  city_index_full_years |>
  dplyr::mutate(
    index_type = "direct"
  ) |>
  dplyr::bind_rows(
    # Include only for full years
    years_1_2,
    years_1_3,
    years_1_4,
    years_1_5,
    years_1_6,
    #years_1_7
  ) %>%
  dplyr::mutate(
    year_from_to = paste0(year_base, "-", year),
    area_name = city_name,
    month_name_short = lubridate::month(month, label = TRUE),
    period = paste0("jan-", month_name_short),
    ci_lower = round(index_p - 1.96 * standard_error, 1),
    ci_upper = round(index_p + 1.96 * standard_error, 1)
    #ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error, 1),
    #ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error, 1)
  ) |>
  dplyr::select(
    -standard_deviation,
    -variance,
    -sum_of_squared_weights
  )

readr::write_rds(
  city_index_yearly_all,
  file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".rds")
)


# City index monthly
# city_monthly <-
#   dplyr::bind_rows(
#     monthly_city_index(city_index_2017),
#     monthly_city_index(city_index_2018),
#     monthly_city_index(city_index_2019),
#     monthly_city_index(city_index_2020),
#     monthly_city_index(city_index_2021),
#     monthly_city_index(city_index_2022)
#   ) %>%
#   dplyr::select(
#     area_name,
#     year,
#     month,
#     period,
#     month_object,
#     month_name,
#     index_p,
#     standard_deviation,
#     confidence_width,
#     base_volume,
#     calc_volume
#   ) %>%
#   dplyr::left_join(
#     n_points_per_month,
#     by = c("year", "month")
#   ) %>%
#   dplyr::mutate(
#     standard_error = round(standard_deviation / sqrt(n_points), digits = 1)
#   )
#
# write.csv2(
#   city_monthly,
#   file =
#     paste0(
#       "data_indexpoints_tidy/byindeks_maanedlig_",
#       city_number,
#       ".csv"
#     ),
#   row.names = F
# )


# Rolling index ----

## Get MDTs ----
mdt <-
  purrr::map_dfr(
    years_from_reference_to_today,
    ~ get_mdt_by_length_for_trp_list(city_trps, .x)
  )

# How many MDTs in reference year from NorTraf?
# n_nortraf_mdt_reference_year <-
#   mdt |>
#   dplyr::filter(
#     length_range == "[..,5.6)",
#     year == reference_year
#   ) |>
#   dplyr::mutate(
#     is_nortraf = dplyr::if_else(is.na(coverage), 1, 0)
#   ) |>
#   dplyr::group_by(
#     trp_id
#   ) |>
#   dplyr::summarise(
#     n_months_nortraf = sum(is_nortraf)
#   )
#
# n_valid_mdt_reference_year <-
#   mdt_validated |>
#   dplyr::filter(
#     year == reference_year
#   ) |>
#   dplyr::mutate(
#     valid_quality = coverage >= 50 & length_quality >= 99
#   ) |>
#   dplyr::filter(
#     valid_quality == TRUE
#   ) |>
#   dplyr::group_by(
#     trp_id
#   ) |>
#   dplyr::summarise(
#     n_months_good_quality = n()
#   )
#
# this_citys_trp_nortraf_reference_year <-
#   points |>
#   dplyr::filter(trp_id %in% city_trps) |>
#   split_road_system_reference() |>
#   dplyr::select(
#     trp_id,
#     name,
#     road_category_and_number
#   ) |>
#   dplyr::left_join(
#     n_valid_mdt_reference_year,
#     by = "trp_id"
#   ) |>
#   dplyr::left_join(
#     n_nortraf_mdt_reference_year,
#     by = "trp_id"
#   ) |>
#   dplyr::filter(
#     n_months_good_quality >= 10,
#     n_months_nortraf > 4
#   )

# write.csv2(
#   this_citys_trp_nortraf_reference_year,
#   file = "nortraf_buskerudbyen.csv",
#   row.names = F,
#   fileEncoding = "latin1"
# )

# TRD
if(city_number == 960) {
  toll_mdt_light <-
    readr::read_rds(
      file = "data_indexpoints_tidy/trd_toll_mdt.rds",
    ) |>
    dplyr::filter(
      class == "lette"
    ) |>
    dplyr::rename(
      year_month = month
    ) |>
    dplyr::mutate(
      year = lubridate::year(year_month),
      month = lubridate::month(year_month),
      length_quality = 100
    ) |>
    dplyr::select(
      -class,
      -n_days,
      -traffic
    )
  # TRD stop
}

# mdt_test <-
#   mdt |>
#   dplyr::filter(
#     length_range == "[..,5.6)",
#     is.na(sd_length_range)
#   )

mdt_filtered <-
  mdt |>
  dplyr::filter(
    length_range == "[..,5.6)"
  ) |>
  dplyr::mutate(
    mdt_valid_length = dplyr::case_when(
     is.na(total_coverage) ~ mdt_total, # If NorTraf, assume high quality
     TRUE ~ mdt_valid_length
    ),
    length_quality = mdt_valid_length / mdt_total * 100,
    coverage = dplyr::case_when(
      is.na(total_coverage) ~ 100, # If NorTraf, assume high quality
      TRUE ~ total_coverage * length_quality / 100
    )
  ) |>
  dplyr::left_join(
    trp_names,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    mdt = mdt_length_range,
    coverage,
    length_quality,
    #sub_area = municipality_name
  ) |>
  dplyr::mutate(
    year_month = lubridate::as_date(
      paste0(
        year,
        "-",
        month,
        "-01"
      )
    )
  ) |>
  tibble::as_tibble()

if(city_number == 960){
  mdt_filtered <-
    mdt_filtered |>
    dplyr::bind_rows(toll_mdt_light)
}

mdt_filtered |>
  readr::write_rds(
    file =
      paste0(
        "data_indexpoints_tidy/mdt_",
        city_number,
        ".rds"
      )
  )

# Read back in
# mdt_filtered <-
#   readr::read_rds(
#     paste0(
#       "data_indexpoints_tidy/mdt_",
#       city_number,
#       ".rds"
#     )
#   )


## Check MDT validity ----
# Exclude trp-months
#source("exclude_trp_mdts.R")
source("exclude_trp_mdts_list.R")


## Check MDT ----
trp_mdt_ok_refyear <-
  mdt_validated |>
  dplyr::filter(
    trp_id %in% city_trps # avoid toll stations appearing here as they've already been checked
  ) |>
  filter_mdt(reference_year) |>
  purrr::pluck(1)

# TODO: define sections
# TODO: look at sectional TRPs concurrently
# TODO: in map, draw curve connecting sectional TRPs

# TODO: correlate exclusions of TRP index and MDT
# TODO: show TRP contributions to rolling indices
# TODO: Shiny app for checking MDT

mdt_validated |>
  dplyr::filter(
    trp_id %in% trp_mdt_ok_refyear[16:17]
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    mdt,
    coverage,
    length_quality
  ) |>
  tidyr::complete(
    trp_id,
    year,
    month,
    fill = list(
      mdt = 0,
      coverage = 0,
      length_quality = 0
    )
  ) |>
  dplyr::mutate(
    month_object =
      lubridate::make_date(
        year = 2000,
        month = month,
        day = 1
      )
  ) |>
  dplyr::left_join(
    points,
    by = "trp_id"
  ) |>
  dplyr::mutate(
    road_category_and_number_and_point_name =
      paste0(
        road_category_and_number,
        " ",
        name
      )
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    mdt,
    coverage,
    length_quality,
    month_object,
    road_category_and_number_and_point_name
  ) |>
  dplyr::mutate(
    valid_quality = coverage >= 50 & length_quality >= 98.5
  ) |>
  create_mdt_barplot()


## Seeing all TRPs MDTs simultaneously ----
mean_mdt_ref_year_per_trp <-
  mdt_validated |>
  filter_mdt(reference_year) |>
  dplyr::select(
    trp_id,
    mean_mdt_ref_year = mean_mdt
  )

mean_mdt_ref_year_all_trp <-
  mdt_validated |>
  dplyr::filter(
    trp_id %in% mean_mdt_ref_year_per_trp$trp_id,
    year == reference_year,
    coverage >= 50, # this is length_coverage!
    length_quality >= 98.5
  ) |>
  dplyr::select(
    trp_id,
    month,
    mdt
  ) |>
  # Need to have a value in every month for all TRPs
  tidyr::complete(
    trp_id,
    month
  ) |>
  dplyr::left_join(
    mean_mdt_ref_year_per_trp,
    by = dplyr::join_by(trp_id)
  ) |>
  # Using the TRPs own mean if it is missing a month
  dplyr::mutate(
    mdt_ref_year =
      dplyr::case_when(
        !is.na(mdt) ~ mdt,
        TRUE ~ mean_mdt_ref_year
      )
  ) |>
  dplyr::group_by(month) |>
  dplyr::mutate(
    mean_mdt_month_area = mean(mdt_ref_year)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-mdt, -mean_mdt_ref_year)


mdts_last_36_months <-
  mdt_validated |>
  dplyr::select(
    trp_id,
    year,
    month,
    year_month,
    mdt
  ) |>
  dplyr::filter(
    trp_id %in% mean_mdt_ref_year_per_trp$trp_id,
    year_month > last_year_month - months(36)
  ) |>
  dplyr::left_join(
    mean_mdt_ref_year_all_trp,
    by = dplyr::join_by(trp_id, month)
  ) |>
  dplyr::arrange(
    year_month,
    trp_id
  ) |>
  dplyr::mutate(
    mdt_relative = mdt / mdt_ref_year,
    mdt_abs_diff = mdt - mdt_ref_year,
    #mdt_relative_log = log(mdt / mdt_ref_year) |> abs() |> exp(),
    mdt_abs_diff_normalized = mdt_abs_diff / mean_mdt_month_area
    # This normalized diff should be comparable on sections, but other trends may complicate the picture.
  ) |>
  dplyr::left_join(
    trp_names,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    name,
    year_month,
    mdt_abs_diff,
    mdt_abs_diff_normalized
  )


plot_mdt_comparisons <-
  mdts_last_36_months |>
  ggplot2::ggplot(aes(x = year_month, y = mdt_abs_diff, color = name)) +
  geom_line() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = svv_background_color),
    panel.background = element_rect(fill = svv_background_color)
  )

plot_mdt_comparisons |>
  plotly::ggplotly()

## Compare exclusions of MDT and index ----
# Check that the "same" exclusions are used on PI as MDT
# TRD toll station MDTs already have the same exclusions

# pointindices_longformat_by_month <-
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
#     length_range == "lette"
#   ) |>
#   dplyr::group_by(year) %>%
#   dplyr::filter(
#     period == "month"
#   ) %>%
#   dplyr::select(
#     trp_id,
#     year,
#     month,
#     index
#   )

mdt_and_pi <-
  mdt_validated |>
  dplyr::filter(
    coverage >= 50,
    length_quality >= 98.5
  ) |>
  dplyr::left_join(
    trp_index_monthly,
    by = c("trp_id", "year", "month"),
    #dplyr::select(trp_toll_index_monthly, -year, -month, -length_range), # TRD
    #by = c("trp_id", "year_month" = "month_object") # TRD
  ) |>
  dplyr::left_join(
    trp_names,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    name,
    year,
    month,
    mdt_coverage = coverage,
    length_quality,
    mdt,
    index
  ) |>
  dplyr::arrange(
    name,
    trp_id,
    year,
    month
  )


mdt_and_pi_check <-
  mdt_and_pi |>
  dplyr::select(
    name,
    year,
    month,
    mdt,
    index
  ) |>
  tidyr::complete(
    name,
    year,
    month
  ) |>
  dplyr::mutate(
    month_label = lubridate::make_date(
      year = 2000,
      month = month,
      day = 1
    ) |>
      lubridate::month(label = TRUE),
    valid_value =
      dplyr::case_when(
        is.na(mdt) & is.na(index) ~ "",
        is.na(mdt) & !is.na(index) ~ "index",
        !is.na(mdt) & is.na(index) ~ "mdt",
        !is.na(mdt) & !is.na(index) ~ "BOTH",
      )
  ) |>
  dplyr::select(
    -month,
    -mdt,
    -index
  ) |>
  tidyr::pivot_wider(
    names_from = "month_label",
    values_from = "valid_value"
  )


## TRP MDT table ----
# To show MDTs in a table in the report
# mdt_each_year <-
#   purrr::map_dfr(
#     years_from_reference_to_today,
#     ~ filter_mdt(mdt_filtered, .x)
#   ) |>
#   dplyr::select(
#     -n_months
#   ) |>
#   tidyr::pivot_wider(
#     names_from = year,
#     names_prefix = "mdt_",
#     values_from = mean_mdt
#   )
#
# city_trps_mdt <-
#   points %>%
#   dplyr::filter(trp_id %in% city_trps) %>%
#   split_road_system_reference() %>%
#   dplyr::select(
#     trp_id,
#     name,
#     road_category_and_number
#   ) %>%
#   dplyr::left_join(
#     mdt_each_year,
#     by = "trp_id"
#   ) |>
#   dplyr::mutate(
#     dplyr::across(
#       tidyselect::starts_with("mdt_"),
#       ~ round(
#         .x,
#         digits = -1
#         )
#     )
#   )
#
# readr::write_rds(
#   city_trps_mdt,
#   file =
#     paste0(
#       "data_indexpoints_tidy/city_trps_mdt_",
#       city_number,
#       ".rds"
#     )
# )


## All possible window indices ----
all_12_month_indices <-
  calculate_rolling_indices(12)

all_24_month_indices <-
  calculate_rolling_indices(24)

all_36_month_indices <-
  calculate_rolling_indices(36)


list(
  all_12_month_indices,
  all_24_month_indices,
  all_36_month_indices
) |>
  readr::write_rds(
    file =
      paste0(
        "data_indexpoints_tidy/rolling_indices_",
        city_number,
        ".rds"
      )
  )

all_36_month_trp_indices <-
  calculate_rolling_indices(36, "by_trp") |>
  dplyr::left_join(
    trp_names,
    by = "trp_id"
  ) |>
  dplyr::mutate(
    trp_index_p = (trp_index_i - 1) * 100,
    index_p = (index_i - 1) * 100
  ) |>
  dplyr::select(
    trp_id,
    name,
    #municipality_name,
    #reference_year = year,
    last_month_in_index = month_object,
    index_period,
    n_months_reference_year = n_months.x,
    mean_mdt_reference_year = mean_mdt.x,
    n_months_in_index_period = n_months.y,
    mean_mdt_index_period = mean_mdt.y,
    trp_index_p,
    area_index_p = index_p
  ) |>
  dplyr::arrange(
    last_month_in_index,
    name,
    trp_id
  )

# Sub area
sub_area_36_month_trp_indices <-
  calculate_rolling_indices(36, "by_sub_area")

writexl::write_xlsx(
  sub_area_36_month_trp_indices,
  path = paste0(
    "data_indexpoints_tidy/sub_area_rolling_indices_",
    city_number,
    ".xlsx"
  )
)


# Check contribution from TRPs each possible 36 month index
trp_mdt_plot <-
  all_36_month_trp_indices |>
  ggplot2::ggplot(
    aes(
      x = last_month_in_index,
      y = name,
      fill = trp_index_p
    )
  ) +
  geom_tile() +
  theme_minimal() +
  labs(
    x = "",
    y = ""
  )

trp_mdt_plot |>
  plotly::ggplotly()


# TRP data to Excel ----
# For those interested in the details
list(
  punkt_adt = this_citys_trps_all_adt_final,
  #punktindeks_maned = trp_index_monthly_wide,
  #punktindeks_ar = this_citys_trp_index_refyear, # drop
  byindeks_aarlig = city_index_yearly_all,
  #byindeks_maanedlig = city_index_monthly,
  punkt_mdt = mdt_and_pi,
  punkt_3_aar_glid_indeks = all_36_month_trp_indices,
  by_3_aar_glid_indeks = all_36_month_indices,
  by_2_aar_glid_indeks = all_24_month_indices,
  by_1_aar_glid_indeks = all_12_month_indices
  # TRD
  #,byindeks_hittil = city_index_so_far_all
) |>
writexl::write_xlsx(
  path = paste0(
    "data_indexpoints_tidy/tallmateriale_",
    city_number,
    ".xlsx"
  )
)


# E18 Buskerudbyen ----
trps_e18 <- c("08879V180819", "17291V181259")

point_index_e18 <-
  dplyr::bind_rows(
    get_pointindices_for_trp_list(trps_e18, 2017),
    get_pointindices_for_trp_list(trps_e18, 2018),
    get_pointindices_for_trp_list(trps_e18, 2019),
    get_pointindices_for_trp_list(trps_e18, 2020),
    get_pointindices_for_trp_list(trps_e18, 2021),
    get_pointindices_for_trp_list(trps_e18, 2022),
    get_pointindices_for_trp_list(trps_e18, 2023)
  ) %>%
  dplyr::filter(
    day_type == "ALL",
    period == "year_to_date"
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::filter(month == max(month))

trps_e18_index <-
  points |>
  dplyr::filter(trp_id %in% trps_e18) |>
  dplyr::left_join(
    point_index_e18,
    by = "trp_id"
  )

write.csv2(
  trps_e18_index,
  file = "data_indexpoints_tidy/buskerudbyen_e18_punktindekser.csv",
  row.names = F
)


# Combining a direct index with rolling indexes ----
## Nord-Jæren test ----
chain_start_year_from_to <- "2017-2019"

city_index_njaeren_2017_2019_official <-
  city_info |>
  dplyr::filter(
    year_from_to == chain_start_year_from_to
  )

chain_link_index_i <- city_index_njaeren_2017_2019_official$index_i
chain_link_se_p <- city_index_njaeren_2017_2019_official$standard_error

all_36_month_indices_combined <-
  all_36_month_indices |>
  dplyr::select(
    index_period,
    month_object,
    month_n,
    year,
    window,
    n_trp,
    index_i,
    standard_error_p
  ) |>
  dplyr::mutate(
    index_period =
      paste0(
        stringr::str_sub(chain_start_year_from_to, 1, 4),
        stringr::str_sub(index_period, 5, -1)
      ),
    chained_index_i = index_i * chain_link_index_i,
    index_p = (chained_index_i - 1) * 100,
    standard_error =
      100 * sqrt(
        index_i^2 * 1e-4 * chain_link_se_p^2 +
          chain_link_index_i^2 * 1e-4 * standard_error_p^2 +
          1e-4 * chain_link_se_p^2 * 1e-4 * standard_error_p^2
      ),
    ci_lower = round(index_p - 1.96 * standard_error, 1),
    ci_upper = round(index_p + 1.96 * standard_error, 1)
  ) |>
  dplyr::select(
    index_period,
    month_object,
    month_n,
    year,
    window,
    n_trp,
    index_i = chained_index_i,
    index_p,
    standard_error,
    ci_lower,
    ci_upper
  )


readr::write_rds(
  all_36_month_indices_combined,
  file =
    paste0(
      "data_indexpoints_tidy/combined_indices_",
      city_number,
      ".rds"
    )
)


## Tromsø ----
chain_start_year_from_to <- "2019-2022"

city_index_tromso_2019_2022 <-
  readr::read_rds(
    "data_indexpoints_tidy/city_index_tromso_2019_2022.rds"
  ) |>
  dplyr::mutate(
    index_i = index_converter(index_p),
    period_build = "direct",
    months = "jan-des"
  )

chain_link_se_p <- city_index_tromso_2019_2022$standard_error

### Yearly index ----
city_index_chained <-
  city_index_yearly_all |>
  dplyr::select(
    index_period = year_from_to,
    year,
    month_n = month,
    n_trp,
    index_i,
    standard_error_p = standard_error
  ) |>
  dplyr::mutate(
    index_period =
      paste0(
        stringr::str_sub(chain_start_year_from_to, 1, 4),
        stringr::str_sub(index_period, 5, -1)
      ),
    chained_index_i = index_i * city_index_tromso_2019_2022$index_i,
    index_p = (chained_index_i - 1) * 100,
    standard_error =
      100 * sqrt(
        index_i^2 * 1e-4 * chain_link_se_p^2 +
          city_index_tromso_2019_2022$index_i^2 * 1e-4 * standard_error_p^2 +
          1e-4 * chain_link_se_p^2 * 1e-4 * standard_error_p^2
      ),
    ci_lower = round(index_p - 1.96 * standard_error, 1),
    ci_upper = round(index_p + 1.96 * standard_error, 1),
    period_build = "chained",
    period = "jan-des"
  ) |>
  dplyr::select(
    index_period,
    period_build,
    period,
    index_i = chained_index_i,
    index_p,
    standard_error,
    ci_lower,
    ci_upper
  )

city_index_final <-
  dplyr::bind_rows(
    city_index_tromso_2019_2022 |>
      dplyr::select(
        index_period = period,
        period = months,
        n_trp,
        period_build,
        index_i,
        index_p,
        standard_error,
        ci_lower,
        ci_upper
      ),
    city_index_yearly_all |>
      dplyr::select(
        index_period = year_from_to,
        period_build = index_type,
        period,
        n_trp,
        index_i,
        index_p,
        standard_error,
        ci_lower,
        ci_upper
      ),
    city_index_chained
  ) |>
  dplyr::mutate(
    area_name = city_name
  )

readr::write_rds(
  city_index_final,
  file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".rds")
)


### Rolling index ----





## Theory ----
# Is the product of two normal variables still normal when means are close to 1 and with small deviation?
# Seems so
# library(extraDistr)
# n1 = extraDistr::rlst(1e4, 10, 1, .025)
# n2 = extraDistr::rlst(1e4, 10, 1, .025)
# #n1 <- rnorm(10000,1,.005)
# #n2 <- rnorm(10000,1,.005)
# n  <- n1*n2
# d  <- density(n)
# plot(d,lwd=2)
# x  <- par('usr')
# dn <- dnorm(d$x,mean=mean(n),sd=sd(n))
# x  <- seq(x[1],x[2],length.out=length(dn))
# lines(x, dn ,col=2, lwd=2)
# legend('topright', legend=c('Estimated density', 'Normal
#     distribution'), lwd=2, lty=c(1,1),col=c(1,2))
