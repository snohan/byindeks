# Source ----
source("rmd_setup.R")
source("get_from_trafficdata_api.R")
source("index_report_functions.R")


# Index codes and years ----
last_complete_year <- 2022
last_complete_month_this_year <- 5

index_codes_and_reference_years <-
  tibble::tibble(
    index_code =
      c(
      4953,  # Grenland
      6953,  # Oslo
      9952,  # Førde
      12952, # Vestfold
      5953,  # Nedre Glomma
      5952,  # Bergen
      6952,  # Nord-Jæren
      11952  # Tromsø
    ),
    reference_year =
      c(
        2017,
        2017,
        2017,
        2017,
        2018,
        2018,
        2019,
        2021
      )
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    index_years = list(c((reference_year + 1):last_complete_year))
  ) |>
  dplyr::ungroup()

index_codes_and_index_years <-
  index_codes_and_reference_years |>
  dplyr::select(
    index_code,
    index_years
  ) |>
  tidyr::unnest(index_years)


# City index ----
bike_indexes_complete <-
  purrr::map2(
    index_codes_and_index_years$index_code,
    index_codes_and_index_years$index_years,
    ~ get_published_index_for_months(.x, .y, 12)
  ) |>
  purrr::list_rbind()

bike_indexes_so_far_this_year <-
  purrr::map(
    index_codes_and_reference_years$index_code,
    ~ get_published_index_for_months(., last_complete_year + 1, last_complete_month_this_year)
  ) |>
  purrr::list_rbind()

bike_indexes <-
  dplyr::bind_rows(
    bike_indexes_complete,
    bike_indexes_so_far_this_year
  ) |>
  dplyr::filter(
    length_range == "[..,..)",
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG"
  ) |>
  dplyr::select(
    #index_code = publishedAreaTrafficVolumeIndex.id,
    area_name,
    year,
    month,
    period,
    index_p,
    index_i,
    base_volume,
    standard_deviation
  ) |>
  dplyr::mutate(
    month_object = lubridate::make_date(year = year, month = month),
    month_object_2000 = lubridate::make_date(year = 2000, month = month),
    month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)
  )


# TRP index ----
# Need n TRP and base volume for weighting
bike_trp_indexes_complete <-
  purrr::map2(
    index_codes_and_index_years$index_code,
    index_codes_and_index_years$index_years,
    ~ get_published_bikepointindex_for_months(.x, .y, 12)[[2]]
  ) |>
  purrr::list_rbind()

bike_trp_indexes_so_far_this_year <-
  purrr::map(
    index_codes_and_reference_years$index_code,
    ~ get_published_bikepointindex_for_months(., last_complete_year + 1, last_complete_month_this_year)[[2]]
  ) |>
  purrr::list_rbind()

bike_trp_indexes <-
  dplyr::bind_rows(
    bike_trp_indexes_complete,
    bike_trp_indexes_so_far_this_year
  ) |>
  dplyr::filter(
    is_excluded == FALSE,
    is_manually_excluded == FALSE
  ) |>
  dplyr::group_by(
    area_name,
    year,
    month,
    period
  ) |>
  dplyr::mutate(
    sum_base_volume = sum(base_volume),
    squared_weight = (base_volume / sum_base_volume)^2
  ) |>
  dplyr::summarise(
    n_trp = n(),
    sum_of_squared_weights = sum(squared_weight),
    .groups = "drop"
  )

# City and n TRP ----
bike_indexes_all <-
  dplyr::left_join(
    bike_indexes,
    bike_trp_indexes,
    by = dplyr::join_by(area_name, year, month, period)
  ) |>
  dplyr::mutate(
    area_name = dplyr::case_when(
      area_name == "Oslo" ~ "Osloområdet",
      TRUE ~ area_name
    ),
    standard_error = sqrt(sum_of_squared_weights) * standard_deviation,
    ci_lower = round(index_p + stats::qt(0.025, n_trp) * standard_error, 1),
    ci_upper = round(index_p - stats::qt(0.025, n_trp) * standard_error, 1)
  ) |>
  dplyr::select(
    -standard_deviation,
    -sum_of_squared_weights
  ) |>
  dplyr::arrange(
    area_name,
    year,
    month,
    period
  )



# Chained index ----
## Complete years ----
bike_index_complete_years <-
  bike_indexes_all |>
  dplyr::filter(
    period == "year_to_date",
    month == 12
  ) |>
  dplyr::select(
    area_name,
    year,
    month,
    index_i,
    n_trp,
    standard_error
  ) |>
  dplyr::mutate(
    year_base = year - 1,
    index_type = "direct"
  )

test_2 <-
  bike_index_complete_years |>
  dplyr::group_by(area_name) |>
  # dplyr::filter(
  #   area_name == "Bergen"
  # ) |>
  calculate_index_chain()

test_3 <-
  bike_index_complete_years |>
  tidyr::nest(.by = area_name) |>
  dplyr::mutate(
    chained_dfs = purrr::map(data, ~ calculate_index_chain(.x))
  )


test_4 <- test_3$data[[1]]  |>
  calculate_index_chain()

## So far this year ----


# Write ----
readr::write_rds(
  bike_indexes_all,
  file = "data_indexpoints_tidy/bike_indexes_all.rds"
)
