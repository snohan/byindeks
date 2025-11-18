# Chained yearly index

if( so_far ) {

  city_index_years_to_chain <- city_index_full_years

}else{

  city_index_years_to_chain <-
    city_index_full_years |>
    dplyr::filter(month == 12)

}

n_index_years <- base::nrow(city_index_years_to_chain)
chain_link_rows <- base::data.frame()

if( n_index_years == 2 ) {

  chain_link_rows <- calculate_two_year_index(city_index_years_to_chain)

}

if( n_index_years > 2 ) {

  chain_link_rows <- calculate_two_year_index(city_index_years_to_chain)

  for (i in 3:n_index_years) {

    link_i <-
      dplyr::bind_rows(
        dplyr::slice_tail(chain_link_rows, n = 1),
        dplyr::slice(city_index_years_to_chain, i)
      ) |>
      calculate_two_year_index()

    chain_link_rows <-
      dplyr::bind_rows(
        chain_link_rows,
        link_i
      )

  }

}

city_index_yearly_all <-
  city_index_full_years |>
  dplyr::mutate(
    index_type = "direct"
  ) |>
  dplyr::bind_rows(chain_link_rows) |>
  dplyr::mutate(
    year_from_to = paste0(year_base, "-", year),
    area_name = city_name,
    month_name_short = lubridate::month(month, label = TRUE),
    period = paste0("jan-", month_name_short),
    index_p = round(index_p, 1),
    ci_lower = round(index_p - 1.96 * standard_error, 1),
    ci_upper = round(index_p + 1.96 * standard_error, 1)
  ) |>
  dplyr::select(
    -standard_deviation,
    -variance,
    -sum_of_squared_weights
  )

readr::write_rds(
  city_index_yearly_all,
  file = paste0("data_indexpoints_tidy/byindeks_", file_name_addition, city_number, ".rds")
)

if(so_far) {
  city_index_yearly_all_so_far <- city_index_yearly_all
}