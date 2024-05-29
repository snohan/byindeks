# City index by area

# Bergen
# Starting from trp_index in city_index_data_prerp.R
trp_index_tidy <-
  trp_index_from_2020 |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "year_to_date",
    year %in% c(2020:2023)
  ) |>
  dplyr::slice_max(
    order_by = month,
    by = c(trp_id, year)
  ) |>
  dplyr::select(
    trp_id,
    year,
    length_base_volume_short,
    length_calc_volume_short,
    index = index_short
  ) |>
  dplyr::filter(
    length_base_volume_short != 0
  )


# Test
city_index_from_trp_index <-
  trp_index_tidy |>
  dplyr::summarise(
    index_i = sum(length_calc_volume_short) / sum(length_base_volume_short),
    index_p = 100 * (index_i - 1),
    n_trp = n(),
    .by = year
  )
# Yields same results! :)

# Per municipality
city_index_from_trp_index_per_municipality <-
  trp_index_tidy |>
  dplyr::left_join(
    this_citys_trps_all_adt_final,
    by = join_by(trp_id)
  ) |>
  dplyr::mutate(
    bergen = municipality_name == "Bergen"
  ) |>
  dplyr::summarise(
    index_i = sum(length_calc_volume_short) / sum(length_base_volume_short),
    index_p = 100 * (index_i - 1),
    n_trp = n(),
    .by = c(year, bergen)
  ) |>
  dplyr::arrange(bergen)

city_index_from_trp_index_per_municipality_chained <-
  city_index_from_trp_index_per_municipality |>
  dplyr::summarise(
    chained_index_i = prod(index_i),
    n_years = n(),
    .by = bergen
  ) |>
  dplyr::mutate(
    index_p = 100 * (chained_index_i - 1),
    years = "2019-2023"
  )

library(writexl)

list(
  "alle" = city_index_full_years,
  "bergen" = city_index_from_trp_index_per_municipality,
  "2018-2023" = city_index_from_trp_index_per_municipality_chained
) |>
  writexl::write_xlsx("spesialuttak/bergen_kommune_indeks.xlsx")
