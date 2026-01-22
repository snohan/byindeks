## Index calculation ----
# In a production setting we'd calculate monthly index
# and so-far-this-year index. But here, we only do rolling index.

# TODO: add standard error in CMDT, based on missing days
# For now, assume no uncertainty here,
# and it probably is much smaller than the contribution from spatial TRP sampling.

index_month_values <-
  mdt_validated |>
  calculate_area_index_month(population_size)

area_index_month <- index_month_values[[1]]
link_index_month <- index_month_values[[2]]

area_index_one_year <- calculate_rolling_area_index_one_year(area_index_month)

area_index_three_years <- calculate_rolling_index_multiple_years(area_index_one_year, 3)

readr::write_rds(
  area_index_month,
  base::paste0("representativity/cmdt_index_month_", city_number, ".rds")
)

readr::write_rds(
  link_index_month,
  base::paste0("representativity/link_index_month_", city_number, ".rds")
)

list(
  area_index_one_year |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(window_years = "one"),
  area_index_three_years |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(window_years = "three")
) |>
  readr::write_rds(
    base::paste0("representativity/rolling_cmdt_index_", city_number, ".rds")
  )