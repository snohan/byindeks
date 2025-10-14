trps_e18 <- c("08879V180819", "17291V181259")

point_index_e18 <-
  dplyr::bind_rows(
    get_pointindices_for_trp_list(trps_e18, 2017),
    get_pointindices_for_trp_list(trps_e18, 2018),
    get_pointindices_for_trp_list(trps_e18, 2019),
    get_pointindices_for_trp_list(trps_e18, 2020),
    get_pointindices_for_trp_list(trps_e18, 2021),
    get_pointindices_for_trp_list(trps_e18, 2022),
    get_pointindices_for_trp_list(trps_e18, 2023),
    get_pointindices_for_trp_list(trps_e18, 2024),
    get_pointindices_for_trp_list(trps_e18, 2025)
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
  ) |>
  dplyr::rename(
    index_short_p = index_short
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    index_short_i = 1 + (index_short_p / 100)
  ) |>
  dplyr::mutate(
    chained_index = base::cumprod(index_short_i),
    .by = trp_id
  ) |>
  dplyr::mutate(
    chained_index_p = (chained_index - 1) * 100
  )

write.csv2(
  trps_e18_index,
  file = "data_indexpoints_tidy/buskerudbyen_e18_punktindekser.csv",
  row.names = F
)
