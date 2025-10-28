all_12_month_indices_sub <- calculate_rolling_indices(12, grouping = "by_sub_area")
all_24_month_indices_sub <- calculate_rolling_indices(24, grouping = "by_sub_area")
all_36_month_indices_sub <- calculate_rolling_indices(36, grouping = "by_sub_area")


all_rolling_indices_list_sub <-
  base::list(
    all_12_month_indices_sub,
    all_24_month_indices_sub,
    all_36_month_indices_sub
  )

all_rolling_indices_sub <-
  dplyr::bind_rows(
    all_rolling_indices_list_sub
  )

all_rolling_indices_list_sub |>
  readr::write_rds(
    file = base::paste0("data_indexpoints_tidy/rolling_indices_sub_area_", city_number, ".rds")
  )