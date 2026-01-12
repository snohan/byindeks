## All possible window indices for TRP ----
# 12
all_12_month_trp_indices <-
  calculate_rolling_indices(12, "by_trp") |>
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

trp_mdt_plot_12 <-
  all_12_month_trp_indices |>
  # dplyr::filter(
  #   last_month_in_index >= "2022-12-01"
  # ) |> 
  dplyr::mutate(
    last_month_in_index = as.character(last_month_in_index) |> factor()
  ) |> 
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
    title = "Ett års glidende indeks i byindekspunktene",
    x = "",
    y = "",
    fill = "Endring\n(%)"
  ) +
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text = element_text(size = 12),
    legend.position = "right"
  ) +
  scale_fill_viridis(discrete = FALSE)


# 36
all_36_month_trp_indices <- calculate_rolling_indices(36, "by_trp")

if(city_number == 960) {

  trd_toll_station_names <-
    readr::read_rds(
      "trd_toll_station_id.rds"
    )

  all_36_month_trp_indices <-
    all_36_month_trp_indices |>
    dplyr::left_join(
      trd_toll_station_names,
      by = "trp_id"
    )
}

all_36_month_trp_indices <-
  all_36_month_trp_indices |>
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

# Check contribution from TRPs each possible 36 month index
trp_mdt_plot_36 <-
  all_36_month_trp_indices |>
  dplyr::mutate(
    last_month_in_index = as.character(last_month_in_index) |> factor()
  ) |> 
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
    title = "Tre års glidende indeks i byindekspunktene",
    x = "",
    y = "",
    fill = "Endring\n(%)"
  ) +
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text = element_text(size = 12),
    legend.position = "right"
  ) +
  scale_fill_viridis(discrete = FALSE)


