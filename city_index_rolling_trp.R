## All possible window indices for TRP ----

if(city_number == 960) {

  trp_names <-
    trp_names |> 
    dplyr::left_join(
      toll_meta_data <- readr::read_rds(file = "bomdata_trondheim/trd_toll_stations.rds") |> 
        dplyr::select(trp_id_2 = trp_id, nvdb_id),
      by = join_by(trp_id == nvdb_id)
    ) |> 
    dplyr::mutate(
      trp_id = dplyr::case_when(
        is.na(trp_id_2) ~ trp_id,
        TRUE ~ trp_id_2
      )
    ) |> 
    dplyr::select(-trp_id_2)
  
}


# 12
all_12_month_trp_indices <-
  calculate_rolling_indices(12, grouping = "by_trp") |>
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


plot_trp_mdt_index <- function(trp_mdt_index_df, n_years_str) {

  title_str <- paste0(n_years_str, " års glidende indeks i byindekspunktene")

  x_axis_breaks <- 
    trp_mdt_index_df$last_month_in_index |> 
    base::unique() |> 
    base::as.character() |> 
    stringr::str_subset("-12-")

  trp_mdt_index_plot <-
    trp_mdt_index_df |>
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
      title = title_str,
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
    scale_fill_viridis(discrete = FALSE) +
    scale_x_discrete(breaks = x_axis_breaks)

  return(trp_mdt_index_plot)
}

trp_mdt_plot_12 <- plot_trp_mdt_index(all_12_month_trp_indices, "Ett")


# 36
all_36_month_trp_indices <- calculate_rolling_indices(36, grouping = "by_trp")

if(nrow(all_36_month_trp_indices > 0)){

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

  trp_mdt_plot_36 <- plot_trp_mdt_index(all_36_month_trp_indices, "Tre")

  }


# Write
list(
  all_12_month_trp_indices,
  all_36_month_trp_indices
) |> 
readr::write_rds(
  file =
    paste0(
      "data_indexpoints_tidy/rolling_trp_indices_",
      city_number,
      ".rds"
    )
)

all_rolling_trp_indices <-
  dplyr::bind_rows(
    all_12_month_trp_indices |> dplyr::mutate(window = "12_months"),
    all_36_month_trp_indices |> dplyr::mutate(window = "36_months")
  )
  