## Jackknife ----
# Run calculate_rolling_indices for every sample with one TRP left out

trp_mdt_ok_refyear_all <- trp_mdt_ok_refyear

if(city_number == 960){

  # Include toll stations here
  trp_mdt_ok_refyear_all <-
    mdt_validated |>
    filter_mdt(reference_year) |>
    purrr::pluck(1)

}

all_12_month_indices_jackknife <- 
  purrr::map_dfr(
    trp_mdt_ok_refyear_all,
    ~ calculate_rolling_indices(
        12, 
        mdt_validated_df = mdt_validated |> dplyr::filter(trp_id != .x)
      ) |> 
      # Need to keep track of which TRP is left out
      dplyr::mutate(
        trp_id_left_out = .x
      )
  )
  
all_12_month_indices_jackknife_tidy <-
  # Some TRPs are already missing, so must keep only jackknife-replicates where the to-be-left-out trp_id is present in trp window index
  dplyr::inner_join(
    all_12_month_indices_jackknife,
    all_12_month_trp_indices |> dplyr::select(trp_id, last_month_in_index),
    by = dplyr::join_by(trp_id_left_out == trp_id, month_object == last_month_in_index)
  )
  
pseudo_observations <-
  # Compare jackknife index to original index
  dplyr::left_join(
    all_12_month_indices_jackknife_tidy |> dplyr::select(trp_id_left_out, index_p_jack = index_p, month_object),
    all_12_month_indices |> dplyr::select(index_p, n_trp, month_object),      
    by = dplyr::join_by(month_object)
  ) |> 
  # Add TRP index
  dplyr::left_join(
    all_12_month_trp_indices |> dplyr::select(trp_id, name, last_month_in_index, trp_index_p),
    by = dplyr::join_by(trp_id_left_out == trp_id, month_object == last_month_in_index)
  ) |> 
  dplyr::mutate(
    trp_index_p_pseudo = n_trp * index_p - (n_trp - 1) * index_p_jack,
    index_p_delta = index_p_jack - index_p
  ) |> 
  dplyr::select(
    trp_id = trp_id_left_out, name, month_object, index_p, index_p_jack, index_p_delta, everything()
  )
  

x_axis_breaks_jack <- 
    pseudo_observations$month_object |> 
    base::unique() |> 
    base::as.character() |> 
    stringr::str_subset("-12-")


trp_jackknife_delta_plot_12 <-
  pseudo_observations |>
  dplyr::mutate(
    month_object = as.character(month_object) |> factor()
  ) |> 
  ggplot2::ggplot(
    aes(
      x = month_object,
      y = name,
      fill = index_p_delta
    )
  ) +
  geom_tile() +
  theme_minimal() +
  labs(
    title = "Jackknife-avvik",
    x = "",
    y = "",
    fill = "Endring\n(%-poeng)"
  ) +
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text = element_text(size = 12),
    legend.position = "right"
  ) +
  scale_fill_viridis(discrete = FALSE) +
  scale_x_discrete(breaks = x_axis_breaks_jack)

# trp_jackknife_delta_plot_12 |> plotly::ggplotly()


# Plot with city index
visualize_city_index_jackknife <- function(city_index_df, pseudo_observation_df) {

  x_breaks_labels <-
    city_index_df |>
    dplyr::filter(
      month_n %in% c(4, 8, 12)
    ) |>
    dplyr::mutate(
      x_label = base::paste0(lubridate::month(month_object, label = TRUE), " ", stringr::str_sub(year, 3, 4))
    )

  x_breaks <- x_breaks_labels |> purrr::pluck("month_object")
  x_labels <- x_breaks_labels |> purrr::pluck("x_label")

  city_index_df |>
    ggplot2::ggplot(aes(x = month_object, y = index_p)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "#58b02c",
      linewidth = 0.8,
      alpha = 0.3
    ) +
    ggplot2::geom_ribbon(
      aes(
        ymin = ci_lower,
        ymax = ci_upper
      ),
      linetype = 2,
      alpha = 0.1,
      fill = "#444f55"
    ) +
    ggplot2::geom_line(color = "#ED9300") +
    ggplot2::geom_point(color = "#ED9300") +
    ggplot2::geom_boxplot(
      data = pseudo_observations,
      aes(x = month_object, y = index_p_jack, group = month_object),
      alpha = 0,
      outlier.alpha = 1
    ) +
    # ggplot2::geom_violin(
    #   data = pseudo_observations,
    #   aes(x = month_object, y = index_p_jack, group = month_object)
    # ) +
    theme_light() +
    theme(
      axis.text.x = element_text(vjust = 0.5, angle = 90),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        )
    ) +
    ggplot2::scale_x_date(
      breaks = x_breaks,
      labels = x_labels
    ) +
    labs(x = NULL, y = "Endring i trafikkmengde (%)") +
    ggtitle("Byindeks og jackknife-variasjoner")
}

# visualize_city_index_jackknife(all_12_month_indices, pseudo_observations)
