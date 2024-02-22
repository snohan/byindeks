## Seeing all TRPs MDTs simultaneously ----
{
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
    dplyr::mutate(
      mean_mdt_month_area = mean(mdt_ref_year),
      .by = month
    ) |>
    dplyr::select(-mdt, -mean_mdt_ref_year)


  mdts_last_36_months <-
    mdt_validated |>
    dplyr::select(
      trp_id,
      year,
      month,
      year_month,
      mdt,
      coverage,
      length_quality
    ) |>
    dplyr::filter(
      coverage >= 50, # this is length_coverage!
      length_quality >= 98.5,
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

}
