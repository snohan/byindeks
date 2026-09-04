# Rolling index with CMDT ----
# Monthly city index based on whichever TRPs are available,
# without considering their time representativeness by year.

calculate_area_index_for_month <- function(trp_mdt_df) {

  # INPUT
  # trp_mdt_df: 
  #   dataframe with cmdt by trp and month for both years
  #   enriched with:
  #     length and functional class of traffic link
  #     traffic work of trafficlink in reference year
  #     relative weights for each functional class based on total traffic work in city area
  #     universal_year_period_id to simplify handling of months in correct order


  # Find reference year (need not be calendar year) from the given data:
  reference_year_start_universal_year_period_id <- base::min(trp_mdt_df$universal_year_period_id)

  calculation_year_start_universal_year_period_id <- reference_year_start_universal_year_period_id + 14
  
  reference_year_months <-
    universal_calendar_periods |> 
    dplyr::filter(
      universal_year_period_id >= reference_year_start_universal_year_period_id,
      universal_year_period_id <= reference_year_start_universal_year_period_id + 13
    )
  
  reference_year_string <- 
    base::paste0(
      reference_year_months$x_label[1],
      "-",
      reference_year_months$x_label[14]
    ) |> 
    stringr::str_remove_all("\\s+")

  
  # CMDT in reference year
  mdt_a <-
    trp_mdt_df |>
    dplyr::filter(
      universal_year_period_id %in% c(reference_year_start_universal_year_period_id:(reference_year_start_universal_year_period_id + 13))
    ) |>
    dplyr::select(
      trp_id,
      year_a = year,
      month,
      compared_to_uypid = universal_year_period_id,
      mdt,
      length_m,
      fcl = function_class,
      trp_tw_ref_kkm,
      tw_fcl_population_kkm,
      tw_fcl_population_share
    )

  
  mdt_b <-
    trp_mdt_df |>
    dplyr::filter(
      universal_year_period_id %in% c(calculation_year_start_universal_year_period_id:(calculation_year_start_universal_year_period_id + 13))
    ) |>
    dplyr::select(
      trp_id,
      year_b = year,
      month,
      mdt,
      universal_year_period_id
    )

  link_index_month_fcl <-
    dplyr::inner_join(
      mdt_a,
      mdt_b,
      by = dplyr::join_by(trp_id, month),
      suffix = c("_a", "_b")
    ) |>
    dplyr::select(
      trp_id,
      year_a, year_b, month, universal_year_period_id, compared_to_uypid,
      mdt_a, mdt_b,
      length_m, fcl, trp_tw_ref_kkm, tw_fcl_population_kkm, tw_fcl_population_share
    ) |>
    # Need some global variables before summarising
    dplyr::mutate(
      tw_fcl_observed_a = base::sum(mdt_a * length_m),
      tw_fcl_observed_b = base::sum(mdt_b * length_m),
      # Variance
      ratio_of_mean_observed = base::sum(tw_fcl_observed_b) / base::sum(tw_fcl_observed_a),
      #
      n_links_in_selection = n(),
      .by = c(universal_year_period_id, fcl)
    ) |>
    # Can't have just one link in a function class
    dplyr::filter(
      n_links_in_selection > 1
    ) |>
    # Entities needed in each summation variable
    dplyr::mutate(
      mdt_delta = mdt_b - mdt_a,
      p_abi_i = mdt_b / mdt_a,
      share_link_to_fcl = mdt_a * length_m / tw_fcl_observed_a,
      # Variance
      tw_trp_a = length_m * mdt_a,
      tw_trp_b = length_m * mdt_b,
      var_robust_factor_trp = 1/(1 - tw_trp_a / tw_fcl_observed_a),
      var_robust_diff = (tw_trp_b - ratio_of_mean_observed * tw_trp_a)^2
    )

  area_index_month_fcl <-
    link_index_month_fcl |>
    dplyr::summarise(
      index_i = base::sum(mdt_b * length_m) / base::sum(mdt_a * length_m),
      index_p = 100 * (index_i - 1),
      n_trp = n(),
      # Variance
      var_robust_fcl = (1 / base::sum(mdt_a * length_m)^2) * base::sum(var_robust_factor_trp * var_robust_diff),
      #
      tw_fcl_selection_ref = base::sum(trp_tw_ref_kkm),
      .by = c(universal_year_period_id, compared_to_uypid, fcl, tw_fcl_population_kkm, tw_fcl_population_share)
    ) |>
    dplyr::mutate(
      tw_selection_ref_share = tw_fcl_selection_ref / base::sum(tw_fcl_selection_ref),
      .by = universal_year_period_id
    ) |>
    dplyr::mutate(
      tvd_diff = base::abs(tw_fcl_population_share - tw_selection_ref_share)
    ) |>
    dplyr::arrange(
      fcl, universal_year_period_id
    )

  area_index_month <-
    area_index_month_fcl |>
    dplyr::summarise(
      index_i = (base::sum(index_i * tw_fcl_population_kkm) / base::sum(tw_fcl_population_kkm)) |> base::round(4),
      index_p = (base::sum(index_p * tw_fcl_population_kkm) / base::sum(tw_fcl_population_kkm)) |> base::round(2),
      n_trp = base::sum(n_trp),
      # Variance
      var_robust_i = base::sum((tw_fcl_population_kkm / base::sum(tw_fcl_population_kkm))^2 * var_robust_fcl),
      sd_robust_i = base::sqrt(var_robust_i),
      em_robust_i = base::round(-stats::qt(0.025, n_trp - 1) * sd_robust_i, 4),
      sd_robust_p = 100 * base::sqrt(var_robust_i),
      em_robust_p = base::round(-stats::qt(0.025, n_trp - 1) * sd_robust_p, 4),
      #
      ci_lower = index_p - em_robust_p,
      ci_upper = index_p + em_robust_p,
      .by = c(universal_year_period_id, compared_to_uypid)
    )  |>
    dplyr::left_join(
      universal_calendar_periods,
      by = dplyr::join_by(universal_year_period_id)
    ) |>
    dplyr::mutate(
      reference_period = reference_year_string
    ) |> 
    dplyr::left_join(
      universal_calendar_periods |> dplyr::select(compared_to_uypid = universal_year_period_id, compared_to = x_label),
      by = "compared_to_uypid"
    ) |> 
    dplyr::select(
      universal_year_period_id,
      x_label,
      compared_to,
      period_name,
      compared_to_uypid,
      reference_period,
      index_i,
      index_p,
      n_trp,
      var_robust_i,
      ci_lower,
      ci_upper
    )

  link_index_month <-
    link_index_month_fcl |>
    dplyr::left_join(
      area_index_month_y |>
        dplyr::select(
          universal_year_period_id, index_p
        ),
      by = "universal_year_period_id"
    ) |>
    dplyr::select(
      trp_id,
      year_a, year_b, month, universal_year_period_id,
      tw_fcl_population_share, share_link_to_fcl, mdt_delta, p_abi_i,index_p
    ) |>
    dplyr::mutate(
      p_abi_p = 100 * (p_abi_i - 1),
      ww = tw_fcl_population_share * share_link_to_fcl,
      ww_p_abi_p = ww * p_abi_p,
      pull = ww * (p_abi_p - index_p)
    )

  return_object <-
    base::list(
      area_index_month,
      link_index_month
    )

  return(return_object)
}


calculate_rolling_area_index_one_year <- function(area_index_month_df) {

  area_index_month_tidy <-
    area_index_month_df |>
    dplyr::left_join(
      period_weights,
      by = dplyr::join_by(period_name)
    ) |>
    dplyr::select(
      universal_year_period_id,
      index_i,
      var_robust_i,
      period_days
    )
  
  compared_to_period <- area_index_month_df$reference_period[1]

  # One-year rolling index for all possible windows.
  # One may choose start and end of the series by first filtering the df before calling this function.

  first_start_id <- base::min(area_index_month_tidy$universal_year_period_id)
  last_start_id <- base::max(area_index_month_tidy$universal_year_period_id - 13)
  possible_window_starts <- c(first_start_id:last_start_id)
  
  # compared_to_year_df <- 
  #   universal_calendar_periods |> 
  #   dplyr::filter(universal_year_period_id == (first_start_id - 1))

  rolling_area_index <- tibble::tibble()

  for(i in c(1:(base::length(possible_window_starts)))) {

    window_ids <- c(possible_window_starts[i]:(possible_window_starts[i] + 13))

    calculation_period <- 
      base::paste0(
      universal_calendar_periods |> dplyr::filter(universal_year_period_id == window_ids[1]) |> dplyr::pull(x_label),
        "-",
      universal_calendar_periods |> dplyr::filter(universal_year_period_id == window_ids[14]) |> dplyr::pull(x_label)
      ) |> 
      stringr::str_remove_all("\\s+")

    rolling_area_index_i <-
      area_index_month_tidy |>
      dplyr::filter(
        universal_year_period_id %in% window_ids
      ) |>
      dplyr::summarise(
        # TODO: weighting by tw per period, instead of days - very important for bike index!
        index_i = base::sum((period_days / base::sum(period_days)) * index_i),
        index_p = 100 * (index_i - 1),
        var_i = base::sum((period_days / base::sum(period_days))^2 * var_robust_i),
        sd_p = 100 * base::sqrt(var_i),
        em_p = base::round(-stats::qnorm(0.025) * sd_p, 4),
        ci_lower = index_p - em_p,
        ci_upper = index_p + em_p,
        universal_year_period_id = window_ids[14]
      ) |>
      dplyr::left_join(
        universal_calendar_periods,
        by = dplyr::join_by(universal_year_period_id)
      ) |>
      dplyr::mutate(
        calculation_period = calculation_period,
        compared_to = compared_to_period
      ) |> 
      dplyr::select(
        universal_year_period_id,
        x_label,
        calculation_period,
        compared_to,
        index_i,
        index_p,
        var_i,
        ci_lower,
        ci_upper
      )

    rolling_area_index <-
      dplyr::bind_rows(
        rolling_area_index,
        rolling_area_index_i
      )

  }

  return(rolling_area_index)

}
