{
  base::Sys.setlocale(locale = "nb.utf8")
  library(tidyverse)
  source("get_from_trafficdata_api.R")
}


## Rolling index ----
# TODO: use already checked MDT data to recalculate - differences will be
# - traffic work weights
# - finite population corrected CI

# Need traffic_work per link that has TRP in order to calculate weighted mean
mdt_tw <-
  links_nj_central_reduced |>
  sf::st_drop_geometry() |>
  dplyr::select(
    trp_id = point_id,
    traffic_work
  ) |>
  dplyr::filter(
    !is.na(trp_id)
  )


{
  present_year <- 2024
  index_month <- 12
  city_number <- 952
}

source("set_time_references.R")

mdt_filtered <-
  readr::read_rds(
    file =
      paste0(
        "data_indexpoints_tidy/mdt_",
        city_number,
        ".rds"
      )
  )

source("exclude_trp_mdts_list.R")

filter_mdt <- function(mdt_df, year_dbl) {

  mdt_df |>
    dplyr::filter(
      year == year_dbl,
      coverage >= 50, # this is length_coverage!
      length_quality >= 98.5
    ) |>
    dplyr::select(
      trp_id,
      year,
      month,
      mdt
    ) |>
    dplyr::group_by(
      trp_id,
      year
    ) |>
    dplyr::summarise(
      n_months = n(),
      mean_mdt = mean(mdt),
      .groups = "drop"
    ) |>
    dplyr::filter(
      n_months >= 10
    )

}


# For testing
base_year <- reference_year
#last_year_month <- "2024-12-01"
window_length <- 36
grouping <- "by_area"
mdt_df <- mdt_validated

test <- calculate_rolling_indices_by_mdt(reference_year, last_year_month, window_length, mdt_validated, grouping)

calculate_rolling_indices_by_mdt <-
  function(base_year, last_year_month, window_length, mdt_df, grouping) {

    # Window length is a number of months, a multiple of 12
    # Grouping must be either:
    # by_area
    # by_sub_area
    # by_trp

    least_number_of_month_enums <-
      dplyr::case_when(
        window_length == 36 ~ 2,
        TRUE ~ 0
      )

    least_number_of_months <-
      dplyr::case_when(
        window_length == 36 ~ 31,
        window_length == 24 ~ 20,
        window_length == 12 ~ 9
      )

    last_year_month <-
      lubridate::as_date(last_year_month)

    mean_mdt_in_window <-
      mdt_df |>
      dplyr::filter(
        year_month %in%
          base::seq.Date(
            from = last_year_month - base::months(window_length - 1),
            to = last_year_month,
            by = "month"
          )
      ) |>
      dplyr::filter(
        coverage >= 50,
        length_quality >= 98.5
      ) |>
      dplyr::group_by(
        trp_id,
        month
      ) |>
      dplyr::summarise(
        n_months = n(),
        mean_mdt = base::mean(mdt),
        .groups = "drop_last"
      ) |>
      dplyr::filter(
        n_months >= least_number_of_month_enums
      ) |>
      dplyr::group_by(
        trp_id
      ) |>
      dplyr::summarise(
        n_months = sum(n_months),
        mean_mdt = base::mean(mean_mdt),
        .groups = "drop"
      ) |>
      dplyr::filter(
        n_months >= least_number_of_months
      )

    index_df <-
      dplyr::inner_join(
        filter_mdt(mdt_df, base_year),
        mean_mdt_in_window,
        by = "trp_id"
      ) |>
      dplyr::mutate(
        w = mean_mdt.x / sum(mean_mdt.x),
        trp_index_i = mean_mdt.y / mean_mdt.x,
        #weigted_mean = sum(w * trp_index_i), # same as index_i :)
        index_i = sum(mean_mdt.y) / sum(mean_mdt.x),
        sd_component = w * (trp_index_i - index_i)^2
      )

    if(grouping == "by_area") {
      index_df_grouped <-
        index_df |>
        dplyr::summarise(
          index_i = sum(mean_mdt.y) / sum(mean_mdt.x),
          index_p = (index_i - 1) * 100,
          n_trp = n(),
          n_eff = 1 / sum(w^2),
          sd_sample_p = 100 * sqrt(sum(sd_component) * (1/(1 - 1/n_eff))),
          standard_error_p = sd_sample_p / sqrt(n_eff),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error_p, 1),
          ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error_p, 1)
        )
    }

    if(grouping == "by_sub_area") {
      index_df_grouped <-
        index_df |>
        dplyr::left_join(
          sub_areas,
          by = join_by(trp_id)
        ) |>
        dplyr::summarise(
          index_i = sum(mean_mdt.y) / sum(mean_mdt.x),
          index_p = (index_i - 1) * 100,
          n_trp = n(),
          n_eff = 1 / sum(w^2),
          sd_sample_p = 100 * sqrt(sum(sd_component) * (1/(1 - 1/n_eff))),
          standard_error_p = sd_sample_p / sqrt(n_eff),
          .by = sub_area
        ) |>
        dplyr::mutate(
          ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error_p, 1),
          ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error_p, 1)
        )
    }

    if(grouping == "by_trp") {
      index_df_grouped <-
        index_df
    }

    index_df_final <-
      index_df_grouped |>
      dplyr::mutate(
        index_period =
          paste0(
            base_year,
            " - (",
            (last_year_month - base::months(window_length - 1)) |>
              lubridate::month(label = TRUE),
            " ",
            (last_year_month - base::months(window_length - 1)) |>
              lubridate::year(),
            " - ",
            last_year_month |>
              lubridate::month(label = TRUE),
            " ",
            last_year_month |>
              lubridate::year(),
            ")"
          ),
        month_object = last_year_month
      ) |>
      dplyr::mutate(
        month_n = lubridate::month(month_object),
        year = lubridate::year(month_object),
        window = paste0(window_length, "_months")
      )

    return(index_df_final)

  }


calculate_rolling_indices <- function(window_length, grouping = "by_area") {

  base::stopifnot(window_length %% 12 == 0)

  n_years <- window_length / 12

  first_possible_year_month <-
    lubridate::as_date(
      paste0(
        reference_year + n_years,
        "-12-01"
      )
    )

  year_months_possible <-
    base::seq.Date(
      from = first_possible_year_month,
      to = last_year_month,
      by = "month"
    )

  purrr::map_dfr(
    year_months_possible,
    ~ calculate_rolling_indices_by_mdt(reference_year, .x, window_length, mdt_validated, grouping)
  )

}


# TODO: include MDT for 2023 and 2024 and calculate index
# TODO: alternative selections of TRPs
# 1. Original 24
# 2. Suggested 59
# 3. Repeated random selection of n


# City TRPs
links_with_city_trp_nj <-
  links_nj |>
  sf::st_drop_geometry() |>
  tidyr::unnest_longer(
    trp_id
  ) |>
  dplyr::inner_join(
    city_trp_info |>
      dplyr::filter(
        city_names == "Nord-Jæren"
      ),
    by = join_by(trp_id == p_id)
  ) |>
  dplyr::select(
    link_id,
    city_trp_id = trp_id
  )

links_nj_final <-
  links_nj |>
  dplyr::left_join(
    links_with_city_trp_nj,
    by = join_by(link_id)
  ) |>
  dplyr::select(
    -trp_id
  ) |>
  sf::st_as_sf()