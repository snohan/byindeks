# Index tidying functions

# Setup ----
library(ggpattern)
source("calendar_functions.R")

period_weights <- readr::read_rds("calendar_weights/periods.rds")
period_weights_imputed <- readr::read_rds("calendar_weights/period_weights_imputed.rds")


# Read CSV ----
read_pointindex_CSV <- function(filename) {

  # Read standard csv export from Datainn

  read.csv2(
    filename,
    encoding = "latin1"
    #check.names = FALSE
    ) |>
    dplyr::filter(
      døgn == "Alle",
      #døgn == "Yrkesdøgn",
      lengdeklasse == "< 5,6m",
      periode == "Hittil i år"
    ) %>%
    dplyr::mutate(
      trs = as.numeric(msnr),
      trafikkmengde.basisaar =
        as.numeric(as.character(trafikkmengde.basisår)),
      trafikkmengde.indeksaar =
        as.numeric(as.character(trafikkmengde.indeksår))
    ) |>
    # Why sum and recalculate index?
    dplyr::group_by(trs) %>%
    dplyr::summarise(
      trafikkmengde_basisaar = sum(trafikkmengde.basisaar),
      trafikkmengde_indeksaar = sum(trafikkmengde.indeksaar),
      index =
        round(
          (trafikkmengde_indeksaar/
            trafikkmengde_basisaar - 1) * 100,
          digits = 1
        )
    ) |>
    dplyr::rename(msnr = trs) %>%
    dplyr::select(msnr, index, base_volume = trafikkmengde_basisaar)

}

read_old_pointindex_csv_monthly <- function(filename, given_year) {

  # Read standard csv export from old VTI-app in Datainn

  read.csv2(
    filename,
    encoding = "latin1"
  ) |>
  dplyr::filter(
    døgn == "Alle",
    lengdeklasse == "< 5,6m",
    !(periode %in% c("Hittil i år", "Siste 12 måneder")),
    indeks != "-"
  ) |>
    dplyr::mutate(
      trs = as.numeric(msnr),
      #year = given_year,
      month = dplyr::case_when(
        periode == "Januar" ~ 1,
        periode == "Februar" ~ 2,
        periode == "Mars" ~ 3,
        periode == "April" ~ 4,
        periode == "Mai" ~ 5,
        periode == "Juni" ~ 6,
        periode == "Juli" ~ 7,
        periode == "August" ~ 8,
        periode == "September" ~ 9,
        periode == "Oktober" ~ 10,
        periode == "November" ~ 11,
        periode == "Desember" ~ 12
     ),
     index = decimal_point(indeks) |>
       as.numeric()
    ) |>
    dplyr::select(trs, #year,
                  month, index) |>
    dplyr::rename(msnr = trs)

}

#filename <- "data_index_raw/punktindeks_nord-jaeren-2020-01.csv"
read_pointindex_csv_with_volumes <- function(filename) {

    # Read standard csv export from Datainn
  df <- read.csv2(filename,
                  stringsAsFactors = F) %>%
    dplyr::filter(døgn == "Alle",
           lengdeklasse == "< 5,6m",
           periode == "Hittil i år") %>%
    dplyr::mutate(index = round(
                            as.numeric(decimal_point(indeks)),
                            digits = 1)) %>%
    dplyr::rename(base_volume = trafikkmengde.basisår,
                  calc_volume = trafikkmengde.indeksår) %>%
    dplyr::mutate(base_volume = as.numeric(base_volume),
                  calc_volume = as.numeric(calc_volume)) %>%
    dplyr::select(msnr, base_volume, calc_volume, index)
}

read_bikepointindex_csv <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename) %>%
    filter(døgn == "Alle",
           periode == "Hittil i år") %>%
    mutate(msnr = as.numeric(msnr),
           index = as.numeric(str_replace(indeks, ",", "."))) %>%
    select(msnr, index)
}

read_bike_index_csv <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename) %>%
    filter(Vegkategori == "E+R+F+K",
           døgn == "Alle",
           lengdeklasse == "Alle",
           periode == "Hittil i år") %>%
    mutate(index = as.numeric(str_replace(indeks, ",", ".")),
           dekning = as.numeric(str_replace(dekning, ",", ".")),
           standardavvik = as.numeric(as.character(standardavvik)),
           konfidensintervall = as.numeric(as.character(konfidensintervall))) %>%
    select(index, dekning, standardavvik, konfidensintervall) %>%
    as_tibble()
}


# Utils ----
index_converter <- function(index) {
  ifelse(
    is.na(index),
    1,
    index/100 + 1)
}

monthly_city_index <- function(city_index_for_a_year) {

  city_monthly <-
    city_index_for_a_year %>%
    dplyr::filter(
      road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
      length_range == "[..,5.6)",
      period == "month"
    ) %>%
    dplyr::mutate(
      month_object = lubridate::make_date(year = year, month = month),
      month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE) %>%
        stringr::str_to_title()
    )

}

filter_city_index <- function(city_index_df, last_month, period_type) {

  city_index_df |>
    dplyr::filter(
      month == last_month,
      road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
      length_range == "[..,5.6)",
      #length_range == "[5.6,..)",
      period == period_type
    )

}

# For TRD
calculate_area_index <- function(trp_index_df) {

  trp_index_df |>
    dplyr::mutate(
      weight = (base_volume / sum(base_volume))
    ) |>
    dplyr::summarise(
      city_base_volume = sum(base_volume),
      city_calc_volume = sum(calc_volume),
      index_p = (city_calc_volume / city_base_volume - 1 ) * 100,
      n_trp = n(),
      standard_deviation = sqrt((1 / (1 - sum(weight^2) )) * sum(weight * (index - index_p)^2) ),
      standard_error = sqrt(sum(weight^2) * standard_deviation^2),
      .groups = "drop"
    )|>
    dplyr::select(
      -city_base_volume,
      -city_calc_volume
    )
}


# Chained index ----
# Compound ci, need to iterate pairwise through the years!
# I.e. make accumulated index for one more year
#index_from_refyear <- 100*(prod(city_index_grenland$index_i)-1)

calculate_two_year_index <- function(city_index_df) {

  two_years <-
    city_index_df |>
    dplyr::select(
      #index_p,
      index_i,
      #variance,
      standard_error,
      n_trp
    ) %>%
    dplyr::slice(1:2)

  year_start <- city_index_df$year_base[1]
  year_end <- city_index_df$year[2]
  last_month <- city_index_df$month[2]

  two_years_to_one <-
    list(
      index_p = 100 * (prod(two_years$index_i) - 1),
      index_i = prod(two_years$index_i),
      year_base = year_start,
      year = year_end,
      month = last_month,
      standard_error =
        100 * sqrt(
          two_years$index_i[1]^2 * 1e-4 * two_years$standard_error[2]^2 +
            two_years$index_i[2]^2 * 1e-4 * two_years$standard_error[1]^2 +
            1e-4 * two_years$standard_error[1]^2 * 1e-4 * two_years$standard_error[2]^2
        ),
      # TODO: find the correct number of TRPs that have contributed
      # over the two years - their index must exist in both years?
      # Not exactly, because all TRPs contribute.
      n_trp = max(two_years$n_trp)
    ) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      year_base,
      year,
      month,
      index_p,
      index_i,
      n_trp,
      standard_error
    ) |>
    dplyr::mutate(
      index_type = "chained"
    )
}


calculate_any_two_year_index <- function(index_row_1, index_row_2) {

  two_years <-
    dplyr::bind_rows(
      index_row_1,
      index_row_2
    )

  year_start <- index_row_1$year_base
  year_end <- index_row_2$year
  last_month <- index_row_1$month
  #area_name <- index_row_1$area_name

  two_years_to_one <-
    list(
      #index_p = 100 * (prod(two_years$index_i) - 1),
      index_i = prod(two_years$index_i),
      year_base = year_start,
      year = year_end,
      month = last_month,
      standard_error =
        100 * sqrt(
          two_years$index_i[1]^2 * 1e-4 * two_years$standard_error[2]^2 +
            two_years$index_i[2]^2 * 1e-4 * two_years$standard_error[1]^2 +
            1e-4 * two_years$standard_error[1]^2 * 1e-4 * two_years$standard_error[2]^2
        ),
      # Find the correct number of TRPs that have contributed
      # over the two years - their index must exist in both years?
      # Not exactly, because all TRPs contribute.
      # The chain is no stronger than its weakest link.
      n_trp = min(two_years$n_trp)
    ) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      year_base,
      year,
      month,
      #index_p,
      index_i,
      n_trp,
      standard_error
    ) |>
    dplyr::mutate(
      index_type = "chained",
      #area_name = area_name
    )
}


calculate_two_years_index_36_month_version <- function(city_index_df) {

  # TODO: add sd and ci

  months_1_24 <- city_index_df %>%
    select(index_p, index_i) %>%
    slice(1:2)

  months_25_36 <- city_index_df %>%
    select(index_p, index_i) %>%
    slice(3)

  first_24_months <- list(
    index_p = 100 * (prod(months_1_24$index_i) - 1),
    index_i = prod(months_1_24$index_i)) %>%
    as_tibble()

  months_1_36 <- bind_rows(first_24_months, months_25_36)

  all_36_months_index <- list(
    index_p = 100 * (prod(months_1_36$index_i) - 1),
    index_i = prod(months_1_36$index_i)) %>%
    as_tibble()

  return(all_36_months_index)
}

calculate_all_possible_36_month_indexes <- function(city_monthly_df) {

  # A for-loop that loops through all consecutive and possible
  # 36-month periods

  no_months <- nrow(city_monthly_df)
  n_end <- no_months - 35

  all_possible_36_month_indexes <- tibble::tibble()

  for (n in 1:n_end) {

    # n from n_start to n_end
    start_month <- n
    end_month <- 35 + n

    # The 36 months for this iteration
    city_monthly_36 <- city_monthly_df %>%
      slice(start_month:end_month) %>%
      tibble::rowid_to_column("id") %>%
      mutate(three_year_group = case_when(
        id <= 12 ~ 1,
        id <= 24 ~ 2,
        id <= 36 ~ 3,
        TRUE ~ 4
      ))

    # The end month for this iteration
    city_monthly_36_period <- city_monthly_df %>%
      slice(end_month) %>%
      select(month_name, month_object, year) #%>%
      #mutate(year = stringr::str_sub(year, 6, 9))

    # The 36 month index for this iteration
    city_monthly_36_index <- city_monthly_36 %>%
      #filter(three_year_group < 4) %>%
      group_by(three_year_group) %>%
      summarise(volume_calc_year = sum(calc_volume),
                volume_base_year = sum(base_volume),
                index_p = (volume_calc_year / volume_base_year - 1 ) * 100,
                index_i = volume_calc_year / volume_base_year,
                n_points = max(n_points)
                # TODO: variance (would be easier if this index was calculated directly from point index)
                ) %>%
      #select(index, index_i) %>%
      calculate_two_years_index_36_month_version() %>%
      bind_cols(city_monthly_36_period)

    all_possible_36_month_indexes <- bind_rows(
      all_possible_36_month_indexes,
      city_monthly_36_index
    )

  }

  return(all_possible_36_month_indexes)
}


cum_se <- function(i1, i2, se1, se2) {

  cum_se =
    100 * sqrt(
      i1^2 * 1e-4 * se2^2 + i2^2 * 1e-4 * se1^2 + 1e-4 * se1^2 * 1e-4 * se2^2
    )
}


calculate_index_chain <- function(direct) {

  chained <-
    dplyr::bind_rows(
      calculate_any_two_year_index(
        direct[1,],
        direct[2,]
      )
    )

  n_chained_to_calculate <- base::nrow(direct) - 2

  for (i in 1:n_chained_to_calculate) {

    chained <-
      chained |>
      dplyr::bind_rows(
        calculate_any_two_year_index(
          chained[i,],
          direct[i+2,]
        )
      )
  }

  return(chained)

}


# Rolling index MDT ----
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


## MDT old 1 ----

# mdt_df <- mdt_validated
# window_length <- 36
# base_year <- reference_year
# last_year_month <- "2023-11-01"

calculate_rolling_indices_by_mdt <- function(base_year, last_year_month, window_length, mdt_df, grouping) {

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
        #weighted_mean = sum(w * trp_index_i), # same as index_i :)
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


## MDT old 2 ----
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


## MDT old 3 ----
calculate_all_rolling_indices_old <- function() {

  all_12_month_indices <-
    calculate_rolling_indices(12)

  all_24_month_indices <-
    calculate_rolling_indices(24)

  all_36_month_indices <-
    calculate_rolling_indices(36)

  all_rolling_indices <-
    dplyr::bind_rows(
      all_12_month_indices,
      all_24_month_indices,
      all_36_month_indices
    )

  return(all_rolling_indices)
}


# Rolling index MDT TW ----
## MDT TW 1 ----
calculate_rolling_indices_tw <- function(base_year, last_year_month, window_length, mdt_df, population_size, grouping) {

    # TODO: much is the same as old, tv version - separate into more functions?

    # Window length is a number of months, a multiple of 12
    # Grouping must be either:
    # by_area
    # by_sub_area
    # by_trp

    # For testing:
    # base_year <- 2018
    # window_length <- 12
    # mdt_df <- mdt_validated

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

    weights_tw <-
      mdt_df |>
      dplyr::select(trp_id, length_m) |>
      dplyr::distinct() |>
      dplyr::mutate(
        length_km = length_m / 1e3
      ) |>
      dplyr::select(-length_m)

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
      dplyr::left_join(
        weights_tw,
        by = dplyr::join_by(trp_id)
      ) |>
      dplyr::mutate(
        tw.x = length_km * mean_mdt.x,
        #tw.y = length_km * mean_mdt.y,
        w_tw = tw.x / sum(tw.x),
        w_tv = mean_mdt.x / sum(mean_mdt.x),
        w_length = length_km / sum(tw.x),
        trp_index_i = mean_mdt.y / mean_mdt.x,
        trp_index_p = (trp_index_i - 1) * 100,
        index_i = sum(w_tw * trp_index_i),
        #index_i_2 = sum(w_length * mean_mdt.y),
        sd_component = w_tw * (trp_index_i - index_i)^2,
        # a1 = mean_mdt.x * mean_mdt.y,
        # sum_a1 = sum(a1),
        # a2 = mean_mdt.x^2,
        # sum_a2 = sum(a2),
        # test = sum_a1 / sum_a2, # ok
        # alpha = sum(mean_mdt.x * mean_mdt.y) / sum(mean_mdt.x^2)
      )

    # Normalized
    #sum(index_df$w_tw)
    # Not normalized
    #sum(index_df$w_length)

    if(grouping == "by_area") {

      calculate_tw_mean <- function(df, indices) {

        bootstrapped_df <- df[indices,] # allows boot to select sample

        summarised_df <-
          bootstrapped_df |>
          dplyr::summarise(
            index_i = sum(w_tw * trp_index_i),
            index_p = (index_i - 1) * 100,
          )

        return(summarised_df$index_p)
      }

      bootstrap_object <-
        boot::boot(
          data = index_df,
          statistic = calculate_tw_mean,
          R = 1000
        )

      booted_cis <- boot::boot.ci(bootstrap_object, type = c("norm", "basic", "perc", "bca"))
      # bootsurv::pseudopop.boot.stsrs ???

      index_df_grouped <-
        index_df |>
        dplyr::summarise(
          index_i = sum(w_tw * trp_index_i),
          index_p = (index_i - 1) * 100,
          n_trp = n(),
          n_eff = 1 / sum(w_tw^2),
          n_eff_tv = 1 / sum(w_tv^2),
          sd_sample_p = 100 * sqrt(sum(sd_component) * (1/(1 - 1/n_eff))),
          standard_error_p = sd_sample_p / sqrt(n_eff) * (1 - n_trp / population_size),
          cv_tv = sd(mean_mdt.x) / mean(mean_mdt.x),
          cv_tw = sd(tw.x) / mean(tw.x),
          alpha = sum(mean_mdt.x * mean_mdt.y) / sum(mean_mdt.x^2),
          var_model_s = (1/(n_trp - 1)) * sum((mean_mdt.y - alpha * mean_mdt.x)^2),
          se_model_p = 100 * sqrt(sum(w_length^2) * var_model_s),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          #ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error_p, 1),
          #ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error_p, 1),
          em_selection = round(-stats::qt(0.025, n_trp - 1) * standard_error_p, 2),
          em_model = round(-stats::qt(0.025, n_trp - 1) * se_model_p, 2),
          bs_bca_lower = booted_cis$bca[1,4],
          bs_bca_upper = booted_cis$bca[1,5]
        )
    }

    # TODO:
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

    # TODO:
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


## MDT TW 2 ----
calculate_rolling_indices_tw_all <- function(window_length, population_size, grouping = "by_area") {

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
    ~ calculate_rolling_indices_tw(
      reference_year,
      .x,
      window_length,
      mdt_validated,
      population_size,
      grouping
    )
  )

}


## MDT TW 3 ----
calculate_all_rolling_indices_tw <- function(population_size) {

  all_12_month_indices <-
    calculate_rolling_indices_tw_all(12, population_size)

  all_24_month_indices <-
    calculate_rolling_indices_tw_all(24, population_size)

  all_36_month_indices <-
    calculate_rolling_indices_tw_all(36, population_size)

  all_rolling_indices <-
    dplyr::bind_rows(
      all_12_month_indices,
      all_24_month_indices,
      all_36_month_indices
    )

  return(all_rolling_indices)
}


# Rolling index CMDT ----
check_year_representativity <- function(mdt_trp_one_year) {

  # Given an CMDT TRP df, check if a TRP has the required periods.

  mdt_trp_one_year_checked <-
    mdt_trp_one_year |>
    dplyr::select(
      trp_id, month
    ) |>
    dplyr::summarise(
      jan_feb = base::any(c("januar", "februar") %in% month),
      mar_apr = base::any(c("mars", "april") %in% month),
      sep_okt_nov = base::any(c("september", "oktober", "november") %in% month),
      andre = base::all(c("påske", "mai", "juni", "juli", "august", "desember") %in% month),
      # Pentecost may be missing. Must be missing in both index periods by inner join.
      .by = trp_id
    ) |>
    dplyr::mutate(
      alle = base::all(jan_feb, mar_apr, sep_okt_nov, andre),
      .by = trp_id
    ) |>
    dplyr::filter(alle)

}


filter_cmdt <- function(mdt_df, period_start) {

  # For any set of consecutive 14 periods, i.e. "whole" years, check that there are enough data

  # period_start:
  # - if string, year_period_name, e.g. "2024-januar"
  # - if numerical, universal_year_period_id

  # Testing:
  #period_start <- "2023-januar"
  #mdt_df <- mdt_validated


  if(is.character(period_start)) {
    base::stopifnot(period_start %in% universal_calendar_periods$year_period_name)

    universal_year_period_id_start <-
      universal_calendar_periods |>
      dplyr::filter(
        year_period_name == period_start
      ) |>
      purrr::pluck("universal_year_period_id")

  }else{
    universal_year_period_id_start <- period_start
  }

  universal_year_period_id_end <- universal_year_period_id_start + 13

  mdt_df_out <-
    mdt_df |>
    dplyr::filter(
      universal_year_period_id %in% c(universal_year_period_id_start:universal_year_period_id_end)
    ) |>
    dplyr::select(
      trp_id,
      #universal_year_period_id,
      month,
      mdt#,
      #coverage_percentage,
      #length_m,
      #function_class,
      #tw_kkm
    )

  representative_trps <- check_year_representativity(mdt_df_out)

  mdt_df_out_representative_year <-
    mdt_df_out |>
    dplyr::filter(
      trp_id %in% representative_trps$trp_id
    )

  return(mdt_df_out_representative_year)

}


impute_missing_cmdt <- function(filtered_cmdt_df) {

  # Group some periods together in order to allow for some missingness,
  # and impute explicitly by nearby period.
  # This instead of no imputation, which is still an imputation (probably a bad one), when missingness is allowed.

  imputed_cmdt <-
    dplyr::bind_rows(
      filtered_cmdt_df |>
        dplyr::filter(
          month %in% c("januar", "februar")
        ) |>
        dplyr::summarise(
          mdt = base::mean(mdt),
          .by = trp_id
        ) |>
        dplyr::mutate(
          month = "jan_feb"
        ),
      filtered_cmdt_df |>
        dplyr::filter(
          month %in% c("mars", "april")
        ) |>
        dplyr::summarise(
          mdt = base::mean(mdt),
          .by = trp_id
        ) |>
        dplyr::mutate(
          month = "mar_apr"
        ),
      filtered_cmdt_df |>
        dplyr::filter(
          month %in% c("september", "oktober", "november")
        ) |>
        dplyr::summarise(
          mdt = base::mean(mdt),
          .by = trp_id
        ) |>
        dplyr::mutate(
          month = "sep_okt_nov"
        ),
      filtered_cmdt_df |>
        dplyr::filter(
          !(month %in% c("januar", "februar", "mars", "april", "september", "oktober", "november"))
        )
    )

  return(imputed_cmdt)
}


# Notation:
# a: base year
# b: calculation year (window)
# fcl: function class

rolling_index_trp <- function(cmdt_df) {

  # One-year rolling index per TRP, for all possible windows.
  # One may choose start and end of the series by first filtering the df before calling this function.

  base_year_start_id <- base::min(cmdt_df$universal_year_period_id) # TODO: check that this is a January
  first_rolling_window_start <- base::min(cmdt_df$universal_year_period_id + 14)
  last_rolling_window_start <- base::max(cmdt_df$universal_year_period_id - 13)
  possible_window_starts <- c(first_rolling_window_start:last_rolling_window_start)

  base_year_cmdt <- filter_cmdt(cmdt_df, base_year_start_id) |> impute_missing_cmdt()

  # For each TRP, find base year and rolling window cmdt,
  # inner join to make sure we compare the same months (pentecost may be missing),
  # and calculate weighted mean cMDT, which is the p in the index formula.

  trp_window_index <- tibble::tibble()

  for(i in 1:length(possible_window_starts)) {

    window_cmdt <-
      filter_cmdt(cmdt_df, possible_window_starts[i]) |>
      impute_missing_cmdt()

    trp_window_index_i <-
      dplyr::inner_join(
        base_year_cmdt,
        window_cmdt |> dplyr::select(trp_id, month, mdt),
        by = dplyr::join_by(trp_id, month),
        suffix = c("_base", "_window")
      ) |>
      dplyr::left_join(
        period_weights_imputed,
        by = "month"
      ) |>
      dplyr::summarise(
        mean_mdt_a = base::sum(mdt_base * period_days) / base::sum(period_days),
        mean_mdt_b = base::sum(mdt_window * period_days) / base::sum(period_days),
        index_i = mean_mdt_b / mean_mdt_a,
        index_p = 100 * (index_i - 1),
        .by = "trp_id"
      ) |>
      dplyr::mutate(
        universal_year_period_id_end = possible_window_starts[i] + 13
      )

    trp_window_index <-
      dplyr::bind_rows(
        trp_window_index,
        trp_window_index_i
      )

  }

  return(trp_window_index)
}


rolling_index_area <- function(trp_window_index) {

  # Weigh each TRP by its traffic work contribution.
  # Post stratify by function class.

  window_index_f <-
    trp_window_index |>
    dplyr::left_join(trp_weights, by = "trp_id") |>
    # Need some global variables before summarising
    dplyr::mutate(
      tw_fcl_observed_a = base::sum(mean_mdt_a * length_m),
      tw_fcl_observed_b = base::sum(mean_mdt_b * length_m),
      # Variance: Population model
      beta_pop_model = base::sum(mean_mdt_a * mean_mdt_b) / base::sum(mean_mdt_a^2),
      # Variance: Robust
      ratio_of_mean_observed = base::sum(tw_fcl_observed_b) / base::sum(tw_fcl_observed_a),
      #
      .by = c(universal_year_period_id_end, function_class)
    ) |>
    # Entities needed in each summation variable
    dplyr::mutate(
      p_abi_i = mean_mdt_b / mean_mdt_a,
      # Variance: Population model
      w_trp_length_a =  length_m / tw_fcl_observed_a,
      # Variance: Robust
      tw_trp_a = length_m * mean_mdt_a,
      tw_trp_b = length_m * mean_mdt_b,
      var_robust_factor_trp = 1/(1 - tw_trp_a / tw_fcl_observed_a),
      var_robust_diff = (tw_trp_b - ratio_of_mean_observed * tw_trp_a)^2
    ) |>
    dplyr::summarise(
      index_i = base::sum(mean_mdt_b * length_m) / base::sum(mean_mdt_a * length_m),
      index_p = 100 * (index_i - 1),
      n_trp = n(),
      # Beale
      mean_tw_estimated_a = base::mean(mean_mdt_a * length_m),
      mean_tw_estimated_b = base::mean(mean_mdt_b * length_m),
      sd_a = stats::sd(mean_mdt_a * length_m),
      sd_b = stats::sd(mean_mdt_b * length_m),
      rho = stats::cor(mean_mdt_a, mean_mdt_b),
      c_ab = rho * sd_a * sd_b / (mean_tw_estimated_a * mean_tw_estimated_b),
      c_aa = sd_a^2 / mean_tw_estimated_a^2,
      # Variance: Population model
      var_pop_model_fcl = base::sum(w_trp_length_a^2) * (1/(n_trp - 1)) * base::sum((mean_mdt_b - beta_pop_model * mean_mdt_a)^2),
      # Variance: Robust
      var_robust_fcl = (1 / base::sum(mean_mdt_a * length_m)^2) * base::sum(var_robust_factor_trp * var_robust_diff),
      #
      .by = c(universal_year_period_id_end, function_class, tw_fcl_population_kkm, n_links)
    ) |>
    dplyr::mutate(
      # Beale
      ratio_factor = (1 - n_trp/n_links) / n_links,
      index_p_beale = index_p * (1 + ratio_factor * c_ab) / (1 + ratio_factor * c_aa),
      beale_factor = (1 + ratio_factor * c_ab) / (1 + ratio_factor * c_aa), # Bergen: all very close to 1
      # Variance: ratio estimator
      #var_re_tw_b_fcl = n_links^2 * (1 - n_trp/n_links) / n_trp * (sd_b^2 + index_i^2 * sd_a^2 - 2 * index_i * rho * sd_a * sd_b),
      #var_re_sys_fcl = (1 - n_trp/n_links) / (n_trp * mean_tw_estimated_a^2) * (index_i * sd_a^2 - rho * sd_a * sd_b)
    ) |>
    dplyr::relocate(index_p_beale, .before = n_trp)

    window_index_post_stratified <-
      window_index_f |>
      dplyr::summarise(
        index_i = (base::sum(index_i * tw_fcl_population_kkm) / base::sum(tw_fcl_population_kkm)) |> base::round(2),
        index_p = (base::sum(index_p * tw_fcl_population_kkm) / base::sum(tw_fcl_population_kkm)) |> base::round(2),
        index_p_beale = (base::sum(index_p_beale * tw_fcl_population_kkm) / base::sum(tw_fcl_population_kkm)) |> base::round(2),
        n_trp = base::sum(n_trp),
        # Variance: Population model
        var_pop_model = base::sum((tw_fcl_population_kkm / base::sum(tw_fcl_population_kkm))^2 * var_pop_model_fcl),
        sd_pop_model_p = 100 * base::sqrt(var_pop_model),
        em_pop_model = base::round(-stats::qt(0.025, n_trp - 1) * sd_pop_model_p, 2),
        # Variance: Robust
        var_robust = base::sum((tw_fcl_population_kkm / base::sum(tw_fcl_population_kkm))^2 * var_robust_fcl),
        sd_robust_i = base::sqrt(var_robust),
        sd_robust_p = 100 * base::sqrt(var_robust),
        em_robust = base::round(-stats::qt(0.025, n_trp - 1) * sd_robust_p, 2),
        em_robust_i = base::round(-stats::qt(0.025, n_trp - 1) * sd_robust_i, 2),
        # Variance: ratio estimator
        #var_re = (1/base::sum(tw_fcl_population_kkm)^2) * base::sum(var_re_tw_b_fcl + tw_fcl_population_kkm^2 * var_re_sys_fcl^2),
        #em_re = base::round(-stats::qt(0.025, n_trp - 1) * 100 * base::sqrt(var_re), 2), # Way too big! Something's wrong...
        #
        ci_lower = index_p - em_robust,
        ci_upper = index_p + em_robust,
        .by = universal_year_period_id_end
      )  |>
      dplyr::left_join(
        universal_calendar_periods,
        by = dplyr::join_by(universal_year_period_id_end == universal_year_period_id)
      ) |>
      dplyr::select(
        universal_year_period_id = universal_year_period_id_end,
        x_label,
        index_i,
        index_p,
        index_p_beale,
        n_trp,
        var_pop_model,
        em_pop_model,
        var_robust,
        em_robust,
        em_robust_i,
        ci_lower,
        ci_upper
        #var_re,
        #em_re
      )

  # Compare with index without post stratification by function class
  # window_index <-
  #   trp_window_index |>
  #   dplyr::left_join(
  #     trp_weights,
  #     by = "trp_id"
  #   ) |>
  #   dplyr::summarise(
  #     index_i = base::sum(mean_mdt_window * length_m) / base::sum(mean_mdt_base * length_m),
  #     index_p = 100 * (index_i - 1),
  #     n_trp = n(),
  #     .by = c(universal_year_period_id_end)
  #   )
  #
  # compare_windows <-
  #   dplyr::left_join(
  #     window_index_f |> dplyr::select(universal_year_period_id_end, index_p_f = index_p),
  #     window_index   |> dplyr::select(universal_year_period_id_end, index_p),
  #     by = "universal_year_period_id_end"
  #   )

  return(window_index_post_stratified)

}


#rolling_index_area_bootstrap <- function(trp_window_index) {

  # Plain bootstrap, let post-stratification adjust for bias
  # Pseudopopulation
  # Non-random sample

  # Take a bootstrapped sample from each period,
  # calculate its area index


#}

# calculate_index_p_bs <- function(trp_window_index) {
#
#   index_p_df <-
#     trp_window_index |>
#     dplyr::left_join(trp_weights, by = "trp_id") |>
#     dplyr::mutate(
#       tw_a = mean_mdt_a * length_m,
#       tw_b = mean_mdt_b * length_m
#     ) |>
#     dplyr::select(
#       universal_year_period_id_end,
#       trp_id,
#       tw_a, tw_b,
#       function_class,
#       tw_fcl_population_kkm
#     )
#
#   stratified_index_p <-
#     index_p_df |>
#     dplyr::summarise(
#       index_i = base::sum(tw_b) / base::sum(tw_a),
#       index_p = 100 * (index_i - 1),
#       .by = c(universal_year_period_id_end, function_class, tw_fcl_population_kkm)
#     ) |>
#     dplyr::summarise(
#       index_p = (base::sum(index_p * tw_fcl_population_kkm) / base::sum(tw_fcl_population_kkm)) |> base::round(2),
#       .by = universal_year_period_id_end
#     )
#
# }

# bootsurv::pseudopop.boot.stsrs ???


rolling_index_multiple_years <- function(one_year_rolling_index_df, n_rolling_years) {

  # No need to have start and end period as input, this is given implicit in one_year_rolling_index_df

  base::stopifnot(n_rolling_years %in% c(2, 3))

  first_end_period_in_multiple_year_window <-
    one_year_rolling_index_df |>
    dplyr::slice_min(universal_year_period_id) |>
    purrr::pluck("universal_year_period_id")

  last_period <-
    one_year_rolling_index_df |>
    dplyr::slice_max(universal_year_period_id) |>
    purrr::pluck("universal_year_period_id")

  last_end_period_in_multiple_year_window <-
    last_period - (n_rolling_years - 1) * 14


  window_indexes <- tibble::tibble()

  for(j in first_end_period_in_multiple_year_window:last_end_period_in_multiple_year_window) {

    window_index_j <-
      one_year_rolling_index_df |>
      dplyr::filter(
        universal_year_period_id %in% base::seq(j, j + (n_rolling_years - 1) * 14, 14)
      ) |>
      dplyr::summarise(
        index_p = base::mean(index_p),
        # TODO: covariance?
        var_pop_model_rolling = base::sum(var_pop_model) / n_rolling_years^2,
        sd_pop_model_rolling_p = 100 * base::sqrt(var_pop_model_rolling),
        em_pop_model_rolling = base::round(-stats::qnorm(0.025) * sd_pop_model_rolling_p, 2),
        #
        var_robust_rolling = base::sum(var_robust) / n_rolling_years^2,
        sd_robust_rolling_p = 100 * base::sqrt(var_robust_rolling),
        em_robust_rolling = base::round(-stats::qnorm(0.025) * sd_robust_rolling_p, 2),
        ci_lower = index_p - em_robust_rolling,
        ci_upper = index_p + em_robust_rolling
      ) |>
      dplyr::mutate(
        # Last years end of window id
        universal_year_period_id = j + (n_rolling_years - 1) * 14
      ) |>
      dplyr::select(
        -tidyselect::starts_with("var_"),
        -tidyselect::starts_with("sd_")
      )

    window_indexes <-
      dplyr::bind_rows(
        window_indexes,
        window_index_j
      )

  }

  window_indexes <-
    window_indexes |>
    dplyr::left_join(
      universal_calendar_periods,
      by = dplyr::join_by(universal_year_period_id)
    ) |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower, ci_upper
    )

  return(window_indexes)

}

# covariance_rolling <-
#   cov(
#     dplyr::inner_join(
#       trp_window_index |> dplyr::filter(universal_year_period_id_end == 56),
#       trp_window_index |> dplyr::filter(universal_year_period_id_end == 84),
#       by = "trp_id"
#     ) |>
#       dplyr::select(
#         starts_with("index_p")
#       )
#   )


# Compare ----
prepare_rolling_indexes_for_comparison <- function(rolling_index_df) {

  rolling_index_df |>
    dplyr::mutate(
      ci_width = ci_upper - ci_lower
    ) |>
    dplyr::select(
      index_p,
      n_trp,
      n_eff,
      ci_width,
      index_period,
      window
    )

}


# Visualize ----
#trp_mdt_long_format <- test
create_mdt_barplot <- function(trp_mdt_long_format) {

  #min_month_object <- min(trp_mdt_long_format$month_object)
  #max_month_object <- max(trp_mdt_long_format$month_object)

  trp_mdt_long_format %>%
    dplyr::mutate(
      year = as.character(year)
    ) %>%
    ggplot2::ggplot(
      aes(
        x = month, #month_object,
        y = mdt,
        fill = year,
        pattern = valid_quality
      )
    ) +
    ggpattern::geom_col_pattern(
      position = "dodge", #position_dodge(preserve = 'single'),
      pattern_density = 0.5,
      pattern_spacing = 0.03
    ) +
    geom_text(
      aes(
        x = month, #month_object,
        label = if_else(mdt == 0, "NA", "")#NULL)
      ),
      position = position_dodge(0.8),
      vjust = 1
    ) +
    ggplot2::facet_grid(
      rows = vars(road_category_and_number_and_point_name),
      labeller = label_wrap_gen(width = 12),
      scales = "free_y"
    ) +
    theme_light(base_size = 10) +
    theme(
      #axis.text.x = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 15, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text.y = element_text(angle = 90),
      strip.background = element_rect(fill = "#444f55"),
      plot.background = element_rect(fill = svv_background_color),
      panel.background = element_rect(fill = svv_background_color),
      legend.position = "bottom",
      legend.background = element_rect(fill = svv_background_color),
      legend.key = element_blank()
    ) +
    # scale_x_date(
    #   breaks = scales::breaks_width("months"),
    #   labels = scales::label_date("%b"),
    #   #limits = c(min_month_object, max_month_object)
    # ) +
    scale_fill_viridis_d(
      name = "\u00c5r",
      option = "viridis"
    ) +
    ggpattern::scale_pattern_manual(
      name = "Godkjent",
      values = c(
        "TRUE" = "none",
        "FALSE" = "stripe"
      )
    ) +
    scale_x_continuous(
      #labels = as.character(month),
      breaks = seq(1, 12, 1)
    ) +
    labs(
      x = NULL,
      y = "M\u00e5nedsd\u00f8gntrafikk",
      caption = "Data: Statens vegvesen og fylkeskommunene"
    ) +
    ggtitle(
      "Gjennomsnittlig antall passeringer per dag",
      subtitle = ""
    )
}


barplot_cmdt <- function(cmdt_df) {

  cmdt_df |>
    ggplot2::ggplot(
      aes(
        x = month,
        y = mdt,
        fill = year
      )
    ) +
    ggplot2::geom_col(
      position = "dodge"
    ) +
    ggplot2::facet_grid(
      rows = vars(road_category_and_number_and_point_name),
      labeller = label_wrap_gen(width = 12),
      scales = "free_y"
    ) +
    theme_light(base_size = 10) +
    theme(
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 15, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text.y = element_text(angle = 90),
      strip.background = element_rect(fill = "#444f55"),
      plot.background = element_rect(fill = svv_background_color),
      panel.background = element_rect(fill = svv_background_color),
      legend.position = "bottom",
      legend.background = element_rect(fill = svv_background_color),
      legend.key = element_blank()
    ) +
    scale_fill_viridis_d(
      name = "\u00c5r",
      option = "viridis"
    ) +
    labs(
      x = NULL,
      y = "M\u00e5nedsd\u00f8gntrafikk",
      caption = "Data: Statens vegvesen og fylkeskommunene"
    ) +
    ggtitle(
      "Gjennomsnittlig antall passeringer per dag",
      subtitle = ""
    )
}








