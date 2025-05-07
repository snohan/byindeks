# Index tidying functions

library(ggpattern)

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
#filename <- "data_index_raw/pointindex_trondheim-2019-12_2018.csv"
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


index_converter <- function(index) {
  ifelse(
    is.na(index),
    1,
    index/100 + 1)
}

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


# mdt_df <- mdt_validated
# window_length <- 36
# base_year <- reference_year
# last_year_month <- "2023-11-01"

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


