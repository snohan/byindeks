# Functions ----
calculate_hourly_index_traffic <- function(traffic_by_length) {

  # IN: hourly traffic by length and lane
  # OUT: hourly traffic when all lanes present

  data_tidy <-
    traffic_by_length |>
    tibble::as_tibble() |>
    dplyr::mutate(
      length_quality = round(length_quality),
      total_coverage = round(total_coverage)
    ) |>
    dplyr::filter(
      length_range == "[..,5.6)",
      length_quality >= 95, # TODO: remove this filter
      # can't remove single hours based on this. lenght diff is based on daily values in order to avoid
      # nightly hours with little traffic be filtered too often
      # same reasoning behind filtering by total coverage and not length coverage here
      total_coverage >= 99
    )

  # lanes_each_month <-
  #   data_tidy |>
  #   dplyr::mutate(
  #     month = lubridate::month(from)
  #   ) |>
  #   dplyr::select(
  #     lane,
  #     month
  #   ) |>
  #   dplyr::distinct() |>
  #   dplyr::summarise(
  #     lanes_month = base::paste(lane, collapse = "#"),
  #     .by = month
  #   )

  hourly_traffic <-
    data_tidy |>
    dplyr::select(
      from,
      traffic,
      #lane
    ) |>
    #dplyr::summarise(
    #  lanes_hour = base::paste(lane, collapse = "#"),
    #  traffic = sum(traffic),
    #  .by = from
    #) |>
    dplyr::mutate(
      month = lubridate::month(from)
    ) |>
    #dplyr::left_join(
    #  lanes_each_month,
    #  by = join_by(month)
    #) |>
    #dplyr::filter(
    #  lanes_hour == lanes_month,
    #) |>
    dplyr::mutate(
      day = lubridate::day(from),
      hour = lubridate::hour(from)
    ) |>
    dplyr::select(
      month,
      day,
      hour,
      #lanes_hour,
      traffic
    ) |>
    # Adding the two hours when DST is set back in October
    dplyr::summarise(
      traffic = sum(traffic),
      .by = c(tidyselect::everything(), -traffic)
    )

  return(hourly_traffic)
}


calculate_trp_index <- function(subfolder_name, trp_id, base_year, calc_year) {

  # Testing:
  # trp_id <- trp_2017[16]
  # trp_id <- "02636V1125920"
  # calc_year <- 2021
  # base_year <- 2019

  ht_base_year <-
    get_hourly_traffic_by_length(
      trp_id,
      paste0(as.character(base_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(base_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    calculate_hourly_index_traffic()

  dt_base_year <-
    get_dt_by_length_for_trp(
      trp_id,
      paste0(as.character(base_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(base_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    dplyr::select(
      from,
      length_quality_base = length_quality
      # yes, must use length_quality and not length_coverage,
      # e.g. if just 16 hours for a day, length_quality might be high, but length_coverage will be low
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      month = lubridate::month(from),
      day = lubridate::mday(from),
      length_quality_base = round(length_quality_base)
    ) |>
    dplyr::select(
      -from
    )

  ht_calc_year <-
    get_hourly_traffic_by_length(
      trp_id,
      paste0(as.character(calc_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(calc_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    calculate_hourly_index_traffic()

  dt_calc_year <-
    get_dt_by_length_for_trp(
      trp_id,
      paste0(as.character(calc_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(calc_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    dplyr::select(
      from,
      length_quality_calc = length_quality
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      month = lubridate::month(from),
      day = lubridate::mday(from),
      length_quality_calc = round(length_quality_calc)
    ) |>
    dplyr::select(
      -from
    )

  trp_index_data <-
    dplyr::inner_join(
      ht_base_year,
      ht_calc_year,
      by = join_by(month, day, hour),
      #by = join_by(month, day, hour, lanes_hour),
      suffix = c("_base", "_calc")
    ) |>
    # Add filter for parts of day
    # Remember to remove criteria of >=16 hours further down
    dplyr::filter(
      hour %in% c(9:14, 17:23)
    ) |> 
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_hours = n(),
      .by = c(month, day)
    ) |>
    # remove days with less than 95 % length quality
    dplyr::left_join(
      dt_base_year,
      by = join_by(month, day)
    ) |>
    dplyr::left_join(
      dt_calc_year,
      by = join_by(month, day)
    ) |>
    dplyr::mutate(
      ok_length = length_quality_calc >= 95 & length_quality_base >= 95
    ) |>
    dplyr::filter(
      # n_hours >= 16,
      ok_length == TRUE
    ) |>
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_days = n(),
      .by = c(month)
    ) |>
    dplyr::filter(
      n_days >= 16
    ) |>
    dplyr::mutate(
      trp_id = trp_id,
      years = paste0(as.character(base_year), "-", as.character(calc_year))
    )

  readr::write_rds(
    trp_index_data,
    file = paste0("trp_index/", subfolder_name, "/", trp_id, "_", base_year, "_", calc_year, ".rds")
  )

  return(trp_index_data)
}


# Without quality metrics ----
calculate_hourly_index_traffic_without_quality <- function(traffic_by_length) {

  # IN: hourly traffic by length and lane
  # OUT: hourly traffic when all lanes present

  hourly_traffic <-
    traffic_by_length |>
    tibble::as_tibble() |>
    dplyr::filter(length_range == "[..,5.6)") |>
    dplyr::select(
      from,
      traffic
    ) |>
    dplyr::mutate(
      month = lubridate::month(from),
      day = lubridate::day(from),
      hour = lubridate::hour(from)
    ) |>
    dplyr::select(
      month,
      day,
      hour,
      traffic
    ) |>
    # Adding the two hours when DST is set back in October
    dplyr::summarise(
      traffic = sum(traffic),
      .by = c(tidyselect::everything(), -traffic)
    )

  return(hourly_traffic)
}


calculate_trp_index_without_quality <- function(subfolder_name, trp_id, base_year, calc_year) {

  ht_base_year <-
    get_hourly_traffic_by_length(
      trp_id,
      paste0(as.character(base_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(base_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    calculate_hourly_index_traffic_without_quality()

  ht_calc_year <-
    get_hourly_traffic_by_length(
      trp_id,
      paste0(as.character(calc_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(calc_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    calculate_hourly_index_traffic_without_quality()

  trp_index_data <-
    dplyr::inner_join(
      ht_base_year,
      ht_calc_year,
      by = join_by(month, day, hour),
      suffix = c("_base", "_calc")
    ) |>
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_hours = n(),
      .by = c(month, day)
    ) |>
    dplyr::filter(
      n_hours >= 16
    ) |>
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_days = n(),
      .by = c(month)
    ) |>
    dplyr::filter(
      n_days >= 16
    ) |>
    dplyr::mutate(
      trp_id = trp_id,
      years = paste0(as.character(base_year), "-", as.character(calc_year))
    )

  readr::write_rds(
    trp_index_data,
    file = paste0("trp_index/", subfolder_name, "/", trp_id, "_", base_year, "_", calc_year, ".rds")
  )

  return(trp_index_data)
}
