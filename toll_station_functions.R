plot_toll_station_data_per_lane <- function(toll_id_chosen, year_chosen, toll_station_info_df) {

  # Test:
  # toll_id_chosen <- 2
  # year_chosen <- 2025

  toll_station_name <-
    toll_station_info_df |>
    dplyr::filter(trp_id == toll_id_chosen) |>
    dplyr::select(name) |>
    purrr::pluck(1)

  data_here <- 
    tolling_data_daily_lane |>
    dplyr::filter(
      trp_id == toll_id_chosen,
      year %in% c(year_chosen),
      class == "lette"
    )
  
  weekday_medians <-
    data_here |> 
    dplyr::summarise(
      traffic = median(traffic),
      .by = c(weekday, lane)
    )
  
  weekday_medians_repeated <-
    tibble::tibble(
      weekday = c(rep(1:7, 6)),
      day_aligned_by_weekday = c(1:42)
    ) |> 
    dplyr::left_join(
      weekday_medians,
      by = "weekday",
      relationship ="many-to-many"
    )

  data_here |>
    ggplot2::ggplot(aes(day_aligned_by_weekday, traffic)) +
    ggplot2::geom_line(linewidth = 1, color = "#FF9600") +
    ggplot2::geom_line(data = weekday_medians_repeated, color = "#DADADA") +
    ggplot2::facet_grid(
      rows = vars(month),
      cols = vars(lane)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(
      breaks = base::seq(1, 36, by = 7),
      minor_breaks = c(1:42)
    ) +
    ggplot2::ggtitle(toll_station_name)

}


calculate_monthly_index_for_tolling_stations_from_daily_traffic <- function(daily_class_data, baseyear, calcyear) {

  # Test:
  # daily_class_data <- tolling_data_daily_all_years
  # baseyear <- 2021
  # calcyear <- 2022
  
  basedata <-
    daily_class_data |> 
    dplyr::filter(year == baseyear) |>
    dplyr::mutate(month_number = lubridate::month(date))

  calcdata <-
    daily_class_data |> 
    dplyr::filter(year == calcyear) |>
    dplyr::mutate(month_number = lubridate::month(date))

  indexdata <-
    dplyr::inner_join(
      basedata,
      calcdata,
      by = c("day", "month_number", "trp_id", "class"),
      suffix = c("_base", "_calc"),
    ) |>
    dplyr::group_by(
      trp_id,
      class,
      month_calc
    ) |>
    dplyr::summarise(
      monthly_volume_base = sum(traffic_base),
      monthly_volume_calc = sum(traffic_calc),
      n_days = n(),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      index_p =
        (monthly_volume_calc / monthly_volume_base - 1) * 100 |> 
        round(digits = 2),
      n_days_of_month = lubridate::days_in_month(month_calc),
      # TODO: should count no Feb as 29?
      coverage = (n_days / n_days_of_month) * 100
      # NB! Not correct for HMV as some stations have days without any HMVs
    )
}
