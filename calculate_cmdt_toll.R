calculate_cmdt_toll <- function(toll_station_id, year_chosen) {

  # Need to have the df "tolling_data_daily_all_years" in environment

  # Testing:
  #toll_station_id <- "51"
  #year_chosen <- 2019

  # Easter and Pentecost days
  complete_calendar <- make_complete_calendar_year(year_chosen)
  classified_days <- classify_days(year_chosen) |> dplyr::select(date, non_working_day)
  n_days_in_calendar <- number_of_days(year_chosen)

  dt <-
    tolling_data_daily_all_years |>
    dplyr::filter(
      trp_id == toll_station_id,
      year == year_chosen
    ) |>
    # To make these data compatible with TRP data
    dplyr::mutate(
      length_class =
        dplyr::case_when(
          class == "alle" ~ "alle",
          class == "lette" ~ "korte",
          class == "tunge" ~ "lange"
        ),
      length_class = base::factor(length_class, levels = c("alle", "korte", "lange"))
    )  |>
    dplyr::select(
      trp_id,
      length_class,
      date,
      volume = traffic
    ) |>
    # Need to change id to NVDB ID for data to match IDs on links
    dplyr::left_join(
      toll_nvdb_id,
      by = "trp_id"
    ) |>
    dplyr::select(-trp_id) |>
    dplyr::rename(trp_id = nvdb_id) |>
    dplyr::left_join(
      complete_calendar,
      by = dplyr::join_by(date)
    )

  if(nrow(dt) > 0) {
    # Easter and Pentecost MDTs
    mdt_easter <-
      dt |>
      dplyr::filter(
        month %in% c("påske", "pinse")
      ) |>
      dplyr::summarise(
        mdt = base::mean(volume, na.rm = FALSE) |> round(-1),
        n_days_in_data = n(),
        .by = c(trp_id, length_class, month)
      ) |>
      dplyr::filter(
        !(month == "påske" & n_days_in_data < 6),
        !(month == "pinse" & n_days_in_data != 4)
      ) |>
      dplyr::arrange(month, length_class)

    # Classify non-working days
    ydt <-
      dt |>
      dplyr::filter(
        !(month %in% c("påske", "pinse"))
      ) |>
      dplyr::left_join(
        classified_days,
        by = dplyr::join_by(date)
      ) |>
      dplyr::summarise(
        mdt = base::mean(volume, na.rm = FALSE),
        n_days_in_data = n(),
        .by = c(trp_id, length_class, month, non_working_day)
      )

    # If a day type has little data
    ok_months <-
      ydt |>
      dplyr::select(
        month, non_working_day, n_days_in_data
      ) |>
      dplyr::distinct() |>
      dplyr::filter(
        non_working_day == TRUE  & n_days_in_data >= 3 |
          non_working_day == FALSE & n_days_in_data >= 7
      ) |>
      dplyr::mutate(
        month_count = n(),
        .by = month
      ) |>
      dplyr::filter(
        month_count == 2
      )

    if(nrow(ok_months) > 0) {

      mdt_weighted <-
        ydt |>
        dplyr::filter(
          (month %in% ok_months$month)
        ) |>
        dplyr::arrange(month, length_class, non_working_day) |>
        dplyr::left_join(
          day_type_weights_relative,
          by = dplyr::join_by(month, non_working_day)
        ) |>
        dplyr::summarise(
          mdt = sum(mdt * weight) |> round(-1),
          n_days_in_data = sum(n_days_in_data),
          .by = c(trp_id, length_class, month)
        )
    }else{
      mdt_weighted <- tibble::tibble()
    }

    if(nrow(ok_months) > 0 | nrow(mdt_easter) > 0) {

      mdt <-
        dplyr::bind_rows(
          mdt_easter,
          mdt_weighted
        ) |>
        dplyr::arrange(month, length_class) |>
        dplyr::left_join(
          n_days_in_calendar |> dplyr::rename(n_days_in_calendar = n_days),
          by = dplyr::join_by(month)
        ) |>
        dplyr::mutate(
          coverage_percentage = 100 * n_days_in_data / n_days_in_calendar,
          year = year_chosen
        ) |>
        dplyr::relocate(
          year,
          .before = month
        )
    }else{
      mdt <- tibble::tibble()
    }

  }else{
    mdt <- tibble::tibble()
  }

  return(mdt)
}