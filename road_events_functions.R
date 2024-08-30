# Filter events by month and join with links

# Events this month ----
filter_events_by_month <- function(index_month_chosen) {

  month_start <-
    paste0(
      index_year - 1,
      "-",
      stringr::str_pad(index_month_chosen, width = 2, pad = "0"),
      "-01"
    ) |>
    lubridate::ymd(tz = "CET")

  month_end <- month_start + base::months(1) - lubridate::days(1)
  this_month_interval_base <- lubridate::interval(month_start, month_end)
  # TODO: find events in calculation year when these are available

  events_this_month <-
    events |>
    dplyr::mutate(
      this_month =
        lubridate::int_overlaps(
          interval,
          this_month_interval_base
        )
    ) |>
    dplyr::filter(
      this_month
    )

  return(events_this_month)
}


# Join ----
# TODO: Would be faster if events were filtered by county beforehand
join_events_and_trp_index_with_links <- function(index_month_chosen, point_index_new_prepared_df) {

  events_and_links <-
    filter_events_by_month(index_month_chosen) |>
    sf::st_join(
      links_in_county |>
        dplyr::select(id),
      join = "st_crosses",
      largest = TRUE
    ) |>
    sf::st_drop_geometry() |>
    dplyr::select(
      description,
      allVehicles,
      passability,
      isWinterClosed,
      interval,
      id
    ) |>
    dplyr::filter(
      !is.na(id)
    ) |>
    dplyr::mutate(
      event_info =
        paste0(
          description, "<br/>",
          " Alle: ", allVehicles,
          ". Passerbarhet: ", passability,
          ". Vinterstengt: ", isWinterClosed, "<br/>",
          ". Tid: ", interval
        )
    ) |>
    dplyr::select(
      id,
      event_info
    ) |>
    # More than one event per link
    dplyr::summarise(
      event_text = paste(event_info, collapse = "<br/>"),
      .by = id
    )

  links_with_events_and_pointindex <-
    links_in_county |>
    dplyr::select(id) |>
    dplyr::left_join(
      links_with_trp,
      by = dplyr::join_by(id)
    ) |>
    dplyr::left_join(
      point_index_new_prepared_df,
      by = dplyr::join_by(this_area_trp_id == trp_id)
    ) |>
    dplyr::left_join(
      events_and_links,
      by = dplyr::join_by(id)
    ) |>
    dplyr::filter(
      !is.na(index_total_p) | !is.na(event_text)
    ) |>
    dplyr::left_join(
      trps_meta,
      by = dplyr::join_by(this_area_trp_id == trp_id)
    ) |>
    dplyr::mutate(
      label_text =
        paste(name, ", ",
              road_category_and_number, "<br/>",
              index_total_p, " %", "<br/>",
              index_total_coverage, " %", "<br/>",
              event_text),
      label_text = lapply(label_text, htmltools::HTML)
    ) |>
    dplyr::select(
      id,
      this_area_trp_id,
      label_text,
      index_total_p,
      event_text
    )

  return(links_with_events_and_pointindex)
}
