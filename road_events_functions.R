# Filter events by month and join with links

get_events_in_year_month <- function(events_df, year_dbl, index_month_dbl) {

  month_start <-
    paste0(
      year_dbl,
      "-",
      stringr::str_pad(index_month_dbl, width = 2, pad = "0"),
      "-01"
    ) |>
    lubridate::ymd(tz = "CET")

  month_end <- month_start + base::months(1) - lubridate::days(1)
  month_interval <- lubridate::interval(month_start, month_end)
  
  events_this_year_month <-
    events_df |>
    dplyr::mutate(
      this_month =
        lubridate::int_overlaps(
          interval,
          month_interval
        )
    ) |>
    dplyr::filter(
      this_month
    )

  return(events_this_year_month)
}


join_links_and_trp_index <- function(link_df, trp_index_df) {

  links_with_trp_index <-
    link_df |> 
    dplyr::left_join(
      trp_index_df,
      by = "trp_id"
    ) |> 
    dplyr::filter(
      !is.na(index_total_p)
    ) |> 
    dplyr::left_join(
      trps_meta,
      by = "trp_id"
    ) |> 
    dplyr::mutate(
      info_text =
        paste0(
          trp_id, " ", name, "<br/>",
          "Indeks alle: ", index_total_p, "<br/>",
          "Indeks korte: ", index_short, "<br/>",
          "Indeks lange: ", index_long
        )
    ) |> 
    dplyr::select(
      link_id, info_text, index_total_p
    )

}


join_links_and_events_by_geometry <- function(link_df, event_df) {

  # Do the join, but keep only ids
  links_with_events <- 
    link_df |> 
    dplyr::select(link_id) |> 
    sf::st_join(
      event_df,
      join = "st_crosses",
      left = FALSE,
      largest = TRUE
    ) |> 
    sf::st_drop_geometry() |> 
    dplyr::select(link_id, event_id, info_text)

}


prepare_links_for_mapping <- function(base_year_dbl, calc_year_dbl, index_month_dbl, trp_index_df) {

  events_b <- get_events_in_year_month(events, base_year_dbl, index_month_dbl)
  events_c <- get_events_in_year_month(events, calc_year_dbl, index_month_dbl)

  links_with_trp_index <- join_links_and_trp_index(links_with_trp, trp_index_df)

  links_with_events_b <- join_links_and_events_by_geometry(links_in_area, events_b)
  links_with_events_c <- join_links_and_events_by_geometry(links_in_area, events_c)

  # Per link
  links_with_info <-
    dplyr::bind_rows(
      links_with_trp_index,
      links_with_events_b,
      links_with_events_c
    ) |> 
    dplyr::select(
    link_id, info_text, index_total_p 
    ) |> 
    dplyr::distinct() |> 
    dplyr::summarise(
      text = paste(info_text, collapse = "<br/>"),
      .by = c(link_id, index_total_p)
    ) |> 
    dplyr::mutate(
      text = lapply(text, htmltools::HTML)
    )

  links_for_map <- 
    links_in_county |> 
    dplyr::filter(link_id %in% links_with_info$link_id) |> 
    dplyr::left_join(links_with_info, by = "link_id") |> 
    dplyr::select(link_id, text, index_total_p)
    
}