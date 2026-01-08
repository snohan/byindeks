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

  # Test
  # link_df <- links_with_trp
  # trp_index_df <- point_index_new_prepared_1

  links_with_trp_index <-
    link_df |> 
    dplyr::left_join(
      trp_index_df,
      by = "trp_id"
    ) |> 
    dplyr::left_join(
      trps_meta,
      by = "trp_id"
    ) |> 
    dplyr::mutate(
      info_text =
        paste0(
          trp_id, "<br/>",
          name, "<br/>",
          "Indeks alle: ", index_total_p, "<br/>",
          "Indeks korte: ", index_short, "<br/>",
          "Indeks lange: ", index_long
        )
    ) |> 
    dplyr::select(
      link_id, info_text, index_total_p
    ) 

}

find_trps_without_links <- function(trp_index_df) {

  trps_without_links <- 
    trps_meta |> 
    dplyr::filter(
      !(trp_id %in% links_with_trp$trp_id)
    ) |> 
    dplyr::left_join(
      trp_index_df,
      by = "trp_id"
    ) |> 
    dplyr::mutate(
      info_text =
        paste0(
          trp_id, "<br/>",
          name, "<br/>",
          "Indeks alle: ", index_total_p, "<br/>",
          "Indeks korte: ", index_short, "<br/>",
          "Indeks lange: ", index_long
        ),
      info_text = lapply(info_text, htmltools::HTML)
    ) |> 
    dplyr::select(
      trp_id, info_text, index = index_total_p, lat, lon
    )

}


# join_links_and_events_by_geometry <- function(link_df, event_df) {

#   # Do the join, but keep only ids
#   links_with_events <- 
#     link_df |> 
#     dplyr::select(link_id) |> 
#     sf::st_join(
#       event_df,
#       join = "st_crosses",
#       left = FALSE,
#       largest = TRUE
#     ) |> 
#     sf::st_drop_geometry() |> 
#     dplyr::select(link_id, event_id, info_text)

# }