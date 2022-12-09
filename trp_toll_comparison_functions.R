#

read_apar_csv <- function(apar_csv) {

  readr::read_csv(
    apar_csv
  ) |>
    dplyr::rename(
      traffic_volume = 'Accepted passages',
      vehicle_class = "vehicle class"
    ) |>
    dplyr::mutate(
      vehicle_class =
        dplyr::case_when(
          stringr::str_detect(vehicle_class, "^Small.*") ~ "Liten",
          stringr::str_detect(vehicle_class, "^Large.*") ~ "Stor",
          TRUE ~ "Ukjent"
        ),
      source = "AutoPASS"
    )
}

create_zoned_date <- function(datestring) {

  datestring |>
    #clock::date_parse() |>
    clock::as_naive_time() |>
    clock::as_zoned_time("CET")
}

get_trp_dt <- function(trp_id, from_date, to_date) {

  # dates: "2022-12-08"

  get_dt_by_length_for_trp(
      trp_id = trp_id,
      from = create_zoned_date(from_date),
      to = create_zoned_date(to_date)
    ) |>
    dplyr::filter(
      length_range %in% c("[..,5.6)", "[5.6,..)")
    ) |>
    dplyr::mutate(
      vehicle_class =
        dplyr::case_when(
          length_range == "[..,5.6)" ~ "Liten",
          length_range == "[5.6,..)" ~ "Stor"
        ),
      source = "TRP"
    ) |>
    dplyr::select(
      date = from,
      traffic_volume = length_range_volume,
      vehicle_class,
      source
    )
}


create_compare_string <- function(mean_diff) {

  comparison_number <-
    paste0(
      as.character(abs(mean_diff))
    )

  comparison_string <-
    dplyr::case_when(
      mean_diff > 0 ~ paste0(comparison_number, " flere kjøretøy enn"),
      mean_diff < 0 ~ paste0(comparison_number, " færre kjøretøy enn"),
      mean_diff == 0 ~ "like mange kjøretøy som"
    )

  return(comparison_string)

}

visualize_comparison_total_traffic <-
  function(
    daily_traffic
  ) {

    svv_background_color <- "#F5F5F5"

    daily_traffic |>
      ggplot(
        aes(
          x = date,
          y = traffic_volume,
          color = source
        )
      ) +
      geom_line(
        linewidth = 2,
        alpha = 0.5
      ) +
      scale_color_manual(
        values =
          c("TRP" = "#ed1c2e",
            "AutoPASS" = "#008ec2"),
        name = "Kilde"
      ) +
      theme_light() +
      theme(
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5
        ),
        axis.text.y = element_text(vjust = 0.5),
        plot.caption =
          element_text(
            face = "italic",
            size = 8,
            lineheight = 1.5,
            vjust = 0
          ),
        plot.background = element_rect(fill = svv_background_color),
        panel.background = element_rect(fill = svv_background_color),
        legend.background = element_rect(fill = svv_background_color),
        legend.key = element_blank()
      ) +
      labs(
        x = NULL,
        y = "Trafikkmengde",
        caption = "Data: Statens vegvesen, fylkene."
      ) +
      ggtitle(
        "Trafikkmendge registrert per dag"
      )
  }
