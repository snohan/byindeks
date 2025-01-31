# Filter traffic links

filter_traffic_links_by_county <- function(county_number) {

  link_ids_in_county <-
    links |>
    sf::st_drop_geometry() |>
    dplyr::select(
      id,
      countyIds
    ) |>
    tidyr::unnest(countyIds) |>
    dplyr::filter(
      countyIds %in% county_number
    ) |>
    dplyr::distinct()

  links_in_county <-
    links |>
    dplyr::filter(
      id %in% link_ids_in_county$id
    )

  return(links_in_county)
}


filter_links_with_trp_no_toll <- function() {

  # Just TRP
  links_with_trp <-
    dplyr::bind_rows(
      links |>
        sf::st_drop_geometry() |>
        dplyr::select(
          id,
          associatedTrpIds
        ) |>
        tidyr::unnest(
          # Will duplicate links with more than one TRP
          associatedTrpIds,
          keep_empty = FALSE
        ) |>
        dplyr::rename(
          this_area_trp_id = associatedTrpIds
        )
    ) |>
    # Narrow down list (duplicates disappear)
    dplyr::filter(
      this_area_trp_id %in% trps_meta$trp_id
    )
}


filter_links_with_trp <- function() {

  # Both TRP and Toll stations
  links_with_trp <-
    dplyr::bind_rows(
      links |>
        sf::st_drop_geometry() |>
        dplyr::select(
          id,
          associatedTrpIds
        ) |>
        tidyr::unnest(
          # Will duplicate links with more than one TRP
          associatedTrpIds,
          keep_empty = FALSE
        ) |>
        dplyr::rename(
          this_area_trp_id = associatedTrpIds
        ),
      links |>
        sf::st_drop_geometry() |>
        dplyr::select(
          id,
          associatedTollStationIds
        ) |>
        tidyr::unnest(
          # No duplicate
          associatedTollStationIds,
          keep_empty = FALSE
        ) |>
        dplyr::rename(
          this_area_trp_id = associatedTollStationIds
        ) |>
        dplyr::mutate(
          this_area_trp_id = as.character(this_area_trp_id)
        )
    ) |>
    # Narrow down list (duplicates disappear)
    dplyr::filter(
      this_area_trp_id %in% trps_meta$trp_id
    )
}


visualize_function_class_distribution <- function(link_population) {

  function_class_pop <-
    link_population |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      count = n(),
      tw = sum(traffic_work_km),
      .by = c(function_class)
    ) |>
    dplyr::mutate(
      total_count = sum(count),
      total_tw = sum(tw),
      percentage_count = count / total_count,
      percentage_tw = tw / total_tw,
      selection = "populasjon"
    )

  function_class_sam <-
    link_population |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(point_id)) |>
    dplyr::summarise(
      count = n(),
      tw = sum(traffic_work_km),
      .by = c(function_class)
    ) |>
    dplyr::mutate(
      total_count = sum(count),
      total_tw = sum(tw),
      percentage_count = count / total_count,
      percentage_tw = tw / total_tw,
      selection = "utvalg"
    )

  function_class_stats <-
    dplyr::bind_rows(
      function_class_pop,
      function_class_sam
    )

  function_class_tw_plot <-
    function_class_stats |>
    ggplot(aes(function_class, percentage_tw, group = selection, fill = selection)) +
    geom_col(position = "dodge2") +
    theme_light() +
    theme(
      axis.ticks.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.background = element_rect(fill = svv_background_color),
      panel.background = element_rect(fill = svv_background_color),
      legend.background = element_rect(fill = svv_background_color),
      legend.position = "bottom"
    ) +
    scale_fill_manual(
      values = c(
        "populasjon" = "#008ec2",
        "utvalg" = "#ed9300"
      ),
      labels = c(
        "Alle trafikklenker (populasjon)",
        "Trafikklenker med trafikkregistrering"
      ),
      name = "Utvalg"
    ) +
    labs(
      x = "Funksjonsklasse",
      y = NULL
    ) +
    ggtitle(
      "Trafikkarbeid fordelt på funksjonsklasse",
      "Data for 2024"
    )

  return(function_class_tw_plot)
}

map_links_with_function_class <- function(link_df) {

  links_with_trp <-
    link_df |>
    dplyr::filter(
      !is.na(point_id)
    )

  links_without_trp <-
    link_df |>
    dplyr::filter(
      is.na(point_id)
    )

  palette_function_class <-
    colorFactor(
      palette = c("#158925", "#077197", "#b63434", "#687277"),
      domain = c("A", "B", "C", "D")
    )

  map <-
    #link_df |>
    links_without_trp |>
    leaflet(
      width = "100%",
      height = 700,
      options =
        leafletOptions(
          crs = nvdb_crs,
          zoomControl = F)
    ) |>
    addTiles(
      urlTemplate = nvdb_map_url,
      attribution = nvdb_map_attribution
    ) |>
    addPolylines(
      #data = links_without_trp,
      #label = ~label_text,
      opacity = 1,
      weight = 4,
      color = ~ palette_function_class(function_class),
      highlightOptions = highlightOptions(
        bringToFront = TRUE,
        sendToBack = FALSE,
        color = "purple",
        opacity = 0.6
      )
    ) |>
    addPolylines(
      data = links_with_trp,
      #label = ~label_text,
      opacity = 1,
      weight = 8,
      color = ~ palette_function_class(function_class),
      highlightOptions = highlightOptions(
        bringToFront = TRUE,
        sendToBack = FALSE,
        color = "purple",
        opacity = 0.6
      )
    ) |>
    addLegend(
      "bottomright",
      pal = palette_function_class,
      values = ~ function_class,
      title = "Funksjonsklasse",
      opacity = 0.7
    )

  return(map)
}

map_caption <- "Trafikklenker og funksjonsklasse. Brede linjer indikerer lenker med trafikkregistrering."

table_trps <- function(trp_df) {

  trp_df |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(point_id)) |>
    dplyr::select(
      name,
      road_reference,
      aadt_lmv,
      function_class
    ) |>
    dplyr::mutate(
      ordered_road_reference =
        base::factor(
          road_reference,
          levels = stringr::str_sort(unique(road_reference), numeric = TRUE),
          ordered = TRUE
        )
    ) |>
    dplyr::arrange(
      ordered_road_reference
    ) |>
    dplyr::select(
      -ordered_road_reference
    ) |>
    DT::datatable(
      filter = "none",
      rownames = FALSE,
      colnames = c(
        "Navn på punkt" = "name",
        "Vegreferanse" = "road_reference",
        "ÅDT lette kjøretøy" = "aadt_lmv",
        "Funksjonsklasse" = "function_class"
      ),
      options = list(
        searching = FALSE,
        pageLength = 20,
        lengthChange = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-left', targets = c(0, 1)))
      )
    )
}


table_link_statistics <- function(link_df) {

  dplyr::bind_rows(
    link_df |>
      sf::st_drop_geometry() |>
      dplyr::summarise(
        number_of_links = n(),
        traffic_work_mill_km = sum(traffic_work_km) / 1e6
      ) |>
      dplyr::mutate(
        selection = "population"
      ),
    link_df |>
      sf::st_drop_geometry() |>
      dplyr::filter(!is.na(point_id)) |>
      dplyr::summarise(
        number_of_links = n(),
        traffic_work_mill_km = sum(traffic_work_km) / 1e6
      ) |>
      dplyr::mutate(
        selection = "sample"
      )
  ) |>
    tidyr::pivot_longer(
      cols = c(number_of_links, traffic_work_mill_km),
      names_to = "quantity"
    ) |>
    tidyr::pivot_wider(
      names_from = selection,
      values_from = value
    ) |>
    dplyr::mutate(
      percentage = 100 * (sample / population),
      quantity =
        dplyr::case_when(
          quantity == "number_of_links" ~ "Antall trafikklenker",
          quantity == "traffic_work_mill_km" ~ "Trafikkarbeid (mill. km)"
        )
    ) |>
    flextable::flextable() |>
    colformat_double(j = 2:4, digits = 0) |>
    set_header_labels(
      quantity = "Størrelse",
      population = "Populasjon",
      sample = "Utvalg",
      percentage = "Prosentandel (%)"
    ) |>
    bold(part = "header") |>
    bg(bg = "#ED9300", part = "header")
}


calculate_ci_width <- function(link_df) {

  weights <-
    link_df |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(point_id)) |>
    dplyr::select(aadt_lmv) |>
    dplyr::mutate(
      sum_aadt_lmv = sum(aadt_lmv),
      weight = aadt_lmv / sum_aadt_lmv,
      squared_weight = weight^2
    )

  ci_width <- round(1.96 * sqrt(sum(weights$squared_weight)) * 5, 1)

  return(ci_width)

}