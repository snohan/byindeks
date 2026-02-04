# Utils ----
borderline <- officer::fp_border(color = "black", style = "solid", width = 1)


# TRP tables ----
create_point_table <- function(all_point_info_df) {

  all_point_info_df %>%
    select(
      name,
      road_reference,
      adt_ref,
      adt,
      year_aadt
    ) %>%
    flextable() %>%
    set_header_labels(
      name = "Punktnavn",
      road_reference = "Vegreferanse",
      adt_ref = "ÅDT\nreferanseår",
      adt = "ÅDT\nnyeste år",
      year_aadt = "År\n(nyeste)"
    ) %>%
    flextable::align(i = 1, j = c(3, 4, 5), align = "center", part = "header") %>%
    flextable::align(j = c(5), align = "center", part = "body") %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    width(j = 1, 2) |>
    width(j = 2, 1.2)
    #autofit() %>%
    # padding(
    #   j = 1,
    #   padding.left = 20,
    #   part = "all"
    # ) |>
    # height_all(height = .1)
}

create_point_table_trd_2 <- function(all_point_info_df) {

  all_point_info_df %>%
    select(
      name,
      station_type_short,
      road_reference,
      adt_ref,
      adt,
      year_aadt
    ) %>%
    flextable() %>%
    set_header_labels(
      name = "Navn",
      station_type_short = "Type",
      road_reference = "Vegreferanse",
      adt_ref = "ÅDT\nreferanseår",
      adt = "ÅDT\nnyeste år",
      year_aadt = "År\n(nyeste)"
    ) %>%
    flextable::align(i = 1, j = c(4, 5), align = "center", part = "header") %>%
    flextable::align(j = c(6), align = "center", part = "body") %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    width(j = 1, 2) |>
    width(j = 2, 0.4) |>
    width(j = 3, 1.3)
  #autofit() %>%
  # padding(
  #   j = 1,
  #   padding.left = 20,
  #   part = "all"
  # ) |>
  # height_all(height = .1)
}

create_point_table_trd <- function(all_point_info_df) {

  all_point_info_df %>%
    select(
      name,
      station_type_short,
      road_reference,
      adt,
      year_aadt
    ) %>%
    flextable() %>%
    set_header_labels(
      name = "Navn",
      station_type_short = "Type",
      road_reference = "Vegreferanse",
      adt = "ÅDT",
      year_aadt = "År"
    ) %>%
    flextable::align(i = 1, j = c(4, 5), align = "center", part = "header") %>%
    flextable::align(j = c(5), align = "center", part = "body") %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    autofit() %>%
    padding(
      j = 1,
      padding.left = 20,
      part = "all"
    ) |>
    height_all(height = .1)
}

table_bike_trps_with_sdt <- function(chosen_area_name) {

  bike_trp_info |>
    dplyr::filter(
      area_name == chosen_area_name
    ) |>
    dplyr::mutate(
      name = stringr::str_trunc(name, 30, "right")
    ) |>
    dplyr::select(
      name,
      road_category_and_number,
      municipality_name,
      year,
      WINTER,
      SPRING,
      SUMMER,
      FALL
    ) |>
    dplyr::arrange(
      road_category_and_number
    ) |>
    flextable() |>
    colformat_double(j = 4, big.mark = "", digits = 0) |>
    set_header_labels(
      name = "Registreringspunkt",
      road_category_and_number = "Veg",
      municipality_name = "Kommune",
      year = "År",
      WINTER = "Vinter",
      SPRING = "Vår",
      SUMMER = "Sommer",
      FALL = "Høst"
    ) |>
    flextable::align(j = 4, align = "center", part = "all") |>
    bold(part = "header") |>
    font(fontname = "Lucida Sans Unicode", part = "all")  |>
    fontsize(size = 7, part = "all") |>
    bg(bg = "#ED9300", part = "header") |>
    border_remove() |>
    hline_top(part = "header", border = borderline) |>
    hline_bottom(part = "all", border = borderline) |>
    autofit()
}

# For border index
create_monthly_index_table <- function(index_df) {
  
  last_month <- max(index_df$month)
  this_year <- max(index_df$year)
  
  index_df |>
    dplyr::arrange(
      length_range, 
      month
    ) |> 
    dplyr::select(
      month_name, 
      year, 
      length_range, 
      index_p, 
      standard_error, 
      standard_deviation, 
      no_points
    ) |>
    flextable::flextable() |>
    flextable::colformat_int(j = 2, big.mark = "") |>
    flextable::colformat_double(j = 4:6, digits = 1) |>
    flextable::set_header_labels(
      month_name = "Måned", year = "År", 
      length_range = "Kjøretøy-\nklasse",
      index_p = "Endring i\ntrafikk-\nmengde\n(%)",
      standard_error = "Standard-\nfeil\n(%)",
      standard_deviation = "Standard-\navvik\n(%)",
      no_points = "Antall\npunkt"
    ) |>
    flextable::align(j = c(2, 3, 7), align = "center", part = "all") |>
    flextable::align(j = 4:6, align = "center", part = "header") |>
    bold(part = "header") |>
    bg(bg = "#ED9300", part = "header") |>
    border_remove() |>
    hline_top(part = "header", border = borderline) |>
    hline_bottom(part = "all", border = borderline) |>
    hline(
      i = c(last_month, 2 * last_month), 
      part = "body",
      border = fp_border(color = "#dadada", width = 1)
    ) |> 
    height_all(height = .2) |>
    width(j = 3, width = .8) |> 
    fix_border_issues()
}

# Maps ----
create_point_adt_map <- function(all_point_info_df, legend_title = "\u00c5DT") {

  palett_adt <-
    leaflet::colorNumeric(
      palette = "Greens",
      domain = NULL
    )

  point_adt_map <-
    all_point_info_df |>
    leaflet::leaflet(
      options =
        leafletOptions(
          crs = nvdb_crs,
          zoomControl = F
        )
    ) |>
    leaflet::addTiles(
      urlTemplate = nvdb_map_url,
      attribution = nvdb_map_attribution
    ) |>
    leaflet::addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = 6,
      stroke = T,
      weight = 2,
      color = "#444f55",
      opacity = 0.8,
      fill = T,
      fillColor = ~palett_adt(adt),
      fillOpacity = 0.8
    ) |>
    leaflet::addLegend(
      #"bottomleft",
      "bottomright",
      pal = palett_adt,
      values = ~adt,
      title = legend_title,
      opacity = 0.6,
      labFormat = leaflet::labelFormat(big.mark = " ")
    )

  return(point_adt_map)
}


create_point_adt_map_with_labels <- function(all_point_info_df) {

  palett_adt <-
    colorNumeric(
      palette = "Greens",
      domain = NULL
    )

  point_adt_map <-
    all_point_info_df %>%
    leaflet(
      width = "100%",
      height = 700,
      options =
        leafletOptions(
          crs = nvdb_crs,
          zoomControl = F
        )
    ) %>%
    addTiles(
      urlTemplate = nvdb_map_url,
      attribution = nvdb_map_attribution
    ) %>%
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = 6,
      stroke = T,
      weight = 2,
      color = "#444f55",
      opacity = 0.8,
      fill = T,
      fillColor = ~palett_adt(adt),
      fillOpacity = 0.8,
      label = ~label_text
    ) %>%
    addLegend(
      #"bottomleft",
      "bottomright",
      pal = palett_adt,
      values = ~adt,
      title = "ADT",
      opacity = 0.7,
      labFormat = labelFormat(big.mark = " ")
    )

  return(point_adt_map)
}

create_point_adt_map_trondheim <- function(all_point_info_df) {

  palett_stasjonstype <-
    leaflet::colorFactor(
      palette = c("#db3b99", "#444f55"),
      domain = c("Bomstasjon", "Trafikkregistrering")
    )

  palett_adt <-
    leaflet::colorNumeric(palette = "Greens",
                 domain = NULL)

  point_adt_map <-
    all_point_info_df |>
    leaflet::leaflet(
      width = "100%",
      #height = 700,
      options =
        leafletOptions(
          crs = nvdb_crs,
          zoomControl = F
        )
    ) |>
    leaflet::addTiles(
      urlTemplate = nvdb_map_url,
      attribution = nvdb_map_attribution
    ) |>
    leaflet::addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = 6,
      stroke = T,
      weight = 2,
      color = ~palett_stasjonstype(station_type),
      opacity = 0.8,
      fill = T,
      fillColor = ~palett_adt(adt),
      fillOpacity = 0.8
    ) |>
    leaflet::addLegend(
      position = "bottomleft",
      pal = palett_stasjonstype,
      values = ~station_type,
      title = "Stasjonstype",
      opacity = 0.7
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal = palett_adt,
      values = ~adt,
      bins = 5,
      title = "ÅDT",
      opacity = 0.7,
      labFormat = labelFormat(big.mark = " ")
    )

  return(point_adt_map)
}


create_point_adt_map_review <- function(all_point_info_df) {

  palett_stasjonstype <-
    colorFactor(
      palette = c("#31a354", "#db3b99", "#878787"),
      levels = c("Er med", "Tas med", "Utelates")
      )

  palett_adt <-
    colorNumeric(palette = "Purples",
                 domain = NULL)

  point_adt_map <- all_point_info_df %>%
    leaflet(width = "100%",
            height = 700,
            options = leafletOptions(crs = nvdb_crs,
                                     zoomControl = F)) %>%
    addTiles(urlTemplate = nvdb_map_url,
             attribution = nvdb_map_attribution) %>%
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = 6,
      stroke = T,
      weight = 3,
      color = ~palett_stasjonstype(status),
      opacity = 0.8,
      fill = T,
      fillColor = ~palett_adt(adt),
      fillOpacity = 0.8,
      label = ~label_text
    ) %>%
    addLegend("bottomright",
              pal = palett_stasjonstype,
              values = ~status,
              title = "I byindeks",
              opacity = 0.7) %>%
    addLegend("bottomright",
              pal = palett_adt,
              values = ~adt,
              title = "ADT",
              opacity = 0.7,
              labFormat = labelFormat(big.mark = " "))

  return(point_adt_map)
}


map_trp_with_category <- function(all_point_info_df) {

  palette_category <-
    leaflet::colorFactor(
      palette = c("#db3b99", "#444f55"),
      domain = c("Ja", "Nei"))

  map <-
    all_point_info_df |>
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
    addCircleMarkers(
      radius = 6,
      stroke = T,
      weight = 2,
      color = ~palette_category(city_index),
      opacity = 0.8,
      fill = T,
      fillColor = ~palette_category(city_index),
      fillOpacity = 0.8,
      label = ~label_text,
    ) |>
    addLegend(
      "bottomright",
      pal = palette_category,
      values = ~city_index,
      title = "Med i indeks",
      opacity = 0.7
    )

  return(map)
}

create_pointindex_map <- function(all_point_info_df) {

  # Create a red-green scale based on index values
  negative_value <-
    round(abs(min(all_point_info_df$index, 0, na.rm = T)), digits = 0) + 1
  positive_value <-
    round(max(all_point_info_df$index, 0, na.rm = T), digits = 0) + 2

  # If even the max value is negative
  if(positive_value <= 0){ positive_value <- 1}

  rc1 <-
    colorRampPalette(colors = c("lightgreen", "green"), space = "Lab")(negative_value)

  ## Make vector of colors for values larger than 0 (180 colors)
  rc2 <-
    colorRampPalette(colors = c("green", "darkgreen"), space = "Lab")(positive_value)

  ## Combine the two color palettes
  rampcols <- c(rc1, rc2)

  palett_index <- leaflet::colorNumeric(palette = rampcols, domain = NULL)

  pointindex_map <-
    all_point_info_df |>
    leaflet(
      width = "100%",
      height = 700,
      options =
        leafletOptions(
          crs = nvdb_crs,
          zoomControl = F
          )
    ) |>
    addTiles(
      urlTemplate = nvdb_map_url,
      attribution = nvdb_map_attribution
    ) |>
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = 6,
      stroke = T,
      weight = 2,
      color = "#444f55",
      opacity = 0.8,
      fill = T,
      fillColor = ~palett_index(index),
      fillOpacity = 0.8,
      label = ~label_text
    ) |>
    addLegend(
      "bottomright",
      pal = palett_index,
      values = ~index,
      title = "Indeks",
      opacity = 0.7,
      labFormat = labelFormat(big.mark = " ")
    )

  return(pointindex_map)
}

map_pointindex <- function(all_point_info_df, index_limit = 10) {

  # Scale not dependent on actual values, but always red for negative, and green for positive
  # Always symmetric
  index_limit_text <- c(
    paste0("< -", index_limit, " %"),
    paste0("[-", index_limit, ", ", index_limit, "] %"),
    paste0("> ", index_limit, " %")
  )

  palett_index_factor <-
    leaflet::colorFactor(
      palette = c("red", "green", "purple"),
      levels = index_limit_text
    )

  pointindex_map <-
    all_point_info_df |>
    dplyr::mutate(
      index_factor =
        dplyr::case_when(
          index < -index_limit ~ index_limit_text[1],
          index <  index_limit ~ index_limit_text[2],
          index >  index_limit ~ index_limit_text[3]
        ) |>
        base::factor(
          levels = index_limit_text
        )
    ) |>
    leaflet(
      width = "100%",
      height = 700,
      options =
        leafletOptions(
          crs = nvdb_crs,
          zoomControl = F
        )
    ) |>
    addTiles(
      urlTemplate = nvdb_map_url,
      attribution = nvdb_map_attribution
    ) |>
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = 6,
      stroke = T,
      weight = 2,
      color = "#444f55",
      opacity = 0.8,
      fill = T,
      fillColor = ~palett_index_factor(index_factor),
      fillOpacity = 0.8,
      label = ~label_text
    ) |>
    addLegend(
      "bottomright",
      pal = palett_index_factor,
      values = ~index_factor,
      title = "Indeks",
      opacity = 0.6
    )

  return(pointindex_map)
}

map_pointindex_and_events <- function(this_df, index_limit = 8) {

  # Create a scale based on index values
  # negative_value <-
  #   round(abs(min(this_df$index_total_p, na.rm = T)), digits = 0) + 1
  # positive_value <-
  #   round(max(this_df$index_total_p, na.rm = T), digits = 0) + 1

  # If even the max value is negative
  # if(positive_value <= 0) positive_value <- 1

  # rc1 <-
  #   colorRampPalette(
  #     colors = c("lightgreen", "green"),
  #     space = "Lab")(negative_value)

  ## Make vector of colors for values larger than 0 (180 colors)
  # rc2 <-
  #   colorRampPalette(
  #     colors = c("green", "darkgreen"),
  #     space = "Lab")(positive_value)

  ## Combine the two color palettes
  # rampcols <- c(rc1, rc2)
  #
  # palett_index <-
  #   leaflet::colorNumeric(
  #     palette = rampcols,
  #     domain = NULL
  #   )
  #
  # Discrete scale
  index_limit_text <- c(
    paste0("< -", index_limit, " %"),
    paste0("[-", index_limit, ", ", index_limit, "] %"),
    paste0("> ", index_limit, " %")
  )

  palett_index_factor <-
    leaflet::colorFactor(
      palette = c("red", "green", "purple"),
      levels = index_limit_text
    )

  this_map <-
    this_df |>
    dplyr::rename(
      index = index_total_p
    ) |>
    dplyr::mutate(
      index_factor =
        dplyr::case_when(
          index < -index_limit ~ index_limit_text[1],
          index <  index_limit ~ index_limit_text[2],
          index >  index_limit ~ index_limit_text[3]
        ) |>
        base::factor(levels = index_limit_text)
    ) |>
    leaflet(
      width = "100%",
      height = 700,
      options =
        leafletOptions(
          crs = nvdb_crs,
          zoomControl = F
        )
    ) |>
    addTiles(
      urlTemplate = nvdb_map_url,
      attribution = nvdb_map_attribution
    ) |>
    addPolylines(
      label = ~text,
      stroke = T,
      opacity = 1,
      #color = ~palett_index(index_total_p),
      color = ~palett_index_factor(index_factor),
      highlightOptions = highlightOptions(
        bringToFront = TRUE,
        sendToBack = FALSE,
        color = "#636363",
        opacity = 0.6
      )
    ) |>
    # addLegend(
    #   "bottomright",
    #   pal = palett_index,
    #   values = ~index_total_p,
    #   title = "Indeks",
    #   opacity = 0.7,
    #   labFormat = labelFormat(big.mark = " ")
    # ) |>
    addLegend(
      "bottomright",
      pal = palett_index_factor,
      values = ~index_factor,
      title = "Indeks",
      opacity = 0.6,
      labFormat = labelFormat(big.mark = " ")
    )

  return(this_map)
}

create_index_limit_text <- function(index_limit_dbl) {

 # Discrete scale for index
  index_limit_text <- c(
    paste0("< -", index_limit_dbl, " %"),
    paste0("[-", index_limit_dbl, ", ", index_limit_dbl, "] %"),
    paste0("> ", index_limit_dbl, " %")
  )

}


add_index_factor_column <- function(index_df, index_limit_text_chr, index_limit_dbl) {

  # index_df must have a column called index

  index_df_with_index_limit_factor <-
    index_df |> 
    dplyr::mutate(
      index_factor =
        dplyr::case_when(
          index < -index_limit_dbl ~ index_limit_text_chr[1],
          index <  index_limit_dbl ~ index_limit_text_chr[2],
          index >  index_limit_dbl ~ index_limit_text_chr[3]
        ) |>
        base::factor(levels = index_limit_text_chr)
    )

}


map_trp_index_and_events <- function(link_df, event_df, trps_without_links_df, index_limit = 8) {

  # Test
  # link_df <- links_for_map
  # event_df <- events_this_month
  # trps_without_links_df <- trps_without_links

  # Discrete scale for index
  index_limit_text <- create_index_limit_text(index_limit)

  palett_index_factor <-
    leaflet::colorFactor(
      palette = c("red", "#1D7721", "purple"),
      levels = index_limit_text
    )
  
# Need to split events by geometry type  
events_this_month_points <- dplyr::filter(event_df, sf::st_geometry_type(geografi) == "POINT")
events_this_month_lines  <- dplyr::filter(event_df, sf::st_geometry_type(geografi) != "POINT")

  this_map_base <-
    link_df |>
    dplyr::rename(
      index = index_total_p
    ) |>
    add_index_factor_column(index_limit_text, index_limit) |> 
    leaflet(
      width = "100%",
      height = 800,
      options =
        leafletOptions(
          crs = nvdb_crs,
          zoomControl = F
        )
    ) |>
    addTiles(
      urlTemplate = nvdb_map_url,
      attribution = nvdb_map_attribution
    ) |>
    addPolylines(
      group = "index",
      label = ~info_text,
      stroke = T,
      opacity = 1,
      color = ~palett_index_factor(index_factor),
      highlightOptions = highlightOptions(
        bringToFront = TRUE,
        sendToBack = FALSE,
        color = "#636363",
        opacity = 0.6
      )
    ) |>
    addPolylines(
      data = events_this_month_lines,
      group = "events",
      label = ~info_text,
      stroke = T,
      opacity = 1,
      highlightOptions = highlightOptions(
        bringToFront = TRUE,
        sendToBack = FALSE,
        color = "#636363",
        opacity = 0.6
      )
    ) |> 
    addMarkers(
      data = events_this_month_points,
      group = "events",
      label = ~info_text
    ) 
  
  if(nrow(trps_without_links_df) > 0) {

    this_map <-
      this_map_base |> 
      addCircleMarkers(
        data = trps_without_links_df |> add_index_factor_column(index_limit_text, index_limit),
        group = "index",
        label = ~info_text,
        radius = 6,
        stroke = T,
        weight = 2,
        color = "#444f55",
        opacity = 0.8,
        fill = T,
        fillColor = ~palett_index_factor(index_factor),
        fillOpacity = 0.8
      ) |> 
      addLayersControl(
        overlayGroups = c("index", "events"),
        options = layersControlOptions(collapsed = FALSE)
      ) |> 
      addLegend(
        "bottomright",
        pal = palett_index_factor,
        values = ~index_factor,
        title = "Indeks",
        opacity = 0.6,
        labFormat = labelFormat(big.mark = " ")
      )
    
  }else{

    this_map <-
      this_map_base |> 
      addLayersControl(
        overlayGroups = c("index", "events"),
        options = layersControlOptions(collapsed = FALSE)
      ) |> 
      addLegend(
        "bottomright",
        pal = palett_index_factor,
        values = ~index_factor,
        title = "Indeks",
        opacity = 0.6,
        labFormat = labelFormat(big.mark = " ")
      )
  }

  return(this_map)
}


map_links_for_index_check <- function(base_year_dbl, calc_year_dbl, index_month_dbl, trp_index_df, index_limit_dbl = 4) {

  # Test
  trp_index_df <- point_index_new_prepared_1
  index_limit_dbl <- 5
  base_year_dbl <- index_year - 1
  calc_year_dbl <- index_year
  index_month_dbl <- latest_published_month + 1

  links_with_trp_index <- join_links_and_trp_index(links_with_trp, trp_index_df)
  trps_without_links <- find_trps_without_links(trp_index_df)

  links_for_map <- 
      links_in_area |> 
      dplyr::filter(link_id %in% links_with_trp$link_id) |> 
      dplyr::left_join(links_with_trp_index, by = "link_id") |> 
      dplyr::select(link_id, info_text, index_total_p) |> 
      dplyr::mutate(
        # info_text = lapply(info_text, htmltools::HTML)
        info_text = purrr::map(info_text, htmltools::HTML)
      )

  events_b <- get_events_in_year_month(events, base_year_dbl, index_month_dbl)
  events_c <- get_events_in_year_month(events, calc_year_dbl, index_month_dbl)

  # Need to keep only distinct events and have them in one layer
  events_this_month <-
    dplyr::bind_rows(
      events_b |> dplyr::select(-interval),
      events_c |> dplyr::select(-interval)
    ) |> 
    dplyr::distinct() |> 
    dplyr::select(info_text) |> 
    dplyr::mutate(
      # info_text = lapply(info_text, htmltools::HTML)
      info_text = purrr::map(info_text, htmltools::HTML)
    )

  map <- map_trp_index_and_events(links_for_map, events_this_month, trps_without_links, index_limit_dbl)

  return(map)

}


map_links_with_trp <- function(link_df) {

  map <-
    link_df |>
    leaflet(
      width = "100%",
      height = 700,
      options =
        leafletOptions(
          crs = nvdb_crs,
          zoomControl = F
        )
    ) |>
    addTiles(
      urlTemplate = nvdb_map_url,
      attribution = nvdb_map_attribution
    ) |>
    addPolylines(
      data = link_df,
      label = ~label_text,
      opacity = 0.8,
      highlightOptions = highlightOptions(
        bringToFront = TRUE,
        sendToBack = FALSE,
        color = "purple",
        opacity = 0.6
      )
    )

  return(map)
}


map_links_with_trp_in_index <- function(link_df) {

  palett_in_index <-
    colorFactor(
      palette = c("#db3b99", "#444f55"),
      domain = c("Ja", "Nei")
    )

  map <-
    link_df |>
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
      data = link_df,
      label = ~label_text,
      opacity = 0.8,
      color = ~palett_in_index(city_index),
      highlightOptions = highlightOptions(
        bringToFront = TRUE,
        sendToBack = FALSE,
        color = "purple",
        opacity = 0.6
      )
    ) |>
    addLegend(
      "bottomright",
      pal = palett_in_index,
      values = ~city_index,
      title = "Med i byindeks",
      opacity = 0.7
    )

  return(map)
}


# Area index tables ----
create_city_index_table <- function(city_info) {

  city_table <- city_info %>%
    dplyr::select(year_from_to, index_p, ci_start, ci_end) %>%
    flextable::flextable() %>%
    colformat_double(
      j = c("index_p", "ci_start", "ci_end"),
      digits = 1
    ) %>%
    set_header_labels(year_from_to = "Periode",
                      index_p = "Endring i \n trafikkmengde (%)",
                      ci_start = "95 % konfidensintervall") %>%
    merge_at(i = 1, j = 3:4, part = "header") %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    autofit() %>%
    height_all(height = .2) %>%
    set_caption("Estimert endring i trafikkmengde for området.",
                autonum = table_numbers,
                style = "Tabelltekst")

  return(city_table)
}

create_city_index_table_sd <- function(city_info) {

  n_years <-
    city_info$year |>
    base::unique() |>
    base::length()

  line_after_single_years <-
    officer::fp_border(
      color = "#dadada",
      style = "solid",
      width = 1
    )

  city_table <-
    city_info %>%
    dplyr::select(
      year_from_to,
      period,
      n_trp,
      index_p,
      #standard_deviation,
      standard_error
    ) %>%
    flextable::flextable() %>%
    colformat_double(
      j = c("index_p", #"standard_deviation",
            "standard_error"),
      digits = 1
    ) %>%
    set_header_labels(
      year_from_to = "Periode",
      period = "",
      n_trp = "Antall punkt",
      index_p = "Endring i \n trafikkmengde \n (%)",
      #standard_deviation = "Standardavvik \n (%)",
      standard_error = "Standardfeil \n (%)"
    ) %>%
    flextable::align(
      j = c("period", "n_trp", "index_p", #"standard_deviation",
            "standard_error"),
      align = "center", part = "all"
    ) %>%
    #padding(
    #  j = c("index_p", "standard_deviation", "standard_error"),
    #  padding.right = 25, part = "body"
    #) %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    hline(i = n_years, border = line_after_single_years, part = "body") |>
    autofit() %>%
    height_all(height = .2)

  return(city_table)
}

create_city_index_table_ci <- function(city_info) {

  n_years <-
    city_info$year |>
    base::unique() |>
    base::length()

  line_after_single_years <-
    officer::fp_border(
      color = "#dadada",
      style = "solid",
      width = 1
    )

  city_table <-
    city_info |>
    dplyr::select(
      year_from_to,
      period,
      n_trp,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(
      period_text = paste0(year_from_to, " ", period),
      ci_lower_text = as.character(ci_lower) |> stringr::str_replace("\\.", ","),
      ci_upper_text = as.character(ci_upper) |> stringr::str_replace("\\.", ","),
      ci = paste0("(", ci_lower_text, ", ", ci_upper_text, ")")
    ) |>
    dplyr::select(
      period_text,
      n_trp,
      index_p,
      ci
    ) |>
    flextable::flextable() |>
    colformat_double(
      j = c("index_p"),
      digits = 1
    ) |>
    set_header_labels(
      period_text = "Periode",
      n_trp = "Antall\npunkt",
      index_p = "Byindeks\n(endring i\ntrafikkmengde)\n(%)",
      ci = "Konfidens-\nintervall\n(%-poeng)"
    ) |>
    flextable::align(
      #j = c("n_trp", "ci"),
      align = "center", part = "all"
    ) |>
    # align(
    #   j = c("index_p"),
    #   align = "left", part = "header"
    # ) |>
    bold(part = "header") |>
    bg(bg = "#ED9300", part = "header") |>
    border_remove() |>
    hline_top(part = "header", border = borderline) |>
    hline_bottom(part = "all", border = borderline) |>
    hline(i = n_years, border = line_after_single_years, part = "body") |>
    autofit() |>
    height_all(height = .2)

  return(city_table)
}

create_monthly_city_index_table <- function(city_monthly) {

  monthly_table <- city_monthly %>%
    dplyr::filter(!is.na(index_p)) %>%
    dplyr::select(year, month_name, index_p) %>%
    flextable::flextable() %>%
    colformat_int(j = "year", big.mark = "") %>%
    colformat_double(j = c("index_p"), digits = 1) %>%
    set_header_labels(year = "År",
                      month_name = "Måned",
                      index_p = "Endring i \n trafikkmengde (%)") %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    flextable::align(j = 3, align = "center", part = "header") %>%
    autofit() %>%
    height_all(height = .2) %>%
    set_caption("Estimert endring i trafikkmengde per måned.",
                autonum = table_numbers,
                style = "Tabelltekst")

  return(monthly_table)
}

create_city_36_index_table <- function(city_36_month) {

  city_table <- city_36_month %>%
    dplyr::select(month_name, year, index_p) %>%
    flextable::flextable() %>%
    colformat_double(j = c("index_p"), digits = 1) %>%
    set_header_labels(month_name = "Treårsperiodens slutt",
                      index_p = "Endring i \n trafikkmengde (%)") %>%
    merge_at(i = 1, j = 1:2, part = "header") %>%
    colformat_double(j = c("year"), big.mark = "", digits = 0) %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    flextable::align(j = 2, align = "left") %>%
    autofit() %>%
    height_all(height = .2) %>%
    set_caption("Estimert endring i trafikkmengde for siste tre år.",
                autonum = table_numbers,
                style = "Tabelltekst")

  return(city_table)
}

create_city_mdt_36_index_table <- function(city_36_month) {

  city_table <-
    city_36_month %>%
    dplyr::select(
      index_period,
      n_trp,
      index_p,
      sd_sample_p,
      standard_error_p
    ) %>%
    flextable::flextable() %>%
    colformat_double(
      j = c("index_p", "sd_sample_p", "standard_error_p"),
      digits = 1
    ) %>%
    set_header_labels(
      index_period = "Sammenligningsperiode",
      index_p = "Byindeks\n(endring i\ntrafikkmengde\n(%)",
      n_trp = "Antall\npunkt",
      sd_sample_p = "Standardavvik\n(prosentpoeng)",
      standard_error_p = "Standardfeil\n(prosentpoeng)"
    ) %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    flextable::align(align = "center", part = "all") |>
    autofit() %>%
    height_all(height = .1)

  return(city_table)
}

create_city_mdt_36_index_table_ci <- function(city_36_month) {

  city_table <-
    city_36_month |>
    dplyr::select(
      index_period,
      n_trp,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(
      ci_lower_text = as.character(ci_lower) |> stringr::str_replace("\\.", ","),
      ci_upper_text = as.character(ci_upper) |> stringr::str_replace("\\.", ","),
      ci = paste0("(", ci_lower_text, ", ", ci_upper_text, ")")
    ) |>
    dplyr::select(
      index_period,
      n_trp,
      index_p,
      ci
    ) |>
    flextable::flextable() |>
    colformat_double(
      j = c("index_p"),
      digits = 1
    ) |>
    set_header_labels(
      index_period = "Sammenligningsperiode",
      index_p = "Byindeks\n(endring i\ntrafikkmengde)\n(%)",
      n_trp = "Antall\npunkt",
      ci = "Konfidens-\nintervall\n(%-poeng)"
    ) |>
    bold(part = "header") |>
    bg(bg = "#ED9300", part = "header") |>
    border_remove() |>
    hline_top(part = "header", border = borderline) |>
    hline_bottom(part = "all", border = borderline) |>
    flextable::align(align = "center", part = "all") |>
    autofit() |>
    height_all(height = .1)

  return(city_table)
}

road_category_names <- data.frame(
  road_category = c("E", "R", "F", "K"),
  road_category_name = c("Europaveg", "Riksveg", "Fylkesveg", "Kommunalveg"))

create_corridor_index_table <- function(corridor_index_all_years) {

  corridor_table <- corridor_index_all_years %>%
    dplyr::select(year, index_total, index_short, index_long) %>%
    flextable::flextable() %>%
    colformat_num(j = 2:4, digits = 1) %>%
    set_header_labels(year = "Periode",
                      index_total = "Endring i alle \n (%)",
                      index_short = "Endring i lette \n (%)",
                      index_long = "Endring i tunge \n (%)") %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    flextable::align(align = "center", part = "header") %>%
    autofit() %>%
    height_all(height = .2) %>%
    set_caption("Estimert endring i trafikkmengde.",
                autonum = table_numbers,
                style = "Tabelltekst")

  return(corridor_table)
}

# For border index
create_monthly_index_lineplot <- function(index_df) {
  
  index_df |> 
    dplyr::filter(period == "month") |> 
    ggplot2::ggplot(
      aes(
        x = month_object, 
        y = index_p, 
        color = length_range
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_grid(rows = vars(year)) +
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.title.y = 
        element_text(
          margin = margin(t = 0, r = 15, b = 0, l = 0)
        ),
      axis.title.x = 
        element_text(
            margin = margin(t = 15, r = 0, b = 0, l = 0)),
            panel.grid.minor.x = element_blank()
    ) +
    scale_x_date(
      breaks = scales::breaks_width("months"),
      labels = scales::label_date("%b")
    ) +
    scale_color_manual(
      values = c("alle" = "#008ec2",
                 "lette" = "#ed9300",
                 "tunge" = "#444f55"),
       name = "Kjøretøyklasse"
    ) +
    labs(
      x = NULL, y = "Endring i trafikkmengde (%) \n",
      caption = "Data: Statens vegvesen og fylkeskommunene"
    ) +
    ggtitle(
      "Estimert endring i trafikkmengde ved riksgrensen",
      subtitle = "Trafikkmengde per måned sammenlignet med foregående år"
    ) +
    theme(legend.position = "bottom")
}


# Historic indexes
table_historic_index <- function(index_df) {

  index_df |> 
    flextable::flextable() |>
    flextable::colformat_double(j = 2:base::ncol(index_df), digits = 1) |>
    flextable::set_header_labels(year = "År") |>
    flextable::align(j = 2:base::ncol(index_df), align = "right", part = "all") |>
    bold(part = "header") |>
    bg(bg = "#ED9300", part = "header") |>
    border_remove() |>
    hline_top(part = "header", border = borderline) |>
    hline_bottom(part = "all", border = borderline) |>
    height_all(height = .2) |>
    fix_border_issues() |>
    padding(padding.top = .3,
            padding.bottom = .3) |> 
    width(width = 0.5)
}


# Road length ----
read_road_length_csv <- function(road_csv) {

  readr::read_csv2(road_csv,
                   locale = readr::locale(
                     encoding = "latin1",
                     decimal_mark = ",",
                     grouping_mark = " ")) %>%
    left_join(road_category_names) %>%
    mutate(road_category = factor(road_category,
                                  levels = c("E", "R", "F", "K"))) %>%
    arrange(municipality_number, road_category)
}

create_municipality_road_length_table <- function(road_lengths) {

  road_lengths_table <- road_lengths %>%
    select(municipality_name, road_category_name, length_km) %>%
    flextable() %>%
    merge_v(j = "municipality_name", target = "municipality_name") %>%
    set_header_labels(municipality_name = "Kommune",
                      road_category_name = "Vegkategori",
                      length_km = "Lengde (km)") %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    fix_border_issues() %>%
    valign(j = 1, valign = "top") %>%
    autofit() %>%
    height_all(height = .2)

  return(road_lengths_table)
}

create_city_road_length_table <- function(road_lengths) {

  road_lengths_table <-
    road_lengths %>%
    select(road_category, road_category_name, length_km) %>%
    group_by(road_category, road_category_name) %>%
    summarise(length_km = sum(length_km)) %>%
    ungroup() %>%
    select(-road_category) %>%
    select(road_category_name, length_km) %>%
    flextable() %>%
    set_header_labels(
      road_category_name = "Vegkategori",
      length_km = "Lengde \n (km)"
    ) %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    #align(j = 1, align = "center") %>%
    autofit() %>%
    height_all(height = .2)

  return(road_lengths_table)
}


# Plots ----
create_city_monthly_index_plot <- function(city_monthly) {

  city_monthly %>%
    ggplot2::ggplot(aes(x = month_object, y = index_p)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_grid(rows = vars(year)) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.minor.x = element_blank(),
          #legend.position = "bottom"
          ) +
    scale_x_date(breaks = scales::breaks_width("months"),
                 labels = scales::label_date("%b")) +
    labs(x = NULL, y = "Endring i trafikkmengde (%) \n",
         caption = caption_credit) +
    ggtitle("Estimert endring i trafikkmengde per måned",
            subtitle = "Trafikkmengde sammenlignet med samme måned året før")

}


#I figuren nedenfor er spredningen i punktindeksene illustrert. Den vertikale grønne streken viser byindeksens samlede verdi for hele perioden 2019 - `r latest_month` 2020.

#```{r point_graph, fig.width=6, fig.height=7, fig.cap="ÅDT og endring i trafikkmengde per trafikkregistreringspunkt."}
# TODO: a plot per year
# all_point_info_index <- all_point_info %>%
#   dplyr::filter(!is.na(index))
#
# subtitle_here <- paste0("Fra 2019 til ", latest_month, " 2020")
#
# ggplot2::ggplot() +
#   geom_point(data = all_point_info_index,
#              aes(x = reorder(name, index), y = index, size = adt),
#              color = "#ED9300", alpha = 0.6) +
#   geom_hline(yintercept = index_all_years, color = "#58B02C") +
#   geom_hline(yintercept = 0, color = "#000000") +
#   xlab("") +
#   ylab("Endring i trafikkmengde (%) \n") +
#   ggtitle(label = "ÅDT og endring i trafikkmengde",
#           subtitle = subtitle_here) +
#   coord_flip() +
#   scale_size(name = "ÅDT") +
#   geom_rect(aes(xmin = -Inf, xmax = Inf,
#                 ymin = index_all_years_ci_lower,
#                 ymax = index_all_years_ci_upper),
#             alpha = 0.1, fill = "#008EC2") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         axis.text.y = element_text(size = 7)) +
#   labs(caption = caption_credit)
# TODO: Add explaining labels with ggforce?
#```

#city_36_month_df <- indexes_official_bergen |> dplyr::filter(window == "12_months",year < 2025)
visualize_city_36_mdt_index <- function(city_36_month_df, caption_text, title_text, sub_text) {

  x_breaks_labels <-
    city_36_month_df |>
    dplyr::filter(
      month_n %in% c(4, 8, 12)
    ) |>
    dplyr::mutate(
      x_label = base::paste0(lubridate::month(month_object, label = TRUE), " ", stringr::str_sub(year, 3, 4))
    )

  x_breaks <- x_breaks_labels |> purrr::pluck("month_object")
  x_labels <- x_breaks_labels |> purrr::pluck("x_label")

  city_36_month_df |>
    ggplot2::ggplot(aes(x = month_object, y = index_p)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "#58b02c",
      linewidth = 0.8,
      alpha = 0.3
    ) +
    ggplot2::geom_ribbon(
      aes(
        ymin = ci_lower,
        ymax = ci_upper
      ),
      linetype = 2,
      alpha = 0.1,
      fill = "#444f55"
    ) +
    ggplot2::geom_line(color = "#ED9300") +
    ggplot2::geom_point(color = "#ED9300") +
    theme_light() +
    theme(
      axis.text.x = element_text(vjust = 0.5, angle = 90),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        )
    ) +
    ggplot2::scale_x_date(
      breaks = x_breaks,
      labels = x_labels
    ) +
    labs(
      x = NULL, y = "Endring i trafikkmengde (%)",
      caption = caption_text) +
    ggtitle(
      title_text,
      subtitle = sub_text
    )
}


visualize_rolling_cmdt_index <- function(rolling_cmdt_df, caption_text, title_text, sub_text) {

  x_breaks_labels <-
    rolling_cmdt_df |>
    dplyr::filter(
      stringr::str_detect(x_label, pattern = "apr|aug|des")
    )

  x_breaks <- x_breaks_labels |> purrr::pluck("universal_year_period_id")
  x_labels <- x_breaks_labels |> purrr::pluck("x_label")

  rolling_cmdt_df |>
    ggplot2::ggplot(aes(x = universal_year_period_id, y = index_p)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "#58b02c",
      linewidth = 0.8,
      alpha = 0.3
    ) +
    ggplot2::geom_ribbon(
      aes(
        ymin = ci_lower,
        ymax = ci_upper
      ),
      linetype = 2,
      alpha = 0.1,
      fill = "#444f55"
    ) +
    ggplot2::geom_line(color = "#ED9300") +
    ggplot2::geom_point(color = "#ED9300") +
    theme_light() +
    theme(
      axis.text.x = element_text(vjust = 0.5, angle = 90),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        )
    ) +
    ggplot2::scale_x_continuous(
      name = NULL,
      breaks = x_breaks,
      labels = x_labels
    ) +
    labs(
      x = NULL, y = "Endring i trafikkmengde (%)",
      caption = caption_text) +
    ggtitle(
      title_text,
      subtitle = sub_text
    )
}


visualize_rolling_cmdt_indices_12_36 <- function(rolling_cmdt_df, title_text, sub_text) {

  x_breaks_labels <-
    rolling_cmdt_df |>
    dplyr::filter(
      stringr::str_detect(x_label, pattern = "apr|aug|des")
    )

  x_breaks <- x_breaks_labels |> purrr::pluck("universal_year_period_id")
  x_labels <- x_breaks_labels |> purrr::pluck("x_label")

  rolling_cmdt_df |>
    ggplot2::ggplot(aes(x = universal_year_period_id, y = index_p, color = window_years, group = window_years)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "#58b02c",
      linewidth = 0.8,
      alpha = 0.3
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    facet_grid(rows = vars(area)) +
    ggplot2::scale_color_manual(
      values = c(
        "one" = "#008ec2",
        "three" = "#ed9300"
      ),
      labels = c(
        "1 år",
        "3 år"
      ),
      name = "Glidende periode"
    ) +
    theme_light() +
    theme(
      axis.text.x = element_text(vjust = 0.5, angle = 90),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        ),
      legend.position = "bottom"
    ) +
    ggplot2::scale_x_continuous(
      name = NULL,
      breaks = x_breaks,
      labels = x_labels
    ) +
    labs(
      x = NULL, y = "Endring i trafikkmengde (%)"
      # caption = caption_text
    ) +
    ggtitle(
      title_text,
      subtitle = sub_text
    )
}


visualize_rolling_indices <- function(rolling_indices_df, caption_text, title_text, sub_text) {

    # Test
    # rolling_indices_df <- rolling_indices_long
    # caption_text <- caption_credit
    # title_text <- "TEST"
    # sub_text <- "test"
  
  x_breaks_labels <-
    rolling_indices_df |>
    dplyr::filter(
      month_n %in% c(4, 8, 12)
    ) |>
    dplyr::mutate(
      x_label = base::paste0(lubridate::month(month_object, label = TRUE), " ", stringr::str_sub(year, 3, 4))
    )

  x_breaks <- x_breaks_labels |> purrr::pluck("month_object")
  x_labels <- x_breaks_labels |> purrr::pluck("x_label")
  
  rolling_indices_df |>
    ggplot2::ggplot(aes(x = month_object, y = index_p, color = window, linetype = window)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "#58b02c",
      linewidth = 0.8,
      alpha = 0.3
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    scale_color_manual(
      values = c(
        "12_months" = "#008ec2",
        "24_months" = "#444f55",
        "36_months" = "#ed9300"
      ),
      breaks = c(
        "12_months",
        "24_months",
        "36_months"
      ),
      labels = c(
        "Siste 1 år",
        "Siste 2 år",
        "Siste 3 år"
      ),
      name = "Gjennomsnittsperiode"
    ) +
    scale_linetype_manual(
      values = c(
        "12_months" = "dotted",
        "24_months" = "dotted",
        "36_months" = "solid"
      ),
      breaks = NULL,
      labels = NULL,
      name = ""
    ) +
    theme_light() +
    theme(
      axis.text.x = element_text(vjust = 0.5, angle = 90),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        ),
      legend.position = "bottom"
    ) +
    scale_x_date(
      breaks = x_breaks,
      labels = x_labels
    ) +
    labs(
      x = NULL, y = "Endring i trafikkmengde (%)",
      caption = caption_text
    ) +
    ggtitle(title_text, subtitle = sub_text)
}

visualize_index_examples <- function(index_df, window_length, title_text, sub_text) {

    # window_length must be "12_months" or "24_months" or "36_months"

    index_df |>
      dplyr::filter(
        window == window_length
      ) |>
      ggplot2::ggplot(aes(x = month_object, y = index_p, color = version)) +
      ggplot2::geom_hline(
        yintercept = 0,
        color = "#58b02c",
        linewidth = 0.8,
        alpha = 0.3
      ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      scale_color_manual(
        values = c(
          "old" = "#008ec2",
          "official" = "#444f55",
          "new" = "#ed9300"
        ),
        breaks = c(
          "old",
          "official",
          "new"
        ),
        labels = c(
          "Gammel",
          "Offisiell",
          "Ny"
        ),
        name = "Indeksversjon"
      ) +
      theme_light() +
      theme(
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        axis.title.y = element_text(
          margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(
          margin = margin(t = 15, r = 0, b = 0, l = 0)),
        panel.grid.minor.x = element_blank(),
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
        legend.position = "bottom"
      ) +
      scale_x_date(
        labels = scales::label_date("%b %Y"),
        date_breaks = "4 months"
      ) +
      ylim(
        min(index_df$index_p),
        max(index_df$index_p)
      ) +
      labs(
        x = NULL, y = "Endring i trafikkmengde (%)"
      ) +
      ggtitle(
        title_text,
        subtitle = sub_text
      )
  }

visualize_n_trp <- function(index_df, window_length, title_text, sub_text) {

    # window_length must be "12_months" or "24_months" or "36_months"

    index_df |>
      dplyr::filter(
        window == window_length,
        version %in% c("official", "new")
      ) |>
      ggplot2::ggplot(aes(x = month_object, y = n_trp, color = version)) +
      #ggplot2::geom_line() +
      ggplot2::geom_point() +
      scale_color_manual(
        values = c(
          "official" = "#444f55",
          "new" = "#ed9300"
        ),
        breaks = c(
          "official",
          "new"
        ),
        labels = c(
          "Offisiell",
          "Ny"
        ),
        name = "Indeksversjon"
      ) +
      theme_light() +
      theme(
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        axis.title.y = element_text(
          margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(
          margin = margin(t = 15, r = 0, b = 0, l = 0)),
        panel.grid.minor.x = element_blank(),
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
        legend.position = "bottom"
      ) +
      scale_x_date(
        labels = scales::label_date("%b %Y"),
        date_breaks = "4 months"
      ) +
      labs(
        x = NULL, y = "Antall punkt"
      ) +
      ggtitle(
        title_text,
        subtitle = sub_text
      )
  }


visualize_error_examples <- function(index_df, window_length, title_text, sub_text) {

    # window_length must be "12_months" or "24_months" or "36_months"

    index_df |>
      dplyr::filter(
        window == window_length
      ) |>
      ggplot2::ggplot(aes(x = month_object, y = em, color = version)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      # scale_color_manual(
      #   values = c(
      #     "old" = "#008ec2",
      #     "official" = "#444f55",
      #     "new" = "#ed9300"
      #   ),
      #   breaks = c(
      #     "old",
      #     "official",
      #     "new"
      #   ),
      #   labels = c(
      #     "Gammel",
      #     "Offisiell",
      #     "Ny"
      #   ),
      #   name = "Indeksversjon"
      # ) +
      theme_light() +
      theme(
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        axis.title.y = element_text(
          margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(
          margin = margin(t = 15, r = 0, b = 0, l = 0)),
        panel.grid.minor.x = element_blank(),
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
        legend.position = "bottom"
      ) +
      scale_x_date(
        labels = scales::label_date("%b %Y"),
        date_breaks = "4 months"
      ) +
      ylim(
        min(index_df$em),
        max(index_df$em)
      ) +
      labs(
        x = NULL, y = "Feilmargin (%-poeng)"
      ) +
      ggtitle(
        title_text,
        subtitle = sub_text
      )
  }


visualize_em_comparison <- function(df) {

  # df is an inner join of dfs holding results from the two index methods

  ggplot(df, aes(x_label, value, color = version, group = version)) +
    geom_point() +
    geom_line() +
    scale_color_manual(
      values = c(
        "dagens" = "#008ec2",
        "forbedret" = "#ed9300"
      ),
      breaks = c(
        "dagens",
        "forbedret"
      ),
      labels = c(
        "Dagens",
        "Forbedret"
      ),
      name = "Metode"
    ) +
    theme_light() +
    theme(
      axis.text.x = element_text(vjust = 0.5, angle = 90),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 15, b = 0, l = 0)),
      axis.title.x = element_text(
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        ),
      legend.position = "bottom"
    ) +
    ggplot2::scale_x_discrete(
      name = NULL,
      breaks = ~ dplyr::if_else(stringr::str_detect(.x, "apr|aug|des"), .x, "")
    ) +
    ylim(0, NA) +
    theme(
      plot.background = element_rect(fill = svv_background_color),
      panel.background = element_rect(fill = svv_background_color),
      legend.background = element_rect(fill = svv_background_color)
    ) +
    labs(
      x = NULL, y = "Relativ feilmargin"
    ) +
    ggtitle("Relativ feilmargin")

}


visualize_n_trp_comparison <- function(df) {

  ggplot(df, aes(x_label, value, color = version, group = version)) +
    geom_point() +
    geom_line() +
    scale_color_manual(
      values = c(
        "dagens" = "#008ec2",
        "forbedret" = "#ed9300"
      ),
      breaks = c(
        "dagens",
        "forbedret"
      ),
      labels = c(
        "Dagens",
        "Forbedret"
      ),
      name = "Metode"
    ) +
    ylim(0, NA) +
    theme_light() +
    theme(
      axis.text.x = element_text(vjust = 0.5, angle = 90),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 15, b = 0, l = 0)),
      axis.title.x = element_text(
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        ),
      legend.position = "bottom"
    ) +
    ggplot2::scale_x_discrete(
      name = NULL,
      breaks = ~ dplyr::if_else(stringr::str_detect(.x, "apr|aug|des"), .x, "")
    ) +
    theme(
      plot.background = element_rect(fill = svv_background_color),
      panel.background = element_rect(fill = svv_background_color),
      legend.background = element_rect(fill = svv_background_color)
    ) +
    labs(
      x = NULL, y = "Antall punkt"
    ) +
    ggtitle("Antall punkt i datagrunnlaget")

}


visualize_representativity <- function(df) {

  ggplot(df, aes(x_label, value, color = measure, group = measure)) +
    geom_point() +
    geom_line() +
    scale_color_manual(
      values = c(
        "n_trp_perc" = "#008ec2",
        "tw_perc" = "#ed9300",
        "tvd" = "#444f55"
      ),
      breaks = c(
        "n_trp_perc",
        "tw_perc",
        "tvd"
      ),
      labels = c(
        "Lenker",
        "Trafikkarbeid",
        "TVD"
      ),
      name = "Mål"
    ) +
    ylim(0, NA) +
    theme_light() +
    theme(
      axis.text.x = element_text(vjust = 0.5, angle = 90),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 15, b = 0, l = 0)),
      axis.title.x = element_text(
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        ),
      legend.position = "bottom"
    ) +
    ggplot2::scale_x_discrete(
      name = NULL,
      breaks = ~ dplyr::if_else(stringr::str_detect(.x, "apr|aug|des"), .x, "")
    ) +
    theme(
      plot.background = element_rect(fill = svv_background_color),
      panel.background = element_rect(fill = svv_background_color),
      legend.background = element_rect(fill = svv_background_color)
    ) +
    labs(
      x = NULL, y = "Andel (%)"
    ) +
    ggtitle("Representativitet i byindeksens utvalg av trafikklenker")

}

# Chains ----
calculate_all_index_chain_combinations <- function(df_index_i) {

  # Input: A df with two columns, year and index_i
  number_of_rows <- nrow(df_index_i)
  year_count <- 1

  first_index_year <- min(df_index_i$year)
  resulting_df <-
    df_index_i %>%
    dplyr::select(year) %>%
    tibble::add_row(year = first_index_year - 1) %>%
    dplyr::arrange(year)

  while (year_count <= number_of_rows) {

    df_index_i_iteration <-
      df_index_i %>%
      dplyr::slice(year_count:number_of_rows)

    first_year <- min(df_index_i_iteration$year)
    base_year <- as.character(first_year - 1)

    df_index_i_iteration_with_base_year <-
      df_index_i_iteration %>%
      tibble::add_row(year = first_year - 1, index_i = 1) %>%
      dplyr::arrange(year)

    next_column <-
      df_index_i_iteration_with_base_year %>%
      dplyr::mutate({{base_year}} := round(cumprod(index_i) * 100, digits = 1)) %>%
      dplyr::select(-index_i)

    resulting_df <-
      dplyr::left_join(
        resulting_df,
        next_column,
        by = "year"
      )

    year_count <- year_count + 1
  }

  resulting_df <- resulting_df %>%
    dplyr::mutate(year = as.character(year))

  return(resulting_df)

}

table_index_chains <- function(chosen_name) {

  n_index_all_years <-
    index_all_years |>
    dplyr::filter(
      area_name == chosen_name,
      month == 12
    ) |>
    nrow()

  # base::ifelse(
  #   n_index_all_years == 0,
  #   n_index_all_years <- NA,
  #   n_index_all_years
  # )

  table_here <-
    index_all_years |>
    dplyr::filter(
      area_name == chosen_name
    ) |>
    dplyr::select(
      area_name,
      years,
      months,
      n_unique_trp,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    flextable() |>
    colformat_double(j = 5, digits = 1) |>
    set_header_labels(
      area_name = "Byområde",
      years = "År",
      months = "Måneder",
      n_unique_trp = "Antall\npunkt",
      index_p = "Endring i\ntrafikk-\nmengde\n(%)",
      ci_lower = "Konfidensintervall (prosentpoeng)",
      ci_upper = ""
    ) |>
    merge_at(i = 1, j = 6:7, part = "header") |>
    flextable::align(i = 1, j = 6, align = "center", part = "header") |>
    flextable::align(j = 2:4, align = "center", part = "all") |>
    flextable::align(j = 5, align = "center", part = "header") |>
    bold(part = "header") |>
    font(fontname = "Lucida Sans Unicode", part = "all")  |>
    bg(bg = "#ED9300", part = "header") |>
    border_remove() |>
    hline_top(part = "header", border = borderline) |>
    hline_bottom(part = "all", border = borderline)

    if(n_index_all_years > 0){
      table_here <-
        table_here |>
        hline(
          i = n_index_all_years,
          part = "body",
          border = fp_border(color = "#dadada", width = 1)
        ) |>
        autofit()
    }else{
      table_here <-
        table_here |>
        autofit()
    }

  return(table_here)
}


plot_index_chain <- function(plot_df, chosen_name) {

  plot_df |>
    dplyr::filter(
      area_name == chosen_name
    ) |>
    ggplot2::ggplot(aes(x = year, y = index_p)) +
    ggplot2::geom_line(color = "#ed9300") +
    ggplot2::geom_point() +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "#58b02c",
      size = 0.8,
      alpha = 0.3
    ) +
    ggplot2::geom_ribbon(
      aes(
        ymin = ci_lower,
        ymax = ci_upper
      ),
      linetype = 2,
      alpha = 0.1,
      fill = "#444f55"
    ) +
    facet_grid(rows = vars(months)) +
    theme_light() +
    theme(
      axis.title.y = element_text(
        margin = margin(t = 0, r = 15, b = 0, l = 0)),
      panel.grid.minor = element_blank(),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        ),
      strip.text.y = element_text(angle = 90),
      strip.background = element_rect(fill = "#ed9300"),
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(1)) +
    labs(
      x = NULL,
      y = "Endring i trafikkmengde (%)",
      caption = "Data: Statens vegvesen, fylkeskommunene og kommunene"
    ) +
    ggtitle(
      "Estimert endring i sykkeltrafikk",
      subtitle = "Antall syklende sammenlignet med referanseår"
    )

}


visualize_rolling_index_comparison <- function(rolling_index_df) {

  title_text <- "Estimert endring i trafikkmengde siste glidende 3 år"
  sub_text  <-
    paste0(
      "Sammenlignet med ",
      stringr::str_sub(rolling_index_df$index_period[1], 1, 4)
    )

  rolling_index_df |>
    ggplot2::ggplot(aes(x = month_object, y = index_p, color = through_traffic)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "#58b02c",
      linewidth = 0.8,
      alpha = 0.3
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(
      aes(
        ymin = ci_lower,
        ymax = ci_upper
      ),
      linetype = 2,
      alpha = 0.1,
      fill = "#444f55"
    ) +
    scale_color_manual(
      values = c(
        "TRUE" = "#008ec2",
        "FALSE" = "#ed9300"
      ),
      breaks = c(
        "TRUE",
        "FALSE"
      ),
      labels = c(
        "Ja",
        "Nei"
      ),
      name = "Med gjennomgangstrafikk"
    ) +
    theme_light() +
    theme(
      axis.text.x = element_text(vjust = 0.5),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 15, b = 0, l = 0)),
      axis.title.x = element_text(
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        ),
      legend.position = "bottom",
      plot.background = element_rect(fill = svv_background_color),
      panel.background = element_rect(fill = svv_background_color),
      legend.background = element_rect(fill = svv_background_color),
      legend.key = element_rect(fill = svv_background_color)
    ) +
    scale_x_date(
      labels = scales::label_date("%b %Y")
    ) +
    labs(
      x = NULL, y = "Endring i trafikkmengde (%)",
      caption = "Data: Statens vegvesen."
    ) +
    ggtitle(
      title_text,
      subtitle = sub_text
    )
}
