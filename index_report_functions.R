#
borderline <- officer::fp_border(color = "black", style = "solid", width = 1)

create_point_table <- function(all_point_info_df) {

  all_point_info_df %>%
    select(
      name,
      road_reference,
      adt,
      year_aadt
    ) %>%
    flextable() %>%
    set_header_labels(
      name = "Navn",
      road_reference = "Vegreferanse",
      adt = "ÅDT",
      year_aadt = "År"
    ) %>%
    align(i = 1, j = c(3, 4), align = "center", part = "header") %>%
    align(j = c(4), align = "center", part = "body") %>%
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
    align(i = 1, j = c(4, 5), align = "center", part = "header") %>%
    align(j = c(5), align = "center", part = "body") %>%
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

create_point_adt_map <- function(all_point_info_df) {

  palett_adt <-
    leaflet::colorNumeric(
      palette = "Greens",
      domain = NULL
    )

  point_adt_map <-
    all_point_info_df |>
    leaflet::leaflet(
      #width = "100%",
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
      title = "\u00c5DT",
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
    colorFactor(palette = c("#db3b99", "#444f55"),
                domain = c("Bomstasjon", "Trafikkregistrering"))

  palett_adt <-
    colorNumeric(palette = "Greens",
                 domain = NULL)

  point_adt_map <-
    all_point_info_df %>%
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
      weight = 2,
      color = ~palett_stasjonstype(station_type),
      opacity = 0.8,
      fill = T,
      fillColor = ~palett_adt(adt),
      fillOpacity = 0.8
    ) %>%
    addLegend("bottomright",
              pal = palett_stasjonstype,
              values = ~station_type,
              title = "Stasjonstype",
              opacity = 0.7) %>%
    addLegend("bottomright",
              pal = palett_adt,
              values = ~adt,
              title = "ÅDT",
              opacity = 0.7,
              labFormat = labelFormat(big.mark = " "))

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
    round(abs(min(all_point_info_df$index, na.rm = T)), digits = 0) + 1
  positive_value <-
    round(max(all_point_info_df$index, na.rm = T), digits = 0) + 1

  # If even the max value is negative
  if(positive_value <= 0) positive_value <- 1

  rc1 <-
    colorRampPalette(colors = c("red", "white"), space = "Lab")(negative_value)

  ## Make vector of colors for values larger than 0 (180 colors)
  rc2 <-
    colorRampPalette(colors = c("white", "green"), space = "Lab")(positive_value)

  ## Combine the two color palettes
  rampcols <- c(rc1, rc2)

  palett_index <-
    colorNumeric(palette = rampcols,
                 domain = NULL)

  pointindex_map <-
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
      fillColor = ~palett_index(index),
      fillOpacity = 0.8,
      label = ~label_text
    ) %>%
    addLegend("bottomright",
              pal = palett_index,
              values = ~index,
              title = "Indeks",
              opacity = 0.7,
              labFormat = labelFormat(big.mark = " "))

  return(pointindex_map)
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
    align(
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
    align(j = 3, align = "center", part = "header") %>%
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
    align(j = 2, align = "left") %>%
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
      index_p = "Endring i\ntrafikkmengde\n(%)",
      n_trp = "Antall\npunkt",
      sd_sample_p = "Standardavvik\n(prosentpoeng)",
      standard_error_p = "Standardfeil\n(prosentpoeng)"
    ) %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    align(align = "center", part = "all") |>
    autofit() %>%
    height_all(height = .1)

  return(city_table)
}

road_category_names <- data.frame(
  road_category = c("E", "R", "F", "K"),
  road_category_name = c("Europaveg", "Riksveg", "Fylkesveg", "Kommunalveg"))


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
    align(align = "center", part = "header") %>%
    autofit() %>%
    height_all(height = .2) %>%
    set_caption("Estimert endring i trafikkmengde.",
                autonum = table_numbers,
                style = "Tabelltekst")

  return(corridor_table)
}


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


visualize_city_36_mdt_index <-
  function(city_36_month_df, caption_text, title_text, sub_text) {

    city_36_month_df %>%
    ggplot2::ggplot(aes(x = month_object, y = index_p)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "#58b02c",
      size = 0.8,
      alpha = 0.3
    ) +
    ggplot2::geom_line(color = "#ED9300") +
    ggplot2::geom_point(color = "#ED9300") +
    ggplot2::geom_ribbon(
      aes(
        ymin = ci_lower,
        ymax = ci_upper
      ),
      linetype = 2,
      alpha = 0.1,
      fill = "#444f55"
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
        )
    ) +
    scale_x_date(
      labels = scales::label_date("%b %Y")
    ) +
    ylim(
      -max(abs(city_36_month_df$ci_lower)),
      max(abs(city_36_month_df$ci_upper))
    ) +
    labs(
      x = NULL, y = "Endring i trafikkmengde (%)",
      caption = caption_text) +
    ggtitle(
      title_text,
      subtitle = sub_text
    )
}


visualize_rolling_indices <-
  function(rolling_indices_df, caption_text, title_text, sub_text) {

    rolling_indices_df %>%
      ggplot2::ggplot(aes(x = month_object, y = index_p, color = window, linetype = window)) +
      ggplot2::geom_hline(
        yintercept = 0,
        color = "#58b02c",
        size = 0.8,
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
        legend.position = "bottom"
      ) +
      scale_x_date(
        labels = scales::label_date("%b %Y")
      ) +
      ylim(
        -max(abs(rolling_indices_df$index_p)),
        max(abs(rolling_indices_df$index_p))
      ) +
      labs(
        x = NULL, y = "Endring i trafikkmengde (%)",
        caption = caption_text) +
      ggtitle(
        title_text,
        subtitle = sub_text
      )
  }


table_index_chains <- function(chosen_name) {

  n_index_all_years <-
    index_all_years |>
    dplyr::filter(
      area_name == chosen_name,
      month == 12
    ) |>
    nrow()

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
    align(i = 1, j = 6, align = "center", part = "header") |>
    align(j = 2:4, align = "center", part = "all") |>
    align(j = 5, align = "center", part = "header") |>
    bold(part = "header") |>
    font(fontname = "Lucida Sans Unicode", part = "all")  |>
    bg(bg = "#ED9300", part = "header") |>
    border_remove() |>
    hline_top(part = "header", border = borderline) |>
    hline_bottom(part = "all", border = borderline) |>
    hline(
      i = n_index_all_years,
      part = "body",
      border = fp_border(color = "#dadada", width = 1)
    ) |>
    autofit()

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
