#

create_point_adt_map <- function(all_point_info_df) {
  palett_adt <-
    colorNumeric(palette = "Greens",
                 domain = NULL)

  point_adt_map <- all_point_info_df %>%
    leaflet(width = "100%",
            height = 700,
            options = leafletOptions(crs = nvdb_crs,
                                     zoomControl = F)) %>%
    addTiles(urlTemplate = nvdb_map_url,
             attribution = nvdb_map_attribution) %>%
    addCircleMarkers(
      radius = 6,
      stroke = T,
      weight = 2,
      color = "#444f55",
      opacity = 0.8,
      fill = T,
      fillColor = ~palett_adt(adt),
      fillOpacity = 0.8
    ) %>%
    addLegend("bottomright",
              pal = palett_adt,
              values = ~adt,
              title = "ADT",
              opacity = 0.7,
              labFormat = labelFormat(big.mark = " "))

  return(point_adt_map)
}


create_pointindex_map <- function(all_point_info_df) {
  # Create a red-green scale based on index values
  negative_value <- round(abs(min(all_point_info_df$index, na.rm = T)), digits = 0) + 1
  positive_value <- round(max(all_point_info_df$index, na.rm = T), digits = 0) + 1

  rc1 <- colorRampPalette(colors = c("red", "white"), space = "Lab")(negative_value)

  ## Make vector of colors for values larger than 0 (180 colors)
  rc2 <- colorRampPalette(colors = c("white", "green"), space = "Lab")(positive_value)

  ## Combine the two color palettes
  rampcols <- c(rc1, rc2)

  palett_index <-
    colorNumeric(palette = rampcols,
                 domain = NULL)

  pointindex_map <- all_point_info_df %>%
    leaflet(width = "100%",
            height = 700,
            options = leafletOptions(crs = nvdb_crs,
                                     zoomControl = F)) %>%
    addTiles(urlTemplate = nvdb_map_url,
             attribution = nvdb_map_attribution) %>%
    addCircleMarkers(
      radius = 6,
      stroke = T,
      weight = 2,
      color = "#444f55",
      opacity = 0.8,
      fill = T,
      fillColor = ~palett_index(index),
      fillOpacity = 0.8
    ) %>%
    addLegend("bottomright",
              pal = palett_index,
              values = ~index,
              title = "Indeks",
              opacity = 0.7,
              labFormat = labelFormat(big.mark = " "))

  return(pointindex_map)
}

borderline <- officer::fp_border(color = "black", style = "solid", width = 1)

create_city_index_table <- function(city_info) {

  city_table <- city_info %>%
    dplyr::select(year, index, ki_start, ki_slutt) %>%
    flextable::flextable() %>%
    colformat_num(j = c("index", "ki_start", "ki_slutt"), digits = 1) %>%
    set_header_labels(year = "Periode",
                      index = "Endring i trafikkmengde (%)",
                      ki_start = "95 % konfidensintervall") %>%
    merge_at(i = 1, j = 3:4, part = "header") %>%
    bold(part = "header") %>%
    fontsize(size = 9, part = "all") %>%
    font(fontname = "Lucida Sans Unicode", part = "all") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    autofit() %>%
    height_all(height = .2) %>%
    padding(padding.top = .3,
            padding.bottom = .3) %>%
    set_caption("Estimert endring i trafikkmengde.")

  return(city_table)
}

create_monthly_city_index_table <- function(city_monthly) {

  monthly_table <- city_monthly %>%
    dplyr::filter(!is.na(index)) %>%
    dplyr::select(year, periode, index) %>%
    flextable::flextable() %>%
    colformat_num(j = c("index"), digits = 1) %>%
    set_header_labels(year = "År",
                      periode = "Periode",
                      index = "Endring i trafikkmengde (%)") %>%
    bold(part = "header") %>%
    fontsize(size = 9, part = "all") %>%
    font(fontname = "Lucida Sans Unicode", part = "all") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    autofit() %>%
    height_all(height = .2) %>%
    padding(padding.top = .3,
            padding.bottom = .3) %>%
    set_caption("Estimert endring i trafikkmengde per måned.")

  return(monthly_table)
}


