#

create_point_table <- function(all_point_info_df, caption_text) {

  all_point_info_df %>%
    select(name, road_reference, adt, year) %>%
    flextable() %>%
    colformat_int(j = "year", big.mark = "") %>%
    set_header_labels(name = "Navn",
                      road_reference = "Vegreferanse",
                      adt = "ÅDT",
                      year = "År") %>%
    align(i = 1, j = 4, align = "center", part = "header") %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    autofit() %>%
    height_all(height = .2) %>%
    set_caption(caption_text,
                autonum = table_numbers,
                style = "Tabelltekst")
}

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

create_point_adt_map_trondheim <- function(all_point_info_df) {
  palett_stasjonstype <-
    colorFactor(palette = c("#db3b99", "#444f55"),
                domain = c("Bom", "Trafikkregistrering"))

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
              title = "ADT",
              opacity = 0.7,
              labFormat = labelFormat(big.mark = " "))

  return(point_adt_map)
}


create_pointindex_map <- function(all_point_info_df) {
  # Create a red-green scale based on index values
  negative_value <- round(abs(min(all_point_info_df$index, na.rm = T)), digits = 0) + 1
  positive_value <- round(max(all_point_info_df$index, na.rm = T), digits = 0) + 1

  # If even the max value is negative
  if(positive_value < 0) positive_value <- 1

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
    dplyr::select(year_from_to, index_p, ci_start, ci_end) %>%
    flextable::flextable() %>%
    colformat_double(j = c("index_p", "ci_start", "ci_end"), digits = 1) %>%
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
    colformat_int(j = "year", big.mark = "") %>%
    colformat_double(j = c("index_p"), digits = 1) %>%
    set_header_labels(month_name = "Treårsperiodens slutt",
                      index_p = "Endring i \n trafikkmengde (%)") %>%
    merge_at(i = 1, j = 1:2, part = "header") %>%
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
    height_all(height = .2) %>%
    set_caption("Samlet veglengde per kommune.",
                autonum = table_numbers,
                style = "Tabelltekst")

  return(road_lengths_table)
}

create_city_road_length_table <- function(road_lengths) {

  road_lengths_table <- road_lengths %>%
    select(road_category, road_category_name, length_km) %>%
    group_by(road_category, road_category_name) %>%
    summarise(length_km = sum(length_km)) %>%
    ungroup() %>%
    select(-road_category) %>%
    select(road_category_name, length_km) %>%
    flextable() %>%
    set_header_labels(road_category_name = "Vegkategori",
                      length_km = "Lengde \n (km)") %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    #align(j = 1, align = "center") %>%
    autofit() %>%
    height_all(height = .2) %>%
    set_caption("Samlet veglengde.",
                autonum = table_numbers,
                style = "Tabelltekst")

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
