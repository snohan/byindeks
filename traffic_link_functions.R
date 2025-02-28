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

summarise_link_population_by_function_class <- function(link_population) {

  function_class_pop <-
    link_population |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      count = n(),
      tw = sum(traffic_work_km, na.rm = TRUE),
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
      tw = sum(traffic_work_km, na.rm = TRUE),
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
    ) |>
    # explicitly have rows with zeroes in the sample for categories that exist in population
    tidyr::complete(
      function_class, nesting(selection, total_count, total_tw),
      fill = list(
        count = 0,
        tw = 0,
        percentage_count = 0,
        percentage_tw = 0
      )
    ) |>
    dplyr::filter(
      # since function class is factor with levels, "E" shows up
      #function_class != "E"
    )

  return(function_class_stats)

}

# Statistical distance
# Comparing sample and population - do they look alike?

# Kji-kvadrat test (med MC)
# test <- chisq.test(
#   x, # tw in selection
#   p, # expected probabilities, percentage tw in population
#   simulate.p.value = TRUE
# )
# This won't work because the observations x are too large

# chi_test <-
#   stats::chisq.test(
#     #x = function_class_stats_wide$tw_utvalg,
#     #x = c(103980352, 25393543, 44294894, 10000),
#     x = function_class_stats_wide$fake,
#     p = function_class_stats_wide$percentage_tw_populasjon,
#     simulate.p.value = TRUE
#   )
# df is supposed to be NA in this case, as this is a goodness of fit-test
#chi_test

# Total Variation distance (not the  supremum definition, which is event-wise, rather the pd-one - easy to understand)
# Hellinger distance (between 0 and 1, more difficult, 0 to 1)
# Kullback-Leibler Divergence (too complicated, from 0 to Inf) - won't work when some category is zero!

calculate_statistical_distance <- function(function_class_stats_df) {

  function_class_stats_wide <-
    function_class_stats_df |>
    dplyr::select(
      function_class,
      selection,
      percentage_tw
    ) |>
    tidyr::pivot_wider(
      names_from = selection,
      values_from = percentage_tw
    ) |>
    dplyr::mutate(
      variation_distance = abs(utvalg - populasjon),
      squared_diff_of_square_roots = (sqrt(utvalg) - sqrt(populasjon))^2
    )

  function_class_stats_summarised <-
    function_class_stats_wide |>
    dplyr::summarise(
      tvd = round(0.5 * sum(variation_distance), 2),
      hellinger = round((1 / sqrt(2)) * sqrt(sum(squared_diff_of_square_roots)), 2)
    )

  return(function_class_stats_summarised)

}

visualize_function_class_distribution <- function(link_population) {

  function_class_tw_plot <-
    # TODO: replace function call, presuming this has been done before calling this viz function?
    summarise_link_population_by_function_class(link_population) |>
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
      palette = c("#158925", "#077197", "#b63434", "#444f55", "#858d90"),
      domain = c("A", "B", "C", "D", "E")
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
        traffic_work_mill_km = sum(traffic_work_km, na.rm = TRUE) / 1e6
      ) |>
      dplyr::mutate(
        selection = "population"
      ),
    link_df |>
      sf::st_drop_geometry() |>
      dplyr::filter(!is.na(point_id)) |>
      dplyr::summarise(
        number_of_links = n(),
        traffic_work_mill_km = sum(traffic_work_km, na.rm = TRUE) / 1e6
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

table_statistical_distance_comparison <- function(stat_df) {

  stat_df |>
    flextable::flextable() |>
    colformat_double(j = 3:4, digits = 2) |>
    set_header_labels(
      population = "Populasjon",
      selection = "Utvalg",
      tvd = "TVD",
      hellinger = "Hellinger"
    ) |>
    bold(part = "header") |>
    bg(bg = "#ED9300", part = "header")
}


calculate_error_margin_with_finite_population <- function() {



}

# Create graph
create_graph_from_links <- function(link_df) {

  nodes_in_area <-
    dplyr::bind_rows(
      nodes |>
        dplyr::filter(
          id %in% link_df$from
        ),
      nodes |>
        dplyr::filter(
          id %in% link_df$to
        )
    ) |>
    dplyr::distinct() |>
    tibble::as_tibble() |>
    tibble::rowid_to_column("id_int") |>
    dplyr::select(
      id = id_int,
      id_original = id
    )

  edges_in_area <-
    link_df |>
    sf::st_drop_geometry() |>
    dplyr::left_join(
      nodes_in_area,
      by = join_by(from == id_original)
    ) |>
    dplyr::rename(
      from_int = id
    ) |>
    dplyr::left_join(
      nodes_in_area,
      by = join_by(to == id_original)
    ) |>
    dplyr::rename(
      to_int = id
    ) |>
    dplyr::select(
      -from,
      -to
    ) |>
    dplyr::rename(
      from = from_int,
      to = to_int
    ) |>
    dplyr::arrange(
      from
    )

  area_graph <-
    tidygraph::tbl_graph(
      nodes = nodes_in_area,
      node_key = "id",
      edges = edges_in_area,
      directed = FALSE
    )

  # Verify that the graph is only one component
  components <- igraph::components(area_graph)
  components$no

  if(components$no > 1) print("Warning: More than one graph component!")

  return(area_graph)

}


create_line_graph <- function(link_df) {

  # Buliding a line graph manually to retain attributes
  # Let the original traffic link graph be G, and let the line graph be L.
  # Links in G will be nodes in L. Nodes in L will be connected if they were adjacent in G.
  L_nodes <-
    link_df |>
    sf::st_drop_geometry() |>
    tibble::rowid_to_column("id") |>
    dplyr::select(
      -from,
      -to
    )

  # 1. Starting with the links in G, find which node ids they were connected to:
  links_stripped <-
    link_df |>
    sf::st_drop_geometry() |>
    dplyr::select(
      link_id, from, to
    ) |>
    tidyr::pivot_longer(
      cols = c(from, to),
      names_to = "new_link_id",
      values_to = "connection_id"
    ) |>
    dplyr::select(
      -new_link_id
    )

  # 2. To find adjacent links in G, self join the list of links and their node ids:
  L_links <-
    links_stripped |>
    dplyr::left_join(
      links_stripped,
      by = join_by(connection_id),
      relationship = "many-to-many"
    ) |>
    # Must remove self loops
    dplyr::filter(
      !(link_id.x == link_id.y)
    ) |>
    # Must remove duplicates
    dplyr::rowwise() |>
    dplyr::mutate(
      sorted_id = list(c(link_id.x, link_id.y)) |> purrr::map(sort)
    ) |>
    dplyr::mutate(
      new_from = purrr::pluck(sorted_id, 1, 1),
      new_to = purrr::pluck(sorted_id, 2, 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      connection_id, # need to have this here to not discard parallell links
      new_from,
      new_to
    ) |>
    dplyr::distinct() |>
    dplyr::select(
      -connection_id
    ) |>
    # Add new node ids
    dplyr::left_join(
      L_nodes |>
        dplyr::select(
          id, link_id
        ),
      by = join_by(new_from == link_id)
    ) |>
    dplyr::rename(
      from = id
    ) |>
    dplyr::left_join(
      L_nodes |>
        dplyr::select(
          id, link_id
        ),
      by = join_by(new_to == link_id)
    ) |>
    dplyr::select(
      from,
      to = id
    )

  # 3. Build the graph
  L_graph <-
    tidygraph::tbl_graph(
      nodes = L_nodes,
      node_key = "id",
      edges = L_links,
      directed = FALSE
    )

  return(L_graph)

}


calculate_mean_distance_to_city_index_points <- function(l_graph) {

  # Subset vertex id based on sample of city_trps
  l_nodes <-
    tibble::tibble(
      id = igraph::vertex_attr(l_graph, "id"),
      city_trp = igraph::vertex_attr(l_graph, "city_trp")
    )

  non_selected_nodes <-
    l_nodes |>
    dplyr::filter(
      !city_trp
    )

  selected_nodes <-
    l_nodes |>
    dplyr::filter(
      city_trp
    )

  selected_node_names <-
    paste0(
      "V",
      c(1:nrow(selected_nodes))
    )

  distance_matrix <-
    igraph::distances(
      l_graph,
      v = non_selected_nodes$id,
      to = selected_nodes$id
    )

  # To avoid warnings form making a tibble
  base::colnames(distance_matrix) <- selected_node_names

  distance_tibble <-
    distance_matrix |>
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      min_dist =
        min(dplyr::c_across(tidyselect::everything()))
    ) |>
    dplyr::ungroup()

  mean_shortest_distance <-
    sum(distance_tibble$min_dist) / (nrow(non_selected_nodes) - nrow(selected_nodes))

  return(mean_shortest_distance)

}


get_link_population_inside_municipalities <- function(area_municipality_ids) {

  # municipality_ids: integer vector

  link_ids_intersecting_municipalities <-
    link_municipality_id |>
    dplyr::filter(
      municipality_id %in% area_municipality_ids
    )

  link_ids_crossing_outer_borders <-
    link_municipality_id |>
    dplyr::filter(
      link_id %in% link_ids_intersecting_municipalities$link_id,
      !(municipality_id %in% area_municipality_ids)
    )

  link_ids_inside_municipalities <-
    link_ids_intersecting_municipalities |>
    dplyr::filter(
      !(link_id %in% link_ids_crossing_outer_borders$link_id)
    )

  # Remove duplicates from links crossing internal borders in case of neighboring municipalities
  link_ids_inside_municipalities_unique <-
    link_ids_inside_municipalities$link_id |>
    base::unique()

  links_inside_municipalities <-
    links |>
    dplyr::filter(
      link_id %in% link_ids_inside_municipalities_unique
    )

  return(links_inside_municipalities)

}