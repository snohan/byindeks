# Prepare a traffic link graph for map viewing
{
  library(tidyverse)
  library(sf)
  library(sfnetworks)
  library(igraph)
  library(tidygraph)
}

# Choose
trd_kommuner <-
  c(
    5001,
    5031,
    5035,
    5028,
    5029,
    5059
  )

# Read ----
## Links ----
# Geojson files from ADM
test_directed_links <-
  sf::st_read(
    "C:/Users/snohan/Desktop/directed_traffic_links_2023.geojson",
    query =
      "SELECT *
      FROM \"directed_traffic_links_2023\"
      LIMIT 10"
  )

directed_links <-
  sf::st_read(
    "C:/Users/snohan/Desktop/directed_traffic_links_2023.geojson",
    query =
      "SELECT
          id, parentTrafficLinkId, length, primaryTrpId, lowestSpeedLimit, isFerryTrafficLink,
          hasOnlyPublicTransportLanes, isBlocked,
          startTrafficNodeId, endTrafficNodeId, municipalities, roadSystemReferences,
          functionalRoadClasses, functionClasses
      FROM \"directed_traffic_links_2023\""
    # trafficVolumes
    # isTrafficWithMetering
    # roadCategory
  ) |>
  dplyr::rename(
    trp_id = primaryTrpId,
    from = startTrafficNodeId,
    to = endTrafficNodeId
  ) |>
  tidyr::unnest_longer(
    municipalities,
    values_to = "municipality_id"
  ) |>
  dplyr::filter(
    municipality_id %in% trd_kommuner,
    hasOnlyPublicTransportLanes == FALSE,
    isBlocked == FALSE
  ) |>
  # Need to remove duplicate links (crossing municipality boundaries)
  dplyr::select(-municipality_id) |>
  dplyr::distinct() |>
  dplyr::mutate(
    speed =
      dplyr::case_when(
        isFerryTrafficLink == TRUE ~ 30,
        TRUE ~ lowestSpeedLimit
      ),
    travel_time_minutes = length / (speed * (1000 / 60))
  ) |>
  dplyr::select(
    id,
    from, to,
    length,
    trp_id,
    roadSystemReferences,
    functionalRoadClasses,
    functionClasses,
    travel_time_minutes,
    geometry
  ) |>
  sf::st_as_sf()


## Nodes ----
nodes <-
  sf::st_read("C:/Users/snohan/Desktop/traffic-nodes-2023.geojson") |>
  # Trim nodes to the ones in area
  dplyr::filter(
    id %in% unique(c(directed_links$from, directed_links$to))
  ) |>
  dplyr::select(id)


# Directed graph ----

# Build directed graph object from nodes and spatially implicit edges
# Implicit because there are gaps (roundabouts) which will give:
# Error: Edge boundaries do not match their corresponding nodes
# Therefore: edges_as_lines = FALSE,

# The edges must contain columns 'from' and 'to'

directed_graph <-
  sfnetworks::sfnetwork(
    nodes = nodes,
    edges = directed_links,
    directed = TRUE,
    node_key = "id",
    edges_as_lines = TRUE,
    # Will error if force = FALSE and more than one component
    force = TRUE
  ) |>
  tidygraph::activate("edges") |>
  dplyr::mutate(
    bc_travel_time =
      tidygraph::centrality_edge_betweenness(
        weights = travel_time_minutes,
        directed = TRUE,
        cutoff = NULL
      )
  )

links <-
  directed_graph |>
  tidygraph::activate("edges") |>
  tibble::as_tibble()

base::class(directed_graph)
base::length(directed_graph)


ggplot2::autoplot(directed_graph)

# directed_graph |>
#   ggraph::ggraph() +
#     #geom_edge_sf()
#     geom_edge_sf(aes(alpha = bc_travel_time))

# Verify that graph contains one single component
cc <-
  directed_graph |>
  igraph::components(mode="weak")

cc$csize


## Smoothing out pseudo nodes? (I-kryss)


# Edited graph 1 ----
edges_to_remove_1 <-
  c(
    "1018242714-WITH",
    "1018242714-AGAINST"
  )

directed_links_1 <-
  directed_links |>
  dplyr::filter(
    !(id %in% edges_to_remove_1)
  )

directed_graph_1 <-
  sfnetworks::sfnetwork(
    nodes = nodes,
    edges = directed_links_1,
    directed = TRUE,
    node_key = "id",
    edges_as_lines = TRUE,
    # Will error if force = FALSE and more than one component
    force = TRUE
  ) |>
  tidygraph::activate("edges") |>
  dplyr::mutate(
    bc_travel_time_1 =
      tidygraph::centrality_edge_betweenness(
        weights = travel_time_minutes,
        directed = TRUE,
        cutoff = NULL
      )
  )


# Edited graph 2 ----
edges_to_remove_2 <-
  c(
    "1018242714-WITH",
    "1018242714-AGAINST",
    "1018243119-WITH",
    "1018243119-AGAINST"
  )

directed_links_2 <-
  directed_links |>
  dplyr::filter(
    !(id %in% edges_to_remove_2)
  )

directed_graph_2 <-
  sfnetworks::sfnetwork(
    nodes = nodes,
    edges = directed_links_2,
    directed = TRUE,
    node_key = "id",
    edges_as_lines = TRUE,
    # Will error if force = FALSE and more than one component
    force = TRUE
  ) |>
  tidygraph::activate("edges") |>
  dplyr::mutate(
    bc_travel_time_2 =
      tidygraph::centrality_edge_betweenness(
        weights = travel_time_minutes,
        directed = TRUE,
        cutoff = NULL
      )
  )


# Compare BC ----
bc <-
  directed_graph |>
  tidygraph::activate("edges") |>
  tibble::as_tibble() |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    bc_travel_time
  )

bc_1 <-
  directed_graph_1 |>
  tidygraph::activate("edges") |>
  tibble::as_tibble() |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    bc_travel_time_1
  )

bc_2 <-
  directed_graph_2 |>
  tidygraph::activate("edges") |>
  tibble::as_tibble() |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    bc_travel_time_2
  )

bc_delta <-
  bc |>
  dplyr::left_join(
    bc_1,
    by = dplyr::join_by(id)
  ) |>
  dplyr::left_join(
    bc_2,
    by = dplyr::join_by(id)
  ) |>
  dplyr::mutate(
    delta_bc_1 = bc_travel_time_1 - bc_travel_time,
    delta_bc_2 = bc_travel_time_2 - bc_travel_time
  ) |>
  dplyr::select(
    id, delta_bc_1, delta_bc_2
  )

# table
bc_delta |>
  dplyr::filter(
    delta_bc_1 != 0
  ) |>
  nrow()

directed_links_bc_delta <-
  directed_links |>
  dplyr::left_join(
    bc_delta,
    by = dplyr::join_by(id)
  )

directed_graph_bc_delta <-
  sfnetworks::sfnetwork(
    nodes = nodes,
    edges = directed_links_bc_delta,
    directed = TRUE,
    node_key = "id",
    edges_as_lines = TRUE,
    # Will error if force = FALSE and more than one component
    force = TRUE
  ) |>
  tidygraph::activate("edges") |>
  dplyr::mutate(
    bc_travel_time =
      tidygraph::centrality_edge_betweenness(
        weights = travel_time_minutes,
        directed = TRUE,
        cutoff = NULL
      ),
    ln_bc = log(bc_travel_time + 1),
    link_label =
      paste(id, roadSystemReferences, bc_travel_time, delta_bc_1, delta_bc_2, sep = "<br/>") |>
      purrr::map(htmltools::HTML)
    #link_label = purrr::map(link_label, htmltools::HTML)
  )


## write
readr::write_rds(
  directed_graph_bc_delta,
  "trd_graf.rds"
)


# From work on representativeness ----
# If more than one, keeping the largest
#graph_tidy <- igraph::largest_component(graph)

# NB! Node ids are being reset to 1:n
# igraph::E(graph_tidy)
# igraph::edge_attr(graph_tidy)
# edges <- igraph::as_edgelist(graph_tidy)

# Or, if more than one
# biggest_component_id <- which.max(components$csize)
#
# nodes_in_biggest_component <-
#   nodes_nj |>
#   dplyr::mutate(
#     component_membership = components$membership
#   ) |>
#   dplyr::filter(
#     component_membership == biggest_component_id
#   )
#
# graph_tidy <- igraph::induced_subgraph(graph, nodes_in_biggest_component$id)
#igraph::isomorphic(test, graph_tidy)


# Plot
# ggraph(graph, layout = 'auto') +
#   geom_edge_link(
#     aes(
#       edge_colour = functional_road_class,
#       edge_width = city_trp
#     )
#   ) +
#   geom_node_point()
#paletteer::scale_color_paletteer_d("LaCroixColoR::PeachPear")

#igraph::diameter(graph)

# Clusters
# g_clusters <- igraph::cluster_edge_betweenness(graph)
# g_clusters$membership
# sizes(g_clusters)
# plot(g_clusters, graph)


# Centrality
# G_graph <-
#   graph |>
#   igraph::set_edge_attr(
#     name = "edge_betweenness",
#     value = igraph::edge_betweenness(graph)
#   )
#
# igraph::edge_attr(G_graph)

# Plot
# ggraph(G_graph, layout = 'auto') +
#   geom_edge_link(
#     aes(
#       edge_colour = edge_betweenness,
#       edge_width = city_trp
#     )
#   ) +
#   geom_node_point()


## Line graph ----
# I.e. links will be nodes and vice versa.
# This in order to do analysis on links' spread and closeness

# line_graph <-
#   igraph::make_line_graph(graph) |>
#   tidygraph::as_tbl_graph()
# But edge attributes lostfrom original graph to the nodes in line graph

# ggraph(line_graph, layout = 'auto') +
#   geom_edge_link() +
#   geom_node_point()
#
# igraph::diameter(line_graph)
# igraph::E(line_graph)
#igraph::vertex_attr(line_graph)

# ggraph(L_graph, layout = 'auto') +
#   geom_edge_link() +
#   geom_node_point(
#     aes(
#       color = functional_road_class,
#       size = city_trp
#     )
#   )
#
# igraph::diameter(L_graph)
# igraph::vertex_attr(L_graph)
# degrees <- igraph::degree_distribution(L_graph)


## Node coverage ----
percentage_nodes_sampled <- nrow(links_with_city_trp_nj) / nrow(links_nj_final_plain)

# Including nearest neighbors
selected_neighbors <-
  L_nodes |>
  dplyr::rowwise() |>
  dplyr::mutate(
    neighbors = list(igraph::neighbors(L_graph, id))
  ) |>
  dplyr::filter(
    city_trp == 1
  )

selected_nodes_and_neighbors <-
  selected_neighbors |>
  tidyr::unnest(
    neighbors
  ) |>
  dplyr::select(
    nodes = neighbors
  ) |>
  dplyr::mutate(
    nodes = as.numeric(nodes)
  ) |>
  dplyr::bind_rows(
    selected_neighbors |>
      dplyr::select(
        nodes = id
      )
  ) |>
  dplyr::distinct()

percentage_nodes_and_neighbors_sampled <- nrow(selected_nodes_and_neighbors) / nrow(links_nj_final_plain)


igraph::mean_distance(L_graph)

test_tbl <-
  distance_matrix |>
  tibble::as_tibble()


shortest_distances <-
  test_tbl |>
  dplyr::mutate(
    shortest = purrr::pmap_dbl(test_tbl, min)
  )

mean_shortest_distances <- mean(shortest_distances$shortest)
sd_shortest_distances <- sd(shortest_distances$shortest)
max_shortest_distances <- max(shortest_distances$shortest)




## Weighted coverage ----
# Traffic work is what counts
percentage_traffic_work <-
  L_nodes |>
  dplyr::summarise(
    traffic_work = sum(traffic_work, na.rm = TRUE),
    .by = "city_trp_lgl"
  ) |>
  tidyr::pivot_wider(
    names_from = "city_trp_lgl",
    names_prefix = "trp_",
    values_from = "traffic_work"
  ) |>
  dplyr::mutate(
    total_tw = sum(trp_TRUE, trp_FALSE),
    percentage_tw = trp_TRUE / total_tw
  )


## Compare distributions ----
# Functional road class
L_nodes |>
  dplyr::summarise(
    n = n(),
    .by = c(city_trp_lgl, functional_road_class)
  ) |>
  tidyr::pivot_wider(
    names_from = city_trp_lgl,
    names_prefix = "trp_",
    values_from = n
  ) |>
  dplyr::arrange(
    functional_road_class
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    trp_all = sum(trp_FALSE, trp_TRUE, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    percentage_TRUE = trp_TRUE / sum(trp_TRUE, na.rm = TRUE),
    percentage_all = trp_all / sum(trp_FALSE, trp_TRUE, na.rm = TRUE)
  )

L_nodes |>
  ggplot2::ggplot(aes(functional_road_class)) +
  geom_bar() +
  facet_wrap(
    ~ city_trp_lgl,
    ncol = 1)


