{
  library(tidyverse)
  library(sf)
  library(sfnetworks)
  library(igraph)
  #library(tidygraph)
}

# Read ----
# Geojson files from ADM
directed_links <-
  sf::st_read("C:/Users/snohan/Desktop/directed_traffic_links_2023.geojson") |>
  dplyr::select(
    id,
    isTrafficWithMetering,
    length,
    primaryTrpId,
    lowestSpeedLimit,
    startTrafficNodeId,
    endTrafficNodeId,
    trafficVolumes,
    municipalities,
    roadCategory,
    functionalRoadClasses,
    functionClasses
  )

nodes <-
  sf::st_read("C:/Users/snohan/Desktop/traffic-nodes-2023.geojson")





# Directed graph ----

# Build directed graph object from nodes and spatially implicit edges
# Implicit because there are gaps (roundabouts) which will give:
# Error: Edge boundaries do not match their corresponding nodes
# Therefore: edges_as_lines = FALSE,

# The edges must contain columns 'from' and 'to'

directed_graph <-
  sfnetworks::sfnetwork(
    nodes = nodes_chosen,
    edges = links_directed,
    directed = TRUE,
    node_key = "node_id",
    edges_as_lines = FALSE,
    force = FALSE
  )

plot(directed_graph)

ggplot2::autoplot(directed_graph)

# Verify that directed graph contains one single component
directed_graph |> igraph::count_components(mode="weak")

cc <- directed_graph |> igraph::components(mode="weak")

rev(table(cc$csize))

## Compute edge betweenness centrality ----
directed_graph_bc <-
  directed_graph %>%
  tidygraph::activate("edges") |>
  dplyr::mutate(
    bc_travel_time =
      tidygraph::centrality_edge_betweenness(
        weights = travel_time_minutes,
        directed = TRUE,
        cutoff = NULL)
  )

# Extract link metainfo, now including BC
# TODO: And reattaching geometry
edges_main <-
  directed_graph_bc |>
  tidygraph::activate("edges") |>
  tibble::as_tibble()




# graph_test <-
#   tidygraph::tbl_graph(
#     nodes = nodes_chosen,
#     node_key = "node_id",
#     edges = links_directed_nodes
#   )
#
# test <- igraph::graph(c(links_directed_nodes$from, links_directed_nodes$to))



