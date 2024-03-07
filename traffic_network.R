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


# Edited graph
edges_to_remove <-
  c(
    "1018242714-WITH",
    "1018242714-AGAINST"
  )


directed_links_2 <-
  directed_links |>
  dplyr::filter(
    !(id %in% edges_to_remove)
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


# Pull out BC
bc_1 <-
  directed_graph |>
  tidygraph::activate("edges") |>
  tibble::as_tibble() |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    bc_travel_time
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
  bc_1 |>
  dplyr::left_join(
    bc_2,
    by = dplyr::join_by(id)
  ) |>
  dplyr::mutate(
    delta_bc = bc_travel_time_2 - bc_travel_time
  ) |>
  dplyr::select(
    id, delta_bc
  )

bc_delta |>
  dplyr::filter(
    delta_bc != 0
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
      paste(id, roadSystemReferences, bc_travel_time, delta_bc, sep = "<br/>") |>
      purrr::map(htmltools::HTML)
    #link_label = purrr::map(link_label, htmltools::HTML)
  )


## write
readr::write_rds(
  directed_graph_bc_delta,
  "trd_graf.rds"
)

