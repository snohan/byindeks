# Save last plot as SVG
library(svglite)
ggplot2::ggsave(
  # filename = "images/nj_chain_graph.svg",
  filename = "images/nj_improved_3y.svg",
  width = 8, height = 5
)


# Save maps
# Can not be used in PowerPoint
htmlwidgets::saveWidget(
  trd_map,
  file = "images/trd_map.html"
)


# Mapshot
library(mapview)
mapview::mapshot2(
  trd_map,
  file = "images/trd_map.png"
)

# Put toghether
library(patchwork)

fig_improved_1y_3y <-
  dplyr::bind_rows(
    qmd_index_examples_cmdt_nj_chained  |> dplyr::mutate(area = "Nord-Jæren"),
    qmd_index_examples_cmdt_brg         |> dplyr::mutate(area = "Bergensområdet"),
    qmd_index_examples_cmdt_oslo        |> dplyr::mutate(area = "Osloområdet"),
    qmd_index_examples_cmdt_trd         |> dplyr::mutate(area = "Trondheimsområdet")
  ) |> 
  dplyr::mutate(
    area = factor(area, levels = c("Nord-Jæren", "Bergensområdet", "Osloområdet", "Trondheimsområdet"))
  ) |> 
  visualize_rolling_cmdt_indices_12_36("Glidende indeks, forbedret metode", NULL)

fig_improved_1y_3y

ggplot2::ggsave(
  filename = "images/improved_1y_3y.svg",
  width = 10, height = 8
)

# from_3y_to_1y <- 
#   (fig_brg_improved_3y + ylim(-15, 10) | fig_brg_improved_1y + ylim(-15, 10)) /
#   (fig_osl_improved_3y + ylim(-15, 10) | fig_osl_improved_1y + ylim(-15, 10)) /
#   (fig_nj_chained_3y +   ylim(-15, 10) | fig_nj_chained_1y +   ylim(-15, 10))
