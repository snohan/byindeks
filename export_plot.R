library(svglite)

# Save last plot as SVG
ggplot2::ggsave(
  # filename = "images/nj_chain_graph.svg",
  plot = trp_mdt_plot_36,
  filename = "images/grenland.svg",
  # filename = "images/mdt_pattern_test.svg",
  width = 8, height = 5
)


# Save maps
# Can not be used in PowerPoint
htmlwidgets::saveWidget(
  the_map,
  file = "images/trd_trp_map.html"
)


# Mapshot
library(mapview)
mapview::mapshot2(
  the_map,
  file = "images/trd_trp_map.png"
)

# Put toghether
ggplot2::ggsave(
  filename = "images/improved_1y_3y.svg",
  width = 10, height = 8
)

ggplot2::ggsave(
  filename = "images/improved_1y_3y.png",
  width = 10, height = 8
)
