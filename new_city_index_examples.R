# Calculate city index using new method

# New concepts:
# - Using only MDT (normal situation, seasonally adjusted)
# - Traffic work weights (need traffic links)
# - Chaining when necessary (need to suitably subdivide index period, possibly one road net version per subperiod)
# - Estimate confidence interval (compare and decide which method to use)
# - Measures of representativity

# To be included later:
# - Seasonally adjusted MDT
# - Vehicle classification by type, not just length

# Resolution in time:
# - Month by month
# - So far this year
# - Last 12 months
# - Last 24 months
# - Last 36 months

# Resolution in day type
# - working days
# - non-working days

# Resolution in vehicle type:
# - light (short)
# - heavy (long)
# - all

# How to compare:
# - new versus old index results in different time resolutions, separated from new chaining strategies
# - the impact of new chaining strategies leading to better representativity


# What are isolated improvements?
# - measures of representativity, especially traffic work
# - smaller (?) confidence interval, mostly (?) due to finiteness of population
# - traffic work weights

# Suitable cities as examples:
# - Bergen (chaining might be useful)
# - Buskerudbyen (has had representativity issues, no chaining?)
# - Trondheim (representativity issues, no chaining?)
# - Nord-Jæren (with new chaining strategy)


{
  base::Sys.setlocale(locale = "nb.utf8")
  svv_background_color <- "#F5F5F5"

  library(tidyverse)

  source("H:/Programmering/R/byindeks/index_report_functions.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  source("get_from_trafficdata_api.R")
  source("road_events_functions.R")
  source("traffic_link_functions.R")
}

# Bergen
# Traffic links 2024
# We do not have any earlier links
# Use the MDTs we already have

