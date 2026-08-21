# Check CMDT link index in map
library(leaflet)
source("H:/Programmering/R/byindeks/leaflet_nvdb_map_setup.R")


# Monthly check ----
# Table each month
month_to_check <-
  link_index_month_2 |> 
  dplyr::filter(
    universal_year_period_id == 99
  ) |> 
  dplyr::left_join(
    trp_info,
    by = "trp_id"
  ) |> 
  dplyr::select(
    trp_id, name, road_category_and_number, year_a, year_b, month, mdt_delta, p_abi_p, index_p
  ) |> 
  dplyr::arrange(p_abi_p)

# Map each month
map_link_index(month_to_check, 10)


# TRP time series ----
trps <- link_index_month_1$trp_id |> unique()
base::length(trps)
trps[53]

visualize_trp_cmdt_index(link_index_month_1, 54)



