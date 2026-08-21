# Toll station info from NVDB
bomstasjoner_nj <- 
  readr::read_rds("bomdata_nj/bomstasjoner_nj.rds") |> 
  dplyr::filter(
    # Remove Gausel bussvei, though traffic from this still is summed with the other Gausel toll station. 
    # This removal for listing and mapping purposes, and to avoid this data being counted twice.
    !(nvdb_id %in% c("1026491363")),
    # Skipping stations on Buøy and Hundvåg
    trp_id < 800
  )
