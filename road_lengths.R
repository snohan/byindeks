# Fetching road lengths for municipalities

source("get_from_trafficdata_api.R")
source("get_from_nvdb_api.R")

# Municipalities ####
municipalities <- get_municipalities()

# Bergen ####
bergen <- get_road_length_for_municipality(4601)
alver <- get_road_length_for_municipality(4631)
askoy <- get_road_length_for_municipality(4627)
bjornafjorden <- get_road_length_for_municipality(4624)

bergen_all <- dplyr::bind_rows(bergen,
                               alver,
                               askoy,
                               bjornafjorden) %>%
  dplyr::left_join(municipalities)

write.csv2(bergen_all, file = "road_lengths/bergen_road_lengths.csv",
           row.names = FALSE)
