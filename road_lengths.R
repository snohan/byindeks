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

# Oslo og Akershus ####
oslo <- get_road_length_for_municipality(301)
barum <- get_road_length_for_municipality(3024)
nordre_follo <- get_road_length_for_municipality(3020)
lillestrom <- get_road_length_for_municipality(3030)
ullensaker <- get_road_length_for_municipality(3033)
as <- get_road_length_for_municipality(3021)
ralingen <- get_road_length_for_municipality(3027)
enebakk <- get_road_length_for_municipality(3028)
vestby <- get_road_length_for_municipality(3019)
frogn <- get_road_length_for_municipality(3022)
nesodden <- get_road_length_for_municipality(3023)
asker <- get_road_length_for_municipality(3025)
aurskog <- get_road_length_for_municipality(3026)
lorenskog <- get_road_length_for_municipality(3029)
nittedal <- get_road_length_for_municipality(3031)
gjerdrum <- get_road_length_for_municipality(3032)
nannestad <- get_road_length_for_municipality(3036)
nes <- get_road_length_for_municipality(3034)
eidsvoll <- get_road_length_for_municipality(3035)
hurdal <- get_road_length_for_municipality(3037)

oslo_all <- dplyr::bind_rows(oslo,
                             barum,
                             nordre_follo,
                             lillestrom,
                             ullensaker,
                             as,
                             ralingen,
                             enebakk,
                             vestby,
                             frogn,
                             nesodden,
                             asker,
                             aurskog,
                             lorenskog,
                             nittedal,
                             gjerdrum,
                             nannestad,
                             nes,
                             eidsvoll,
                             hurdal) %>%
  dplyr::left_join(municipalities)

write.csv2(oslo_all, file = "road_lengths/oslo_road_lengths.csv",
           row.names = FALSE)

# Nord-Jæren ####
