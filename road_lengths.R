# Fetching road lengths for municipalities

source("get_from_trafficdata_api.R")
source("get_from_nvdb_api.R")

# Municipalities ####
municipalities <- get_municipalities()

# TODO: make a function that takes a list of municipality numbers
# TODO: add column with relative lengths in rmd file or pie chart
# TODO: make comment about traffic work
# TODO: add traffic work
# TODO: use traffic links
# TODO: use user defined polygon

# Bergen ####
bergen <- get_road_length_for_municipality(4601)
#alver <- get_road_length_for_municipality(4631)
#askoy <- get_road_length_for_municipality(4627)
#bjornafjorden <- get_road_length_for_municipality(4624)

bergen_all <- dplyr::bind_rows(bergen#,
#                               alver,
#                               askoy,
#                               bjornafjorden
) %>%
  dplyr::left_join(municipalities)

write.csv2(bergen_all, file = "road_lengths/bergen_road_lengths.csv",
           row.names = FALSE)


# Buskerudbyen ####
lier <- get_road_length_for_municipality(3049)
drammen <- get_road_length_for_municipality(3005)
eiker <- get_road_length_for_municipality(3048)
kongsberg <- get_road_length_for_municipality(3006)

buskerudbyen <- dplyr::bind_rows(lier,
                                 drammen,
                                 eiker,
                                 kongsberg) %>%
  dplyr::left_join(municipalities)

write.csv2(buskerudbyen, file = "road_lengths/buskerudbyen_road_lengths.csv",
           row.names = FALSE)

# Grenland ####
siljan <- get_road_length_for_municipality(3812)
skien <- get_road_length_for_municipality(3807)
porsgrunn <- get_road_length_for_municipality(3806)

grenland <- dplyr::bind_rows(siljan,
                             porsgrunn,
                             skien) %>%
  dplyr::left_join(municipalities)

write.csv2(grenland, file = "road_lengths/grenland_road_lengths.csv",
           row.names = FALSE)

# Kristiansand ####
kristiansand <- get_road_length_for_municipality(4204)
vennesla <- get_road_length_for_municipality(4223)

krs_all <- dplyr::bind_rows(kristiansand,
                            vennesla) %>%
  dplyr::left_join(municipalities)

write.csv2(krs_all, file = "road_lengths/kristiansand_road_lengths.csv",
           row.names = FALSE)


# Nedre Glomma ####
fredrikstad <- get_road_length_for_municipality(3004)
sarpsborg <- get_road_length_for_municipality(3003)

glomma <- dplyr::bind_rows(fredrikstad,
                             sarpsborg) %>%
  dplyr::left_join(municipalities)

write.csv2(glomma, file = "road_lengths/glomma_road_lengths.csv",
           row.names = FALSE)



# Nord-Jæren ####

stavanger <- get_road_length_for_municipality(1103)
sandnes <- get_road_length_for_municipality(1108)
sola <- get_road_length_for_municipality(1124)
randaberg <- get_road_length_for_municipality(1127)

nord_jaeren_all <- dplyr::bind_rows(stavanger,
                               sandnes,
                               sola,
                               randaberg) %>%
  dplyr::left_join(municipalities)

write.csv2(nord_jaeren_all, file = "road_lengths/nord-jaeren_road_lengths.csv",
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




# Trondheim ####
trondheim <- get_road_length_for_municipality(5001)
melhus <- get_road_length_for_municipality(5028)
malvik <- get_road_length_for_municipality(5031)
stjordal <- get_road_length_for_municipality(5035)

trondheim_all <- dplyr::bind_rows(trondheim,
                                  melhus,
                                  malvik,
                                  stjordal) %>%
  dplyr::left_join(municipalities)

write.csv2(trondheim_all, file = "road_lengths/trondheim_road_lengths.csv",
           row.names = FALSE)


# Tromsø ####
tromso <- get_road_length_for_municipality(5401) %>%
  dplyr::left_join(municipalities)

write.csv2(tromso, file = "road_lengths/tromso_road_lengths.csv",
           row.names = FALSE)



