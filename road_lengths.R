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
alver <- get_road_length_for_municipality(4631)
askoy <- get_road_length_for_municipality(4627)
bjornafjorden <- get_road_length_for_municipality(4624)

bergen_all <-
  dplyr::bind_rows(
    bergen,
    alver,
    askoy,
    bjornafjorden
  ) |>
  dplyr::left_join(municipalities)

write.csv2(bergen_all, file = "road_lengths/road_lengths_8952.csv",
           row.names = FALSE)


# Buskerudbyen ####
lier <- get_road_length_for_municipality(3312)
drammen <- get_road_length_for_municipality(3301)
eiker <- get_road_length_for_municipality(3314)
kongsberg <- get_road_length_for_municipality(3303)

buskerudbyen <-
  dplyr::bind_rows(
    lier,
    drammen,
    eiker,
    kongsberg
  ) |>
  dplyr::left_join(municipalities)

write.csv2(buskerudbyen, file = "road_lengths/road_lengths_1952.csv",
           row.names = FALSE)

# Grenland ####
siljan <- get_road_length_for_municipality(4010)
skien <- get_road_length_for_municipality(4003)
porsgrunn <- get_road_length_for_municipality(4001)

grenland <-
  dplyr::bind_rows(
    siljan,
    porsgrunn,
    skien
  ) |>
  dplyr::left_join(municipalities)

write.csv2(grenland, file = "road_lengths/road_lengths_955.csv",
           row.names = FALSE)

# Kristiansand ####
kristiansand <- get_road_length_for_municipality(4204)
vennesla <- get_road_length_for_municipality(4223)

krs_all <- dplyr::bind_rows(kristiansand,
                            vennesla) %>%
  dplyr::left_join(municipalities)

write.csv2(krs_all, file = "road_lengths/road_lengths_957.csv",
           row.names = FALSE)

# Kristiansandsregionen ####
kristiansand <- get_road_length_for_municipality(4204)
vennesla <- get_road_length_for_municipality(4223)
lillesand <- get_road_length_for_municipality(4215)
birkenes <- get_road_length_for_municipality(4216)
iveland <- get_road_length_for_municipality(4218)

krs_reg <-
  dplyr::bind_rows(
    kristiansand,
    vennesla,
    lillesand,
    birkenes,
    iveland
  ) |>
  dplyr::left_join(municipalities, by = join_by(municipality_number))

write.csv2(krs_reg, file = "road_lengths/road_lengths_19953.csv", row.names = FALSE)

# Nedre Glomma ####
fredrikstad <- get_road_length_for_municipality(3107)
sarpsborg <- get_road_length_for_municipality(3105)

glomma <- dplyr::bind_rows(fredrikstad,
                             sarpsborg) %>%
  dplyr::left_join(municipalities)

write.csv2(glomma, file = "road_lengths/road_lengths_953.csv",
           row.names = FALSE)


# Nord-Jæren ####
stavanger <- get_road_length_for_municipality(1103)
sandnes <- get_road_length_for_municipality(1108)
sola <- get_road_length_for_municipality(1124)
randaberg <- get_road_length_for_municipality(1127)

nord_jaeren_all <-
  dplyr::bind_rows(
    stavanger,
    sandnes,
    sola,
    randaberg
  ) |>
  dplyr::left_join(municipalities, by = join_by(municipality_number))

write.csv2(
  nord_jaeren_all,
  file = "road_lengths/road_lengths_952.csv",
  row.names = FALSE
)


# Oslo og Akershus ####
oslo <- get_road_length_for_municipality(301)
barum <- get_road_length_for_municipality(3201)
nordre_follo <- get_road_length_for_municipality(3207)
lillestrom <- get_road_length_for_municipality(3205)
ullensaker <- get_road_length_for_municipality(3209)
as <- get_road_length_for_municipality(3218)
ralingen <- get_road_length_for_municipality(3224)
enebakk <- get_road_length_for_municipality(3220)
vestby <- get_road_length_for_municipality(3216)
frogn <- get_road_length_for_municipality(3214)
nesodden <- get_road_length_for_municipality(3212)
asker <- get_road_length_for_municipality(3203)
aurskog <- get_road_length_for_municipality(3226)
lorenskog <- get_road_length_for_municipality(3222)
nittedal <- get_road_length_for_municipality(3232)
gjerdrum <- get_road_length_for_municipality(3230)
nannestad <- get_road_length_for_municipality(3238)
nes <- get_road_length_for_municipality(3228)
eidsvoll <- get_road_length_for_municipality(3240)
hurdal <- get_road_length_for_municipality(3242)

oslo_all <-
  dplyr::bind_rows(
    oslo,
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
    hurdal
  ) |>
  dplyr::left_join(municipalities)

write.csv2(oslo_all, file = "road_lengths/road_lengths_959.csv",
           row.names = FALSE)


# Trondheim ####
trondheim <- get_road_length_for_municipality(5001)
melhus <- get_road_length_for_municipality(5028)
malvik <- get_road_length_for_municipality(5031)
stjordal <- get_road_length_for_municipality(5035)
skaun  <- get_road_length_for_municipality(5029)
orkland  <- get_road_length_for_municipality(5059)

trondheim_all <-
  dplyr::bind_rows(
    trondheim,
    melhus,
    malvik,
    stjordal,
    skaun,
    orkland
  ) |>
  dplyr::left_join(municipalities)

write.csv2(trondheim_all, file = "road_lengths/road_lengths_960.csv",
           row.names = FALSE)


# Tromsø ####
tromso <- get_road_length_for_municipality(5501) %>%
  dplyr::left_join(municipalities)

write.csv2(tromso, file = "road_lengths/road_lengths_961.csv",
           row.names = FALSE)



