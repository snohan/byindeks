# Gathering info on all points and indexes

# Setup ####

# Packages are to be loaded through sourcing rmd_setup.R in the Rmd report file.
source("rmd_setup.R")

# Traffic Data API calls to get points metadata and aadt
source("get_from_trafficdata_api.R")

# If necessary, get all TRPs from TRP API
# TRPs without commissions are not i TD-API!
source("get_from_trp_api.R")

# NVDB API calls to get tolling stations
source("get_from_nvdb_api.R")

# Points ####

# Points used in each city
cities_points <- read_csv2("data_points_raw/cities_points.csv")
cities_points_unestablished <-
  read_csv2("data_points_raw/points_unestablished.csv")

# All points from Traffic Data API
points <- getPoints()

# All points from TRP API (if needed)
points_trp <- getPointsFromTRPAPI()

# Oslo og Akershus 2019 ####
oslopunkter <- cities_points %>%
  dplyr::filter(city_area_name == "Oslo og Akershus",
                agreement_start == 2019) %>%
  dplyr::mutate(established = "Ja" )

# Adding metadata
indekspunkter_oslo <- dplyr::left_join(oslopunkter, points) %>%
  dplyr::select(1:5, 7:11, 6)

# Legger inn uetablerte punkter
indekspunkter_oslo_uetablerte <-
  read.csv2("points_not_yet_established.csv") %>%
  dplyr::filter(city_area_name == "Oslo og Akershus",
                agreement_start == 2019) %>%
  dplyr::mutate(established = "Nei" )

indekspunktene_oslo <- bind_rows(indekspunkter_oslo,
                                 indekspunkter_oslo_uetablerte)

write.csv2(indekspunktene_oslo,
           file = "data_indexpoints_tidy/indekspunktene_oslo_2019.csv",
           row.names = F)


# ADT
oslo_adt <- getAdtForpoints(indekspunktene_oslo$trp_id)
# TODO: filtrere ut 2018
# TODO: join
# TODO: hente fra NVDB de som mangler?

# Test
#uten_adt <- getTrpAadt("32135V604101")


# Trondheim 2017 ####
trp_trondheim_2017_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Trondheim",
                agreement_start == 2017)

# Adding metadata
# Bruker trp fra trp-aåi, da noen mangler igangsetting.
trp_trondheim_2017 <- dplyr::left_join(trp_trondheim_2017_ids, points_trp) %>%
  dplyr::select(1:5, 7:11, 6) %>%
# Må ta vekk ene punktet i Strindheimtunnelen
  dplyr::filter(trp_id != "21571V2394246")

# Bompunkter i Trondheim
kommunenr <- "5001"
kommunenavn <- hent_kommune(kommunenr)[[1]]

# kommune_trser_5001 <-
#   hent_trafikkregistreringsstasjon_for_omraade(kommunenr) %>%
#   mutate(Type = "TRS")

kommune_bomer <-
  hent_bomstasjon_for_kommune(kommunenr) %>%
  mutate(Type = "Bom")

# Må legge til for 52 som er borte fra API-et.
bom_52 <- data.frame(
  "Stasjonnr" = "52",
  "Navn" = "Klett - E6, S-snitt",
  "Vegreferanse" = "5000 Ev6 hp9 m1252",
  "lat" = 63.32590,
  "lon" = 10.32702,
  "Veg" = "E6",
  "Kommune" = "Trondheim",
  "Type" = "Bom")

# HIT
# TODO: endre til td-api-format på tabellen
kommunepunkter <- bind_rows(kommune_trser_5001, kommune_bomer, bom_52) %>%
  mutate(Vegreferanse = str_sub(Vegreferanse, 5))

# The points chosen for the index
byindekspunkter_valgte <- read.csv2("byindekspunkter_vedtatte.csv") %>%
  dplyr::filter(city_area_name == "Trondheim") %>%
  mutate(Stasjonnr = as.character(legacyNortrafMpn)) %>%
  select(Stasjonnr)

# ADT
# TODO: Get AADT for reference year with coverage from TD-API.
adt <- read.csv2("adt_2017_nortraf.csv") %>%
  filter(Felt == "R0") %>%
  mutate(Stasjonnr = as.character(Tellepunkt),
         ADT = round(ADT, digits = -2)) %>%
  select(Stasjonnr, ADT)

# Read index results from CSV-files
indekstall <-
  read.csv2("data_index_raw/punktindeks_trondheim_alle_punkter_jan-des18.csv") %>%
  mutate(trs = as.numeric(msnr),
         trs = if_else(trs > 9916000, trs - 9916000, trs)) %>%
  select(-msnr) %>%
  group_by(trs) %>%
  summarise(trafikkmengde_basisaar = sum(trafikkmengde.basisaar),
            trafikkmengde_indeksaar = sum(trafikkmengde.indeksaar),
            Indeks = round((trafikkmengde_indeksaar/
                              trafikkmengde_basisaar - 1) * 100,
                           digits = 1)) %>%
  mutate(trs = as.character(trs)) %>%
  rename(Stasjonnr = trs) %>%
  select(Stasjonnr, Indeks)

# Final table
indekspunktene <- byindekspunkter_valgte %>%
  left_join(kommunepunkter) %>%
  left_join(adt) %>%
  left_join(indekstall)

write.csv2(indekspunktene,
           file = "data_indexpoints_tidy/indekspunktene_trondheim_2018.csv",
           row.names = F)


