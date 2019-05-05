# Samler all skripting for å lage grunnlaget for rapporten her.

# Library ####
library(tidyverse)
library(httr)
library(jsonlite)

# NVDB API calls ####
source("funksjoner_hent_nvdb_info.R")

# Get point information ####
# Fetching index traffic registration points (TRPs).
# TODO: From a manually edited CSV-file.
# TODO: Get metadata from Trafficdata-API.

# Get all TRS in the municipalities from NVDB
# Trondheim 5001
kommunenr <- "5001"

kommunenavn <- hent_kommune(kommunenr)[[1]]

kommune_trser_5001 <-
  hent_trafikkregistreringsstasjon_for_omraade(kommunenr) %>%
  mutate(Type = "TRS")

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

kommunepunkter <- bind_rows(kommune_trser_5001, kommune_bomer, bom_52) %>%
  mutate(Vegreferanse = str_sub(Vegreferanse, 5))

# The points chosen for the index ####
byindekspunkter_valgte <- read.csv2("byindekspunkter_vedtatte.csv") %>%
  dplyr::filter(city_area_name == "Trondheim") %>%
  mutate(Stasjonnr = as.character(legacyNortrafMpn)) %>%
  select(Stasjonnr)

# ADT ####
# TODO: Get AADT with coverage for the index TRPs.
adt <- read.csv2("adt_2017_nortraf.csv") %>%
  filter(Felt == "R0") %>%
  mutate(Stasjonnr = as.character(Tellepunkt),
         ADT = round(ADT, digits = -2)) %>%
  select(Stasjonnr, ADT)

# Read index results from CSV-files ####
indekstall <-
  read.csv2("punktindeks_trondheim_alle_punkter_jan-des18.csv") %>%
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

# Final table ####
indekspunktene <- byindekspunkter_valgte %>%
  left_join(kommunepunkter) %>%
  left_join(adt) %>%
  left_join(indekstall)

  write.csv2(indekspunktene, file = "indekspunktene_trondheim_2018.csv",
           row.names = F)


# End