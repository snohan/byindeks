# Funksjoner for å hente nødvendig info fra NVDB

# Definer URI og sti ####
nvdb_url <- "https://www.vegvesen.no/nvdb/api/v2"
sti_vegobjekter <- "/vegobjekter"

# Bygger spørring etter trafikkregistreringsstasjoner per kommune

hent_kommune <- function(kommunenr) {
  api_query_536 <- paste0(nvdb_url,
                          sti_vegobjekter,
                          "/536",
                          "?inkluder=egenskaper,lokasjon")

  api_query_536_kommune <- paste0(api_query_536,
                                  "&kommune=",
                                  kommunenr,
                                  "&srid=wgs84")

  respons <- GET(api_query_536_kommune,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  kommune <- bind_rows(uthenta$objekter$egenskaper, .id = "kid") %>%
    filter(id %in% c(4585))

  kommunenavn <- kommune$verdi

  polygon <- uthenta$objekter$lokasjon.geometri.wkt

  svar <- list(kommunenavn, polygon)

  return(svar)
}

hent_trafikkregistreringsstasjon_for_omraade <- function(omraadenr) {
  # Fire siffer angir kommunenr.
  # Ett eller to siffer angir fylkenr.

  api_query_482 <- paste0(nvdb_url,
                          sti_vegobjekter,
                          "/482",
                          "?inkluder=egenskaper,lokasjon")

  omraadestreng <- ifelse(nchar(omraadenr) <= 2,
                          "&fylke=",
                          "&kommune="
  )

  api_query_482_omraade <- paste0(api_query_482,
                                  omraadestreng,
                                  omraadenr,
                                  "&srid=wgs84",
                                  "&egenskap='3910=4892'")
  # Over: Filtrerer på kontinuerlige stasjoner

  respons <- GET(api_query_482_omraade,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  trs <- bind_rows(uthenta$objekter$egenskaper, .id = "trsid") %>%
    filter(id %in% c(4626, 4627, 5201, 9293, 3910)) %>%
    select(navn, verdi) %>%
    mutate(navn = as.factor(navn)) %>%
    group_by(navn) %>%
    mutate(grouped_id = row_number()) %>%
    spread(navn, verdi, drop = F) %>%
    select(-grouped_id) %>%
    select(1, 4, 3, 2, 5)

  colnames(trs) <- c("Stasjonnr", "Navn", "Status", "Nivaa", "Trafikantgruppe")

  vegreferanse <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                            .id = "trser") %>%
    select(-trser)
  # Ta bort fylkenr, kommunenr og mellomrom, slik at vegreferansen er på kortform
  # som kan brukes direkte i spørring etter trafikkmengde i NVDB-API.

  koordinater <- as.data.frame(uthenta$objekter$lokasjon.geometri.wkt) %>%
    mutate(geometri_sub =
             str_sub(uthenta$objekter$lokasjon.geometri.wkt, 10, -2)) %>%
    separate(geometri_sub, into = c("lat", "lon", "alt"), sep = "[[:space:]]",
             convert = T) %>%
    select(-1)

  trser <- bind_cols(trs, vegreferanse, koordinater) %>%
    #filter(Nivaa == "Kontinuerlig (Nivå 1)") %>%
    filter(Trafikantgruppe == "Motorkjøretøy") %>%
    filter(Status != "Nedlagt, ikke lov å bruke") %>%
    mutate(Veg = paste0(kategori, nummer)) %>%
    select(-Status, -Nivaa, -Trafikantgruppe, -fylke, -kommune, -kategori, -nummer,
           -status, -hp, -meter) %>%
    mutate(Kommune = kommunenavn)

  colnames(trser) <- c("Stasjonnr", "Navn", "Vegreferanse",
                       "lat", "lon", "alt", "Veg", "Kommune")

  return(trser)
}

hent_bomstasjon_for_kommune <- function(kommunenr) {
  # Laget for Trondheim
  api_query_45 <- paste0(nvdb_url,
                         sti_vegobjekter,
                         "/45",
                         "?inkluder=egenskaper,lokasjon")

  api_query_45_kommune <- paste0(api_query_45,
                                 "&kommune=",
                                 kommunenr,
                                 "&srid=wgs84")

  respons <- GET(api_query_45_kommune,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  bom <- bind_rows(uthenta$objekter$egenskaper, .id = "bomid") %>%
    filter(id %in% c(1078, 9595)) %>%
    select(navn, verdi) %>%
    filter(verdi != "Ferje Flakk-Rørvik") %>%
    group_by(navn) %>%
    mutate(grouped_id = row_number()) %>%
    spread(navn, verdi) %>%
    select(-grouped_id)

  vegreferanse <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                            .id = "bomer") %>%
    filter(nummer != 715) %>%
    select(-bomer)

  koordinater <- as.data.frame(uthenta$objekter$lokasjon.geometri.wkt) %>%
    mutate(geometri_sub = str_sub(uthenta$objekter$lokasjon.geometri.wkt, 10, -2)) %>%
    separate(geometri_sub, into = c("lat", "lon", "alt"), sep = "[[:space:]]",
             convert = T) %>%
    select(-1) %>%
    filter(alt != 8.79280)

  bomer <- bind_cols(bom, vegreferanse, koordinater) %>%
    mutate(Veg = paste0(kategori, nummer)) %>%
    select(-fylke, -kommune, -kategori, -nummer,
           -status, -hp, -meter) %>%
    mutate(Kommune = kommunenavn)

  colnames(bomer) <- c("Stasjonnr", "Navn", "Vegreferanse",
                       "lat", "lon", "alt", "Veg", "Kommune")

  return(bomer)
}