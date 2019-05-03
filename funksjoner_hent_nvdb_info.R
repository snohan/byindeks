# Funksjoner for å hente nødvendig info fra NVDB-API.
# Bruker v2.
# Erstattes av Trafikkdata-API for henting av ÅDT og punkter.


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

  trs <- uthenta$objekter %>%
    select(id, egenskaper) %>%
    unnest() %>%
    filter(id1 %in% c(4626, 4627, 5201, 9293, 3910)) %>%
    select(id, navn, verdi) %>%
    spread(navn, verdi)

  vegreferanser <- uthenta$objekter %>%
    select(id, lokasjon.vegreferanser) %>%
    unnest()

  # TODO: Ta bort fylkenr, kommunenr og mellomrom, slik at vegreferansen er på kortform
  # som kan brukes direkte i spørring etter trafikkmengde i NVDB-API.

  koordinater <- uthenta$objekter %>%
    select(id, lokasjon.geometri.wkt) %>%
    unnest() %>%
    mutate(geometri_sub = str_sub(lokasjon.geometri.wkt, 10, -2)) %>%
    separate(geometri_sub, into = c("lat", "lon", "alt"), sep = "[[:space:]]",
             convert = T) %>%
    select(id, lat, lon)

  trser <- trs %>%
    left_join(vegreferanser) %>%
    left_join(koordinater) %>%
    filter(Trafikantgruppe == "Motorkjøretøy") %>%
    filter(Status != "Nedlagt, ikke lov å bruke") %>%
    mutate(Veg = paste0(kategori, nummer)) %>%
    select(2, 5, 14:17) %>%
    mutate(Kommune = kommunenavn)

  colnames(trser) <- c("Stasjonnr", "Navn", "Vegreferanse",
                       "lat", "lon", "Veg", "Kommune")

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

  bom <- uthenta$objekter %>%
    select(id, egenskaper) %>%
    unnest() %>%
    filter(id1 %in% c(1078, 9595)) %>%
    select(id, navn, verdi) %>%
    spread(navn, verdi)

  vegreferanser <- uthenta$objekter %>%
    select(id, lokasjon.vegreferanser) %>%
    unnest()

  koordinater <- uthenta$objekter %>%
    select(id, lokasjon.geometri.wkt) %>%
    unnest() %>%
    mutate(geometri_sub = str_sub(lokasjon.geometri.wkt, 10, -2)) %>%
    separate(geometri_sub, into = c("lat", "lon", "alt"), sep = "[[:space:]]",
             convert = T) %>%
    select(id, lat, lon)

  bomer <- bom %>%
    left_join(vegreferanser) %>%
    left_join(koordinater) %>%
    mutate(Veg = paste0(kategori, nummer)) %>%
    select(-id, -fylke, -kommune, -kategori, -nummer,
           -status, -hp, -meter) %>%
    mutate(Kommune = kommunenavn)

  colnames(bomer) <- c("Stasjonnr", "Navn", "Vegreferanse",
                       "lat", "lon", "Veg", "Kommune")

  return(bomer)
}
