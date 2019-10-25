# Funksjoner for å hente nødvendig info fra NVDB-API v2.

library(httr)

# Definer URI og sti ####
nvdb_url <- "https://www.vegvesen.no/nvdb/api/v2"
sti_vegobjekter <- "/vegobjekter"

# Hent trafikkregistreringsstasjoner i en kommune
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

# Deprecated
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

get_tolling_stations <- function(kommunenr) {
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
    mutate(Kommune = kommunenavn,
           kortform = str_sub(kortform, 6))

  colnames(bomer) <- c("msnr", "name", "road_reference",
                       "lat", "lon", "road", "kommune")

  return(bomer)
}

getAadtByRoadReference <- function(roadref) {
  api_query_540 <- paste0(nvdb_url,
                         sti_vegobjekter,
                         "/540",
                         "?inkluder=egenskaper")

  api_query_540_vegref <- paste0(api_query_540,
                                 "&vegreferanse=",
                                 roadref)

  respons <- GET(api_query_540_vegref,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  adt_total <- uthenta$objekter %>%
    select(id, egenskaper) %>%
    unnest() %>%
    filter(id1 %in% c(4623)) %>%
    select(verdi)

  adt_verdi <- round(as.numeric(adt_total[1, 1]), digits = -2)

  return(adt_verdi)
}

getAadtByRoadlinkposition <- function(roadlink) {
  api_query_540 <- paste0(nvdb_url,
                          sti_vegobjekter,
                          "/540",
                          "?inkluder=egenskaper")

  api_query_540_vegref <- paste0(api_query_540,
                                 "&veglenke=",
                                 roadlink)

  respons <- GET(api_query_540_vegref,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  adt_total <- uthenta$objekter %>%
    select(id, egenskaper) %>%
    rename(id1 = id) %>%
    unnest(cols = c(egenskaper)) %>%
    filter(id %in% c(4623)) %>%
    select(verdi)

  adt_verdi <- round(as.numeric(adt_total[1, 1]), digits = -2)

  return(adt_verdi)
}

roadlink <- "0.26634@181322"
#roadref <- "1200EV39hp74m14171"

getSpeedLimit <- function(roadref) {
  api_query_105 <- paste0(nvdb_url,
                          sti_vegobjekter,
                          "/105",
                          "?inkluder=egenskaper")

  api_query_105_vegref <- paste0(api_query_105,
                                 "&vegreferanse=",
                                 roadref)

  respons <- GET(api_query_105_vegref,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  speed_limit <- uthenta$objekter %>%
    select(id, egenskaper) %>%
    unnest() %>%
    filter(id1 %in% c(2021)) %>%
    select(verdi)

  verdi <- speed_limit[1, 1]

  return(verdi)
}

#roadlink <- "0.38722@2037772"

getSpeedLimit_roadlink <- function(roadlink) {
  api_query_105 <- paste0(nvdb_url,
                          sti_vegobjekter,
                          "/105",
                          "?inkluder=egenskaper")

  api_query_105_vegref <- paste0(api_query_105,
                                 "&veglenke=",
                                 roadlink)

  respons <- GET(api_query_105_vegref,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  objekter <- uthenta$objekter

  if(length(objekter) == 0) {
    verdi = NA
  }else{
    speed_limit <- objekter %>%
      select(id, egenskaper) %>%
      # If more than one response, choosing the first (lazily avoiding
      # unnest to crash). Not sure what would be the right response to choose.
      slice(1) %>%
      unnest() %>%
      filter(id1 %in% c(2021)) %>%
      select(verdi)

    verdi <- speed_limit[1, 1]
  }

  return(verdi)
}
