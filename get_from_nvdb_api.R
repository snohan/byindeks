# Funksjoner for å hente nødvendig info fra NVDB-API v3.

library(tidyverse)
library(httr)
library(jsonlite)
library(sf)

# Definer URI og sti ----
nvdb_url <- "https://www.vegvesen.no/nvdb/api/v2"
nvdb_url_v3 <- "https://nvdbapiles-v3.atlas.vegvesen.no"
sti_vegobjekter <- "/vegobjekter"
sti_veg <- "/veg"
sti_posisjon <- "/posisjon"

nvdb_v3_headers <- c(
  "X-Client" = "trafikkdatagruppa",
  "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
  "Accept" = "application/vnd.vegvesen.nvdb-v3-rev1+json"
)


# Vegnett ----
#vegsystemreferanse <- "KV6064S1D1m30"
#kommunenr <- "5001"
# Hent stedfesting i punkt på vegnettet
hent_vegpunkt <- function(vegsystemreferanse, kommunenr) {
  api_query <- paste0(nvdb_url_v3,
                      sti_veg,
                      "?vegsystemreferanse=",
                      vegsystemreferanse,
                      "&kommune=",
                      kommunenr)

  respons <- httr::GET(api_query,
                 httr::add_headers(.headers = nvdb_v3_headers))

  uthenta <- jsonlite::fromJSON(
    stringr::str_conv(
    respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  kommune <- bind_rows(uthenta$objekter$egenskaper, .id = "kid")
}


hent_veglenkesekvens <- function(veglenkesekvens_id) {

  api_query <- paste0(nvdb_url_v3,
                      "/vegnett",
                      "/veglenkesekvenser/",
                      veglenkesekvens_id)

  respons <- httr::GET(api_query,
                       httr::add_headers(.headers = nvdb_v3_headers))

  uthenta <- jsonlite::fromJSON(
    stringr::str_conv(
      respons$content, encoding = "UTF-8"),
    simplifyDataFrame = T,
    flatten = T)

  resultat <- dplyr::bind_rows(uthenta$veglenker) %>%
    dplyr::arrange(veglenkenummer) %>%
    dplyr::select(veglenkenummer, type, startposisjon, sluttposisjon, lengde,
                  'detaljnivå', typeVeg, feltoversikt, startdato,
                  geometri.wkt, geometri.srid, geometri.kommune, geometri.lengde) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(feltoversikt = stringr::str_c(feltoversikt, collapse = ", "))

  return(resultat)
}



#veglenkeposisjon <- "0.25636836@423823"
# Hent stedfesting i punkt på vegnettet
hent_vegsystemreferanse <- function(veglenkeposisjon) {
  api_query <- paste0(nvdb_url_v3,
                      sti_veg,
                      "?veglenkesekvens=",
                      veglenkeposisjon)

  respons <- GET(api_query,
                 httr::add_headers(.headers = nvdb_v3_headers))

  uthenta <- jsonlite::fromJSON(
    stringr::str_conv(
      respons$content, encoding = "UTF-8"),
    simplifyDataFrame = T,
    flatten = T)

  vegsystemreferanse <- uthenta$vegsystemreferanse$kortform

  result <- dplyr::if_else(is.null(vegsystemreferanse), "", vegsystemreferanse)

  return(result)
}


hent_vegpunkt_via_latlon <- function(lat, lon) {

  api_query <- paste0(nvdb_url_v3,
                      sti_posisjon,
                      "?lat=",
                      lat,
                      "&lon=",
                      lon,
                      "&maks_avstand=60&maks_antall=6&konnekteringslenker=false&detaljerte_lenker=false&vegsystemreferanse=E,R,F,K&srid=4326")

  respons <- httr::GET(api_query,
                       httr::add_headers(.headers = nvdb_v3_headers))

  uthenta <- jsonlite::fromJSON(
    stringr::str_conv(
      respons$content, encoding = "UTF-8"),
    simplifyDataFrame = T,
    flatten = T)

  # TODO: allow more responses, filter by traffic_Type and choose closest remaining

  return(uthenta)
}

# trp_df <- data.frame(row_id = "58.44_6.02",
#                     lat = "58.4401",
#                     lon = "6.611268")

hent_vegpunkt_via_latlon_for_flere_punkter <- function(trp_df) {

  # Takes a trp_list with columns named 'lat' and 'lon' and 'row_id' (for rejoining)
  number_of_points <- nrow(trp_df)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {

    new_data_point <- data.frame(
      row_id = trp_df$row_id[trp_count],
      hent_vegpunkt_via_latlon(trp_df$lat[trp_count],
                               trp_df$lon[trp_count])
    )

    if (ncol(new_data_point) < 10) {
      data_points <- data_points
    } else {
      data_points <- bind_rows(data_points, new_data_point)
    }

    trp_count <- trp_count + 1
  }

  trp_data <- data_points %>%
    dplyr::select(site_id = row_id,
                  road_reference = vegsystemreferanse.kortform,
                  traffic_type = vegsystemreferanse.strekning.trafikantgruppe,
                  road_link_position = veglenkesekvens.kortform,
                  distance = avstand,
                  municipality = kommune
                 ) %>%
    dplyr::filter(traffic_type == "K") %>%
    dplyr::group_by(site_id) %>%
    dplyr::slice_min(distance)

  return(trp_data)
}


# Geografiske områder ----

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

#kommunenr <- "5001"
hent_kommune_v3 <- function(kommunenr) {
  api_query <- paste0(nvdb_url_v3,
                          sti_vegobjekter,
                          "/946",
                          "?inkluder=egenskaper")

  api_query_kommune <- paste0(api_query,
                                  "&kommune=",
                                  kommunenr)

  respons <- GET(api_query_kommune,
                 httr::add_headers(.headers = nvdb_v3_headers))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  kommuneinfo <- bind_rows(uthenta$objekter$egenskaper[[1]], .id = "kid") %>%
    dplyr::select(navn, verdi) %>%
    tidyr::pivot_wider(names_from = navn, values_from = verdi) %>%
    dplyr::select(kommunenavn = Kommunenavn,
                  kommunenr = Kommunenummer,
                  polygon = 1) %>%
    sf::st_as_sf(wkt = "polygon",
                 crs = 5973) %>%
    sf::st_zm(drop = T, what = "ZM") %>%
    sf::st_transform("+proj=longlat +datum=WGS84")

  return(kommuneinfo)
}

hent_alle_kommuner_v3 <- function() {
  api_query <- paste0(nvdb_url_v3,
                      "/omrader/kommuner",
                      "?inkluder=kartutsnitt")#&srid=wgs84")

  respons <- GET(api_query,
                 httr::add_headers(.headers = nvdb_v3_headers))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  uthenta_sf <- uthenta %>%
    sf::st_as_sf(wkt = "kartutsnitt.wkt",
                 crs = 5973) %>%
    sf::st_zm(drop = T, what = "ZM") %>%
    #dplyr::select(GEOMETRY) %>%
    sf::st_transform("+proj=longlat +datum=WGS84")

  # uthenta_sf <- sf::st_as_sf(uthenta,
  #                            wkt = "kartutsnitt.wkt",
  #                            crs = "+proj=longlat +datum=WGS84")
  #
  return(uthenta_sf)
}

# kommunenr <- "5031"
# hent_kommune_v3 <- function(kommunenr) {
#
#   api_query <- paste0(nvdb_url_v3,
#                       sti_vegobjekter,
#                       "/946",
#                       "?inkluder=egenskaper,lokasjon")
#
#   api_query_kommune <- paste0(api_query,
#                                   "?kommune=",
#                                   kommunenr,
#                                   "&srid=wgs84")
#
#   respons <- GET(api_query_kommune,
#                  add_headers("X-Client" = "trafikkdatagruppa",
#                              "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
#                              "Accept" = "application/vnd.vegvesen.nvdb-v3-rev1+json"))
#
#   uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
#                       simplifyDataFrame = T,
#                       flatten = T)
#
#   kommune <- bind_rows(uthenta$objekter$egenskaper, .id = "kid") %>%
#     filter(id %in% c(4585))
#
#   kommunenavn <- kommune$verdi
#
#   polygon <- uthenta$objekter$lokasjon.geometri.wkt
#
#   svar <- list(kommunenavn, polygon)
#
#   return(svar)
# }


# Hent målestasjoner ----
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


# Bomstasjoner ----
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
    rename(id1 = id) %>%
    unnest(cols = c(egenskaper)) %>%
    filter(id %in% c(1078, 9595)) %>%
    select(id1, navn, verdi) %>%
    rename(id = id1) %>%
    spread(navn, verdi)

  vegreferanser <- uthenta$objekter %>%
    select(id, lokasjon.vegreferanser) %>%
    unnest(cols = c(lokasjon.vegreferanser))

  koordinater <- uthenta$objekter %>%
    select(id, lokasjon.geometri.wkt) %>%
    unnest(cols = c(lokasjon.geometri.wkt)) %>%
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

get_tolling_stations_v3 <- function(kommunenr) {
  # Laget for Trondheim
  api_query_45 <- paste0(nvdb_url_v3,
                         sti_vegobjekter,
                         "/45",
                         "?inkluder=egenskaper,lokasjon")

  api_query_45_kommune <- paste0(api_query_45,
                                 "&kommune=",
                                 kommunenr,
                                 "&srid=wgs84")

  respons <- GET(api_query_45_kommune,
                 httr::add_headers(.headers = nvdb_v3_headers))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  bom <- uthenta$objekter %>%
    dplyr::select(id, egenskaper) %>%
    dplyr::rename(id1 = id) %>%
    tidyr::unnest(cols = c(egenskaper)) %>%
    dplyr::filter(id %in% c(1078, 9595)) %>%
    dplyr::select(id1, navn, verdi) %>%
    dplyr::rename(id = id1) %>%
    tidyr::pivot_wider(names_from = navn, values_from = verdi) %>%
    dplyr::rename(name = 2,
                  msnr = 3)

  vegreferanser <- uthenta$objekter %>%
    dplyr::select(id, lokasjon.vegsystemreferanser) %>%
    tidyr::unnest(cols = c(lokasjon.vegsystemreferanser)) %>%
    dplyr::select(id, kortform) %>%
    dplyr::rename(road_reference = kortform)

  koordinater <- uthenta$objekter %>%
    dplyr::select(id, lokasjon.geometri.wkt) %>%
    dplyr::mutate(geometri_sub = str_sub(lokasjon.geometri.wkt, 9, -2)) %>%
    tidyr::separate(geometri_sub, into = c("lat", "lon", "alt"), sep = "[[:space:]]",
             convert = T) %>%
    dplyr::select(id, lat, lon)

  veglenkeposisjoner <- uthenta$objekter %>%
    dplyr::select(id, lokasjon.stedfestinger) %>%
    tidyr::unnest(cols = c(lokasjon.stedfestinger)) %>%
    dplyr::select(id, kortform) %>%
    dplyr::rename(road_link_position = kortform)

  # Setter sammen
  bomer <- bom %>%
    dplyr::left_join(vegreferanser) %>%
    dplyr::left_join(koordinater) %>%
    dplyr::left_join(veglenkeposisjoner) %>%
    dplyr::select(-id) %>%
    dplyr::select(msnr, name, road_reference,
                  road_link_position, lat, lon)

  return(bomer)
}


# Årsdøgntrafikk ----
#roadref <- "fv76s1d1m18400"
getAadtByRoadReference <- function(roadref) {
  api_query_540 <- paste0(nvdb_url_v3,
                         sti_vegobjekter,
                         "/540",
                         "?inkluder=egenskaper")

  api_query_540_vegref <- paste0(api_query_540,
                                 "&vegsystemreferanse=",
                                 roadref)

  respons <- GET(api_query_540_vegref,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v3+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  adt_total <- uthenta$objekter %>%
    dplyr::select(id, egenskaper) %>%
    rename(id1 = id) %>%
    tidyr::unnest(cols = egenskaper) %>%
    filter(id %in% c(4623)) %>%
    select(verdi)

  adt_verdi <- round(as.numeric(adt_total[1, 1]), digits = -2)

  return(adt_verdi)
}


get_historic_aadt_by_roadlinkposition <- function(roadlinkposition) {

  api_query_540 <- paste0(nvdb_url_v3,
                          sti_vegobjekter,
                          "/540",
                          "?inkluder=egenskaper")

  api_query_540_vegref <- paste0(api_query_540,
                                 "&veglenkesekvens=",
                                 roadlinkposition,
                                 "&alle_versjoner=TRUE")

  respons <- httr::GET(
    api_query_540_vegref,
    add_headers("X-Client" = "trafikkdatagruppa",
                "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                "Accept" = "application/vnd.vegvesen.nvdb-v3+json"))

  uthenta <- jsonlite::fromJSON(
    str_conv(respons$content, encoding = "UTF-8"),
    simplifyDataFrame = T,
    flatten = T)

  adt_history <- uthenta$objekter %>%
    dplyr::select(id, egenskaper) %>%
    dplyr::rename(id1 = id) %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column("versjon_id") %>%
    tidyr::unnest(cols = egenskaper) %>%
    dplyr::filter(id %in% c(4621, 4623, 4624, 4625)) %>%
    dplyr::select(versjon_id, id, navn, verdi) %>%
    dplyr::mutate(value_name = dplyr::case_when(
      id == 4621 ~ "year",
      id == 4623 ~ "aadt_total",
      id == 4624 ~ "heavy_percentage",
      id == 4625 ~ "source",
      TRUE ~ "unspecified"
    )) %>%
    dplyr::select(versjon_id, value_name, verdi) %>%
    tidyr::pivot_wider(names_from = value_name, values_from = verdi) %>%
    dplyr::select(-versjon_id) %>%
    dplyr::mutate(aadt_total = as.numeric(aadt_total),
                  heavy_percentage = as.numeric(heavy_percentage)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(year)

  return(adt_history)
}


#roadlink <- "0.81008@41567"
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

  adt_verdi <- round(as.numeric(adt_total[1, 1]), digits = -1)

  return(adt_verdi)
}
test <- getAadtByRoadlinkposition("0.4@2411536")
roadlink <- "0.4@2411536"
#roadref <- "1200EV39hp74m14171"

# Fartsgrense ----
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

get_speedlimit_by_roadlink <- function(roadlink) {
  api_query_105 <- paste0(nvdb_url_v3,
                          sti_vegobjekter,
                          "/105",
                          "?inkluder=egenskaper")

  api_query_105_vegref <- paste0(api_query_105,
                                 "&veglenkesekvens=",
                                 roadlink)

  respons <- GET(api_query_105_vegref,
                 httr::add_headers(.headers = nvdb_v3_headers))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  objekter <- uthenta$objekter

  if(length(objekter) == 0) {
    verdi = NA
  }else{
    speed_limit_raw <- objekter %>%
      dplyr::select(egenskaper) %>%
      tidyr::unnest(cols = c(egenskaper))

    # Some responses lack 5127 (valid from date)
    if(5127 %in% speed_limit_raw) {
      speed_limit <- speed_limit_raw %>%
        dplyr::filter(id %in% c(2021, 5127)) %>%
        dplyr::select(navn, verdi) %>%
        dplyr::distinct() %>%
        tidyr::pivot_wider(names_from = navn,
                           values_from = verdi) %>%
        dplyr::rename(speed_limit = 1,
                      valid_from = 2) %>%
        dplyr::mutate(valid_from = lubridate::ymd(valid_from)) %>%
        # Choosing latest
        dplyr::filter(valid_from == max(valid_from))

      verdi <- speed_limit$speed_limit[1]
    }else{
      speed_limit <- speed_limit_raw %>%
        dplyr::filter(id %in% c(2021)) %>%
        dplyr::select(navn, verdi)

      verdi <- speed_limit$verdi[1]
    }
  }

  verdi <- as.numeric(verdi)

  return(verdi)
}


# Veglengder ----
#municipality_number <- "5001"
get_road_length_for_municipality <- function(municipality_number) {

  # Paginated response, starting with an empty tibble to add each response to
  road_segments_selected <- tibble::tibble()

  api_query <- paste0(nvdb_url_v3,
                      "/vegnett/veglenkesekvenser/segmentert?",
                      "kommune=",
                      municipality_number,
                      "&historisk=false",
                      "&kryssystem=false",
                      "&sideanlegg=false",
                      "&detaljniva=VT,VTKB",
                      "&typeveg=kanalisertveg,enkelbilveg",
                      "&adskiltelop=Med,Nei",
                      "&veglenketype=hoved",
                      "&trafikantgruppe=K",
                      "&geometritoleranse=30",
                      "&tidspunkt='2021-01-01'")

  respons <- httr::GET(api_query,
                      httr::add_headers(.headers = nvdb_v3_headers))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  road_segment_info <- uthenta$objekter %>%
    dplyr::select(link_type = type,
                  road_type = typeVeg,
                  length_m = lengde,
                  road_reference = vegsystemreferanse.kortform,
                  road_category = vegsystemreferanse.vegsystem.vegkategori,
                  road_phase = vegsystemreferanse.vegsystem.fase,
                  road_number = vegsystemreferanse.vegsystem.nummer,
                  ) %>%
    dplyr::filter(road_category %in% c("E", "R", "F", "K"))

  road_segments_selected <- dplyr::bind_rows(
    road_segments_selected,
    road_segment_info
  )

  # How to know if has next page?
  # If "returned" < 1000 !!! Beware of changed page size in their API!
  returned <- uthenta$metadata$returnert
  # TODO: use page_size given in response as determination

  while(returned == 1000) {
    next_page <- uthenta$metadata$neste$href

    respons <- httr::GET(next_page,
                         httr::add_headers(.headers = nvdb_v3_headers))

    uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                        simplifyDataFrame = T,
                        flatten = T)

    road_segment_info <- uthenta$objekter %>%
      dplyr::select(link_type = type,
                    road_type = typeVeg,
                    length_m = lengde,
                    road_reference = vegsystemreferanse.kortform,
                    road_category = vegsystemreferanse.vegsystem.vegkategori,
                    road_phase = vegsystemreferanse.vegsystem.fase,
                    road_number = vegsystemreferanse.vegsystem.nummer,
      ) %>%
      dplyr::filter(road_category %in% c("E", "R", "F", "K"))

    road_segments_selected <- dplyr::bind_rows(
      road_segments_selected,
      road_segment_info
    )

    returned <- uthenta$metadata$returnert
  }

  road_lengths <- road_segments_selected %>%
    group_by(road_category) %>%
    summarise(length_km = round(sum(length_m) / 1000, digits = 0)) %>%
    mutate(municipality_number = municipality_number)

  return(road_lengths)
}



#trondheim_roads <- get_road_length_for_municipality("5001")


# ÅDT-belegging per riksvegrute ----
#rutenavn <- "RUTE3"
#periode <- "2014-2023 / 2018-2029"
get_trafikkmengde_for_riksvegrute <- function(rutenavn, periode) {

  # TODO: tidspunkt, vegnett gyldig, år gjelder for o.l.
  periode <- stringr::str_replace_all(periode, " ", "%20")
  resultat <- tibble::tibble()

  api_query <- paste0(nvdb_url_v3,
                      sti_vegobjekter,
                      "/540",
                      "?inkluder=egenskaper,lokasjon",
                      "&riksvegrute=%27",
                      rutenavn,
                      "%20",
                      periode,
                      "%27")

  respons <- httr::GET(api_query,
                       httr::add_headers(.headers = nvdb_v3_headers))

  Sys.sleep(2)

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  process_response <- function(uthenta) {

  trafikkmengder <- uthenta$objekter %>%
    dplyr::select(id, egenskaper) %>%
    # Avoiding bind_rows to fail due to different data types, when some rows are missing in response
    dplyr::mutate(egenskaper = map(egenskaper, ~ .x %>%
                        mutate_all(as.character))) %>%
    tidyr::unnest(egenskaper, names_repair = "unique") %>%
    dplyr::filter(id...2 %in% c(4621, 4623, 4624)) %>%
    dplyr::select(id = id...1, navn, verdi) %>%
    dplyr::mutate(navn = dplyr::case_when(navn == "År, gjelder for" ~ "year",
                                          navn == "ÅDT, total" ~ "aadt",
                                          navn == "ÅDT, andel lange kjøretøy" ~ "part_heavy")) %>%
    tidyr::pivot_wider(names_from = navn, values_from = verdi) %>%
    dplyr::mutate(aadt = as.numeric(aadt))

  lengder <- uthenta$objekter %>%
    dplyr::select(id, lengde = lokasjon.lengde)

  geometri <- uthenta$objekter %>%
    dplyr::select(id, geometri = lokasjon.geometri.wkt) %>%
    dplyr::filter(!is.na(geometri))

  vegsystemreferanser <- uthenta$objekter %>%
    dplyr::select(id, vegsystemreferanser = lokasjon.vegsystemreferanser) %>%
    tidyr::unnest(vegsystemreferanser) %>%
    dplyr::select(id, kortform) %>%
    dplyr::group_by(id) %>%
    dplyr::arrange(id, kortform)

  vegsystemreferanser_start <- vegsystemreferanser %>%
    dplyr::summarise(vegref_start = min(kortform))

  vegsystemreferanser_slutt <- vegsystemreferanser %>%
    dplyr::summarise(vegref_slutt = max(kortform))

  trafikkmengder_med_lengder <- trafikkmengder %>%
    dplyr::left_join(vegsystemreferanser_start) %>%
    dplyr::left_join(vegsystemreferanser_slutt) %>%
    dplyr::left_join(lengder) %>%
    dplyr::left_join(geometri) %>%
    dplyr::filter(lengde > 0)

  }

  delresultat <- process_response(uthenta)

  resultat <- dplyr::bind_rows(
    resultat,
    delresultat
  )

  # Pagination
  page_size <- uthenta$metadata$'sidestørrelse'
  returned <- uthenta$metadata$returnert
  total_size <- uthenta$metadata$antall

  while(returned == page_size) {
    next_page <- uthenta$metadata$neste$href

    respons <- httr::GET(next_page,
                         httr::add_headers(.headers = nvdb_v3_headers))

    Sys.sleep(2)

    uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                        simplifyDataFrame = T,
                        flatten = T)

    delresultat <- process_response(uthenta)

    resultat <- dplyr::bind_rows(
      resultat,
      delresultat
    )

    returned <- uthenta$metadata$returnert
  }

  resultat_sf <- resultat %>%
    sf::st_as_sf(wkt = "geometri",
                 crs = 5973,
                 na.fail = FALSE) %>%
    sf::st_zm(drop = T, what = "ZM") %>%
    sf::st_transform("+proj=longlat +datum=WGS84")

  return(resultat_sf)
}


# Nasjonale turistveger ----

get_national_tourist_roads <- function() {

  api_query <- paste0(nvdb_url_v3,
                      "/vegobjekter/777?segmentering=true&inkluder=lokasjon,egenskaper")

  respons <- httr::GET(api_query,
                       httr::add_headers(.headers = nvdb_v3_headers))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  navn <- uthenta$objekter %>%
    dplyr::select(objekt_id = id, egenskaper) %>%
    tidyr::unnest(egenskaper) %>%
    dplyr::filter(id %in% c(8128, 8129)) %>%
    dplyr::select(objekt_id, navn, verdi) %>%
    tidyr::spread(navn, verdi)

  vegreferanser <- uthenta$objekter %>%
    dplyr::select(objekt_id = id, lokasjon.vegsystemreferanser) %>%
    tidyr::unnest(lokasjon.vegsystemreferanser) %>%
    dplyr::mutate(veg = paste0(vegsystem.vegkategori, "v ", vegsystem.nummer)) %>%
    dplyr::select(objekt_id, veg) %>%
    dplyr::distinct()

  # For å finne hvilke trper som ligger på strekningene
  veglenkeposisjoner <- uthenta$objekter %>%
    dplyr::select(objekt_id = id, lokasjon.stedfestinger) %>%
    tidyr::unnest(lokasjon.stedfestinger) %>%
    dplyr::select(objekt_id, veglenkesekvensid, startposisjon, sluttposisjon,
                  kortform)

  lengder <- uthenta$objekter %>%
    dplyr::select(objekt_id = id, lokasjon.lengde)

  geometri <- uthenta$objekter %>%
    dplyr::select(objekt_id = id, lokasjon.geometri.wkt)

  srid <- uthenta$objekter %>%
    dplyr::select(objekt_id = id, lokasjon.geometri.srid)

  turistveg_vegreferanser <- navn %>%
    dplyr::left_join(vegreferanser, by = "objekt_id") %>%
    dplyr::left_join(lengder, by = "objekt_id")

  turistveg_veglenkeposisjoner <- navn %>%
    dplyr::left_join(veglenkeposisjoner, by = "objekt_id") %>%
    dplyr::left_join(lengder, by = "objekt_id")

  turistveg_geometri <- navn %>%
    dplyr::left_join(geometri, by = "objekt_id") %>%
    dplyr::left_join(srid, by = "objekt_id") %>%
    sf::st_as_sf(wkt = "lokasjon.geometri.wkt",
                 crs = 5973,
                 na.fail = FALSE) %>%
    sf::st_zm(drop = T, what = "ZM") %>%
    sf::st_transform("+proj=longlat +datum=WGS84")

  turistveger <- list(
    vegreferanser = turistveg_vegreferanser,
    veglenkeposisjoner = turistveg_veglenkeposisjoner,
    geometri = turistveg_geometri
  )

  return(turistveger)
}
