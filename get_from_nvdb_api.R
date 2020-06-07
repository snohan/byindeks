# Funksjoner for å hente nødvendig info fra NVDB-API v2 og v3.

library(tidyverse)
library(httr)
library(jsonlite)

# Definer URI og sti ####
nvdb_url <- "https://www.vegvesen.no/nvdb/api/v2"
nvdb_url_v3 <- "https://www.vegvesen.no/nvdb/api/v3"
sti_vegobjekter <- "/vegobjekter"
sti_veg <- "/veg"

nvdb_v3_headers <- c(
  "X-Client" = "trafikkdatagruppa",
  "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
  "Accept" = "application/vnd.vegvesen.nvdb-v3+json"
)


vegsystemreferanse <- "KV6064S1D1m30"
kommunenr <- "5001"
# Hent stedfesting i punkt på vegnettet
hent_vegpunkt <- function(vegsystemreferanse, kommunenr) {
  api_query <- paste0(nvdb_url_v3,
                      sti_veg,
                      "?vegsystemreferanse=",
                      vegsystemreferanse,
                      "&kommune=",
                      kommunenr)

  respons <- GET(api_query,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v3+json"))

  uthenta <- jsonlite::fromJSON(
    stringr::str_conv(
    respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  kommune <- bind_rows(uthenta$objekter$egenskaper, .id = "kid")
}

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
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v3-rev1+json"))

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

roadlink <- "0.81008@41567"
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

roadlink <- "0.38722@2037772"

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
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v3+json"))

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

  return(verdi)
}

municipality_number <- "5001"
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
                      "&tidspunkt='2020-01-01'")

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

  # TODO: paginering, a while loop?
  # How to know if has next page?
  # If "returned" < 1000 !!!
  returned <- uthenta$metadata$returnert

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

trondheim_roads <- get_road_length_for_municipality("5001")





