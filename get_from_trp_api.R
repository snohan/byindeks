# Get data directly from TRP-API

# Library and sources ####
library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr)
library(ghql)

source("H:/Programmering/R/byindeks/trp_api_cookies.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

# Definitions ####
#trp_api_url <- "https://www.vegvesen.no/datainn/traffic-registration-point/api/"
trp_api_url <- "https://trafikkdata-adm.atlas.vegvesen.no/datainn/traffic-registration-point/api/"

parse_and_floor_date <- function(date_column, floor_unit) {
  # floor_unit is second, minute, day etc.
  date_column_parsed <-
  lubridate::floor_date(
    lubridate::with_tz(
      lubridate::ymd_hms(date_column)),
    unit = floor_unit)
}

# Without ghql
get_via_httr <- function(api_query) {

  api_query_trimmed <- stringr::str_replace_all(api_query, "[\r\n]", " ")

  api_query_string <- paste0('{"query":"', api_query_trimmed, '" }')

  response <- httr::POST(url = trp_api_url,
                         httr::add_headers(.headers = trp_api_headers),
                         httr::set_cookies(.cookies = trp_api_cookies),
                         body = api_query_string)

  response_parsed <- fromJSON(str_conv(response$content, encoding = "UTF-8"),
                              simplifyDataFrame = T,
                              flatten = T) %>%
    as.data.frame()

  return(response_parsed)
}

# With ghql (doesn't work anymore, unknown why)
# cli_trp <- GraphqlClient$new(
#   url = "https://www.vegvesen.no/datainn/traffic-registration-point/api/",
#   headers = list(
#     #.headers = trp_api_headers,
#     .cookies = trp_api_cookies
#   )
#   #httr::add_headers(.headers = trp_api_headers),
#   #httr::set_cookies(.cookies = trp_api_cookies)
# )


# Points ####
get_points_from_trp_api <- function() {
  # Get all traffic registration points

  api_query <-
    "query all_trps {
      trafficRegistrationPoints {
        id
        name
        trafficType
        operationalStatus
        registrationFrequency
        legacyNortrafMpn
        location{
          coordinates{
            latlon{
              latitude
              longitude
            }
          }
          municipality {
            name
            number
            county {
              geographicNumber
              name
            }
          }
          roadReference {
            shortForm
          }
          roadLink {
            id
            position
          }
        }
      }
    }"

  points_trp <- get_via_httr(api_query) %>%
    dplyr::rename(
      trp_id = data.trafficRegistrationPoints.id,
      legacy_nortraf_mpn = data.trafficRegistrationPoints.legacyNortrafMpn,
      name = data.trafficRegistrationPoints.name,
      traffic_type = data.trafficRegistrationPoints.trafficType,
      trp_status = data.trafficRegistrationPoints.operationalStatus,
      registration_frequency = data.trafficRegistrationPoints.registrationFrequency,
      municipality_name = data.trafficRegistrationPoints.location.municipality.name,
      #county_geono = data.trafficRegistrationPoints.location.municipality.county.geographicNumber,
      county_name = data.trafficRegistrationPoints.location.municipality.county.name,
      lat = data.trafficRegistrationPoints.location.coordinates.latlon.latitude,
      lon = data.trafficRegistrationPoints.location.coordinates.latlon.longitude,
      road_reference = data.trafficRegistrationPoints.location.roadReference.shortForm,
      road_network_position =
        data.trafficRegistrationPoints.location.roadLink.position,
      road_network_link =
        data.trafficRegistrationPoints.location.roadLink.id) %>%
    dplyr::mutate(road_link_position = paste0(road_network_position, "@",
                                              road_network_link)) %>%
    dplyr::select(trp_id, legacy_nortraf_mpn, name, traffic_type, trp_status, registration_frequency,
                  #county_geono,
                  county_name, municipality_name,
                  road_reference, road_link_position, lat, lon)

  return(points_trp)
}

get_periodic_trps_with_commission <- function() {

  api_query <-
    "query hentTRP {
      trafficRegistrationPoints(stationType: [PERIODIC]) {
    id
    name
    trafficType
    operationalStatus
    registrationFrequency
    location {
      roadLink {
        id
        position
      }
      coordinates {
        latlon {
          latitude
          longitude
        }
      }
      roadReference {
        shortForm
      }
    }
    commissionElements {
      commission {
        validFrom
        validTo
      }
    }
  }
}"

  response_parsed <-
    get_via_httr(api_query) %>%
    tidyr::unnest(cols =
                    c("data.trafficRegistrationPoints.commissionElements")) %>%
    dplyr::select(trp_id = data.trafficRegistrationPoints.id,
                  trp_name = data.trafficRegistrationPoints.name,
                  trafficType = data.trafficRegistrationPoints.trafficType,
                  status = data.trafficRegistrationPoints.operationalStatus,
                  registration_frequency = data.trafficRegistrationPoints.registrationFrequency,
                  commission_from = commission.validFrom,
                  commission_to = commission.validTo,
                  road_link = data.trafficRegistrationPoints.location.roadLink.id,
                  road_link_position = data.trafficRegistrationPoints.location.roadLink.position,
                  lat = data.trafficRegistrationPoints.location.coordinates.latlon.latitude,
                  lon = data.trafficRegistrationPoints.location.coordinates.latlon.longitude,
                  road_reference = data.trafficRegistrationPoints.location.roadReference.shortForm) %>%
    dplyr::mutate(commission_from = parse_and_floor_date(commission_from, "day"),
                  commission_to = parse_and_floor_date(commission_to, "day"),
                  commission_interval = lubridate::interval(commission_from,
                                                            commission_to),
                  commission_length_in_days = round(commission_interval / lubridate::ddays(1),
                                                     digits = 0)
                  )

  return(response_parsed)
}

get_trp_for_vti <- function() {

  counties_numbers <- get_counties() %>%
    select(county_number, geo_number)

  api_query <-
    "query vti_trp {
  trafficRegistrationPoints (trafficType: VEHICLE) {
    id
    name
    location{
      coordinates{
        latlon{
          latitude
          longitude
        }
      }
      roadReference{
        shortForm
        roadCategory {
          id
        }
      }
      roadLink{
        id
        position
      }
      municipality {
        county {
          number
          name
        }
        number
        name
      }
    }
    commissionElements {
      commission {
        validFrom
        validTo
      }
    }
  }
}"

  response_parsed <- get_via_httr(api_query) %>%
    tidyr::unnest(cols =
                    c("data.trafficRegistrationPoints.commissionElements" )) %>%
    dplyr::rename(
      trp_id = data.trafficRegistrationPoints.id,
      name = data.trafficRegistrationPoints.name,
      lat = data.trafficRegistrationPoints.location.coordinates.latlon.latitude,
      lon = data.trafficRegistrationPoints.location.coordinates.latlon.longitude,
      road_reference =
        data.trafficRegistrationPoints.location.roadReference.shortForm,
      road_network_position =
        data.trafficRegistrationPoints.location.roadLink.position,
      road_network_link =
        data.trafficRegistrationPoints.location.roadLink.id,
      valid_from = commission.validFrom,
      valid_to = commission.validTo,
      county_number = data.trafficRegistrationPoints.location.municipality.county.number,
      county_name = data.trafficRegistrationPoints.location.municipality.county.name,
      municipality_number = data.trafficRegistrationPoints.location.municipality.number,
      municipality_name = data.trafficRegistrationPoints.location.municipality.name,
      road_category =
        data.trafficRegistrationPoints.location.roadReference.roadCategory.id) %>%
    # Removing points without commissions
    dplyr::filter(!is.na(valid_from)) %>%
    dplyr::mutate(valid_from = lubridate::floor_date(
      lubridate::with_tz(
        lubridate::ymd_hms(valid_from)),
      unit = "second")) %>%
    dplyr::group_by(trp_id) %>%
    dplyr::slice(which.min(valid_from)) %>%
    # tidyr::separate(road_reference,
    #                 into = c("road_number", NA, "parsell", NA, "meter"),
    #                 sep = " ",
    #                 remove = F) %>%
    dplyr::mutate(road_link_position = paste0(road_network_position, "@",
                                              road_network_link),
                  #road_number = as.integer(stringr::str_sub(road_number, 3, -1)),
                  #parsell = as.integer(parsell),
                  #meter = as.integer(meter),
                  #road_reference = str_replace(road_reference, "HP ", "hp"),
                  #road_reference = str_replace(road_reference, "Meter ", "m"),
                  first_commission_datainn = lubridate::floor_date(
                    valid_from, unit = "day")) %>%
    #dplyr::filter(#parsell < 70,
    #              first_commission_datainn < "2019-02-01") %>%
    dplyr::left_join(counties_numbers) %>%
    dplyr::select(geo_number, county_name,
                  municipality_name,
                  trp_id, name,
                  road_reference, road_category,
                  #road_number, parsell, meter,
                  road_link_position, lat, lon, first_commission_datainn) %>%
    dplyr::arrange(geo_number,
                   road_category, road_reference) %>%
    dplyr::select(-geo_number) %>%
    dplyr::ungroup()

  return(response_parsed)
}

get_trp_for_adt <- function() {

  api_query <-
    "query vti_trp {
  trafficRegistrationPoints (trafficType: VEHICLE, stationType: CONTINUOUS) {
    id
    name
    location{
      roadReference{
        shortForm
      }
      municipality {
        name
        county {
          geographicNumber
          name
        }
      }
    }
    commissionElements {
      commission {
        validFrom
        validTo
      }
    }
  }
}"

  api_query_trimmed <- stringr::str_replace_all(api_query, "[\r\n]", " ")

  api_query_string <- paste0('{"query":"', api_query_trimmed, '" }')

  response <- httr::POST(url = trp_api_url,
                         httr::add_headers(.headers = trp_api_headers),
                         httr::set_cookies(.cookies = trp_api_cookies),
                         body = api_query_string)

  response_parsed <- fromJSON(str_conv(response$content, encoding = "UTF-8"),
                              simplifyDataFrame = T,
                              flatten = T) %>%
    as.data.frame() %>%
    tidyr::unnest(cols =
                    c("data.trafficRegistrationPoints.commissionElements")) %>%
    dplyr::rename(
      trp_id = data.trafficRegistrationPoints.id,
      name = data.trafficRegistrationPoints.name,
      road_reference =
        data.trafficRegistrationPoints.location.roadReference.shortForm,
      valid_from = commission.validFrom,
      valid_to = commission.validTo,
      municipality_name = data.trafficRegistrationPoints.location.municipality.name,
      county_geonumber = data.trafficRegistrationPoints.location.municipality.county.geographicNumber,
      county_name = data.trafficRegistrationPoints.location.municipality.county.name) %>%
    # Removing points without commissions
    dplyr::filter(!is.na(valid_from)) %>%
    dplyr::mutate(valid_from = lubridate::floor_date(
      lubridate::with_tz(
        lubridate::ymd_hms(valid_from)),
      unit = "second")) %>%
    dplyr::group_by(trp_id) %>%
    dplyr::slice(which.min(valid_from)) %>%
    dplyr::mutate(first_commission = lubridate::floor_date(
                    valid_from, unit = "day")) %>%
    dplyr::select(-valid_from, - valid_to) %>%
    dplyr::select(county_geonumber, county_name, trp_id, name,
                  road_reference, first_commission) %>%
    split_road_system_reference() %>%
    dplyr::select(-road, -road_number) %>%
    dplyr::relocate(road_category, .before = road_reference) %>%
    dplyr::arrange(county_geonumber) %>%
    dplyr::ungroup()

  return(response_parsed)
}

# Stations ####

get_trs_and_trp_id <- function() {
  # Many stations have no trps defined

  api_query <-
    "query trs {
      trafficRegistrationStations {
        id
        name
        operationalStatus
        stationType
        trafficType
        trafficRegistrationPoints {
          id
        }
      }
    }"

  response_parsed <-
    get_via_httr(api_query) %>%
    tidyr::unnest(cols =
                    c("data.trafficRegistrationStations.trafficRegistrationPoints"),
                  keep_empty = TRUE) %>%
    dplyr::select(trs_id = data.trafficRegistrationStations.id,
                  trs_name = data.trafficRegistrationStations.name,
                  trs_status = data.trafficRegistrationStations.operationalStatus,
                  trs_type = data.trafficRegistrationStations.stationType,
                  trs_traffic_type = data.trafficRegistrationStations.trafficType,
                  trp_id = id)

  return(response_parsed)
}

get_stations_from_TRPAPI <- function() {
  # Get all traffic registration stations
  query_points_trp <-
    "query getStation{
  trafficRegistrationStations{
    id
    name
    trafficRegistrationPoints {
      id
      name
      direction{
        from
        to
      }
      location{
        currentRoadReference{
          shortForm
          countyInformation {
            id
            name
          }
        }
      }
    }
  }
}"

  myqueries <- Query$new()
  myqueries$query("points_trp", query_points_trp)

  points_trp <- cli_trp$exec(myqueries$queries$points_trp) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    tidyr::unnest() %>%
    dplyr::rename(
      stasjonnr = data.trafficRegistrationStations.id,
      stasjonnavn = data.trafficRegistrationStations.name,
      punktnr = id,
      punktnavn = name,
      retning_fra = direction.from,
      retning_til = direction.to,
      vegreferanse = location.currentRoadReference.shortForm,
      fylkenr = location.currentRoadReference.countyInformation.id,
      fylkenavn = location.currentRoadReference.countyInformation.name)

  return(points_trp)
}

get_stations_and_trps_with_coordinates_from_TRPAPI_httr <- function() {
  # Get all traffic registration stations
  api_query <-
    "query getStation{
    trafficRegistrationStations{
    id
    name
    location{
      roadReference{
        shortForm
      }
      coordinates{
        latlon{
          latitude
          longitude
        }
      }
    }
    trafficRegistrationPoints {
      id
      name
      location{
        coordinates{
          latlon{
            latitude
            longitude
          }
        }
        currentRoadReference{
          shortForm
          countyInformation {
            id
            name
          }
          category
        }
      }
    }
  }
}"

  points_trp <- get_via_httr(api_query) %>%
    tidyr::unnest(data.trafficRegistrationStations.trafficRegistrationPoints) %>%
    dplyr::rename(
      stasjonnr = data.trafficRegistrationStations.id,
      stasjonnavn = data.trafficRegistrationStations.name,
      stasjon_vegref = data.trafficRegistrationStations.location.roadReference.shortForm,
      stasjon_lat = data.trafficRegistrationStations.location.coordinates.latlon.latitude,
      stasjon_lon = data.trafficRegistrationStations.location.coordinates.latlon.longitude,
      punktnr = id,
      punktnavn = name,
      punkt_lat = location.coordinates.latlon.latitude,
      punkt_lon = location.coordinates.latlon.longitude,
      punkt_vegref = location.currentRoadReference.shortForm,
      punkt_vegref_kort = location.currentRoadReference,
      punkt_vegkategori = location.currentRoadReference.category,
      fylkenr = location.currentRoadReference.countyInformation.id,
      fylkenavn = location.currentRoadReference.countyInformation.name) %>%
    dplyr::select(-punkt_vegref_kort)

  return(points_trp)
}

get_trs_trp <- function() {
  # Get all trs' and their trps
  query_points_trp <-
    "query trs_trp{
 trafficRegistrationStations(
  trafficType: VEHICLE,
  withTrps: true,
  stationType: CONTINUOUS){
  id
  name
  trafficType
  stationType
  trafficRegistrationPoints{
    id
    name
    legacyNortrafMpn
  }
}
}"

myqueries <- Query$new()
myqueries$query("points_trp", query_points_trp)

points_trp <- cli_trp$exec(myqueries$queries$points_trp) %>%
  jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
  as.data.frame() %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.trafficRegistrationPoints)) %>%
  dplyr::rename(trs_id = 1,
                trs_name = 2,
                traffic_type = 3,
                station_type = 4,
                trp_id = 5,
                trp_name = 6)

return(points_trp)
}

get_all_trs_with_trp <- function() {
  # Get all trs' and their trps
  # NB! Is not including trp's on trs' without commissions
  api_query <-
    "query trs_trp{
 trafficRegistrationStations(
  withTrps: true){
  id
  name
  trafficType
  stationType
  trafficRegistrationPoints{
    id
    name
    legacyNortrafMpn
  }
}
}"

response_parsed <- get_via_httr(api_query) %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.trafficRegistrationPoints)) %>%
  dplyr::rename(trs_id = 1,
                trs_name = 2,
                traffic_type = 3,
                station_type = 4,
                trp_id = 5,
                trp_name = 6)

return(response_parsed)
}

get_all_trs_with_trp_via_sensorconfig <- function() {
  # Get all trs' and their trps
  api_query <-
"query trs_trp {
  trafficRegistrationStations {
    id
    name
    trafficType
    stationType
    sensorConfigurations {
      trafficRegistrationPoint {
        id
        name
      }
    }
  }
}
"

response_parsed <- get_via_httr(api_query) %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.sensorConfigurations)) %>%
  dplyr::select(trs_id = data.trafficRegistrationStations.id,
                trs_name = data.trafficRegistrationStations.name,
                traffic_type = data.trafficRegistrationStations.trafficType,
                station_type = data.trafficRegistrationStations.stationType,
                trp_id = trafficRegistrationPoint.id,
                trp_name = trafficRegistrationPoint.name)

return(response_parsed)
}

get_trs_trp_commissions_httr <- function() {
  # Get all trp's and their commissions (and trs)
  api_query <-
    "query trs_commissions {
  trafficRegistrationStations (stationType: CONTINUOUS) {
    id
    name
    stationType
    trafficType
    location {
      roadReference {
        shortForm
      }
      municipality {
        county {
          id
          geographicNumber
          name
        }
      }
    }
    deviceTypeHistory {
      deviceType
      validFrom
      validTo
    }
    commissions {
      validFrom
      validTo
    }
    operationalStatus
  }
}
"

response_parsed <- get_via_httr(api_query) %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.commissions)) %>%
  dplyr::rename(commission_from = validFrom,
                commission_to = validTo) %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.deviceTypeHistory)) %>%
  dplyr::rename(trs_id = 1,
                trs_name = 2,
                traffic_type = 4,
                station_type = 3,
                device_type = 5,
                device_from = validFrom,
                device_to = validTo,
                operational_status = 10,
                road_reference = 11,
                county_number = 12,
                geo_number = 13,
                county_name = 14) %>%
  dplyr::mutate(trs_id = as.numeric(trs_id),
                device_from = parse_and_floor_date(device_from, "day"),
                device_to = parse_and_floor_date(device_to, "day"),
                commission_from = parse_and_floor_date(commission_from, "day"),
                commission_to = parse_and_floor_date(commission_to, "day")
  )

return(response_parsed)
}

get_trs_commissions <- function() {

  api_query <-
    "query trs_commissions {
      trafficRegistrationStations {
        id
        name
        operationalStatus
        stationType
        trafficType
        location {
          roadReference {
            shortForm
          }
          municipality {
            name
            county {
              name
            }
          }
        }
        commissions {
          source
          validFrom
          validTo
        }
        trafficRegistrationPoints {
          id
          name
          location {
            roadReference {
              shortForm
            }
          }
        }
      }
    }
    "

response_parsed <- get_via_httr(api_query) %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.commissions)) %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.trafficRegistrationPoints)) %>%
  dplyr::select(trs_id = data.trafficRegistrationStations.id,
                trs_name = data.trafficRegistrationStations.name,
                operational_status = data.trafficRegistrationStations.operationalStatus,
                traffic_type = data.trafficRegistrationStations.trafficType,
                station_type = data.trafficRegistrationStations.stationType,
                trs_road_reference = data.trafficRegistrationStations.location.roadReference.shortForm,
                county_name = data.trafficRegistrationStations.location.municipality.county.name,
                municipality_name = data.trafficRegistrationStations.location.municipality.name,
                system_name = source,
                commission_from = validFrom,
                commission_to = validTo,
                trp_id = id,
                trp_name = name,
                trp_road_reference = location.roadReference.shortForm) %>%
  dplyr::mutate(commission_from = parse_and_floor_date(commission_from, "hour"),
                commission_to = parse_and_floor_date(commission_to, "hour")
  )

return(response_parsed)
}

get_trs_info <- function() {

api_query <-
  "query trs {
    trafficRegistrationStations {
      id
      name
      operationalStatus
      stationType
      trafficType
      location {
        roadReference {
          shortForm
        }
        municipality {
          name
          county {
            name
            geographicNumber
          }
        }
      }
    }
  }"

trs <- get_via_httr(api_query) %>%
  dplyr::select(trs_id = data.trafficRegistrationStations.id,
                name = data.trafficRegistrationStations.name,
                status = data.trafficRegistrationStations.operationalStatus,
                registration_frequency = data.trafficRegistrationStations.stationType,
                traffic_type = data.trafficRegistrationStations.trafficType,
                road_reference = data.trafficRegistrationStations.location.roadReference.shortForm,
                municipality_name = data.trafficRegistrationStations.location.municipality.name,
                county_name = data.trafficRegistrationStations.location.municipality.county.name,
                geo_no = data.trafficRegistrationStations.location.municipality.county.geographicNumber
                ) %>%
  split_road_system_reference() %>%
  dplyr::select(trs_id, name, status, registration_frequency, traffic_type, road_category,
                road_reference, geo_no, county_name, municipality_name)

return(trs)
}

get_trs_trp_lanes_httr <- function() {
  # Get all trs' and their trps
  api_query <-
    "query trs_trp_lanes {
  trafficRegistrationStations (stationType: [CONTINUOUS], trafficType: [VEHICLE], withTrps: true) {
    id
    name
    stationType
    trafficType
    location {
      municipality {
        name
        county {
          number
          name
        }
      }
    }
    commissions {
      validFrom
      validTo
      consistsOf {
        trp {
          id
        }
        lanes {
          trsLaneNumber
          trpLaneNumber
        }
      }
    }
  }
}
"

api_query_trimmed <- stringr::str_replace_all(api_query, "[\r\n]", " ")

api_query_string <- paste0('{"query":"', api_query_trimmed, '" }')

response <- httr::POST(url = trp_api_url,
                       httr::add_headers(.headers = trp_api_headers),
                       httr::set_cookies(.cookies = trp_api_cookies),
                       body = api_query_string)

response_parsed <- fromJSON(str_conv(response$content, encoding = "UTF-8"),
                            simplifyDataFrame = T,
                            flatten = T) %>%
  as.data.frame() %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.commissions)) %>%
  tidyr::unnest(cols = c(consistsOf)) %>%
  tidyr::unnest(cols = c(lanes)) %>%
  dplyr::select(trs_id = data.trafficRegistrationStations.id,
                trs_name = data.trafficRegistrationStations.name,
                station_type = data.trafficRegistrationStations.stationType,
                traffic_type = data.trafficRegistrationStations.trafficType,
                valid_from = validFrom,
                valid_to = validTo,
                trs_lane_number = trsLaneNumber,
                trp_lane_number = trpLaneNumber,
                trp_id = trp.id,
                municipality_name = data.trafficRegistrationStations.location.municipality.name,
                county_name = data.trafficRegistrationStations.location.municipality.county.name)

return(response_parsed)
}


# Manual labels ####
get_manual_labels_deprecated <- function() {
  # Get all manual labels
  query_points_trp <-
    "query hentMM{
  trafficRegistrationPoints{
    id
    name
    trafficType
    manualLabels{
      id
      trpId
      validFrom
      validTo
      created
      description
      labelLanes{
        lane
        states
      }
    }
    legacyNortrafMpn
  }
}"

  myqueries <- Query$new()
  myqueries$query("points_trp", query_points_trp)

  points_trp <- cli_trp$exec(myqueries$queries$points_trp) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(data.trafficRegistrationPoints.manualLabels)) %>%
    tidyr::unnest(cols = c(labelLanes)) %>%
    dplyr::mutate(states = stringr::str_c(states)) %>%
    dplyr::select(-labelLanes, -trpId) %>%
    dplyr::rename(stasjonnr = data.trafficRegistrationPoints.legacyNortrafMpn,
                  punkt_id = data.trafficRegistrationPoints.id,
                  punktnavn = data.trafficRegistrationPoints.name,
                  trafikanttype = data.trafficRegistrationPoints.trafficType,
                  merke_id = id,
                  gjelder_fra = validFrom,
                  gjelder_til = validTo,
                  lagt_inn = created,
                  beskrivelse = description,
                  felt = lane,
                  grunn = states) %>%
    dplyr::select(stasjonnr, everything()) %>%
    dplyr::mutate(gjelder_fra =
                    lubridate::with_tz(
                      lubridate::ymd_hms(gjelder_fra)),
                  gjelder_til =
                    lubridate::with_tz(
                      lubridate::ymd_hms(gjelder_til)),
                  lagt_inn =
                    lubridate::with_tz(
                      lubridate::ymd_hms(lagt_inn)
    ))

  return(points_trp)
}

#county <- "50"
#road_cat <- "F"
get_manual_labels_by_county <- function(county, road_cat) {

  api_query <- paste0(
    "query manual_labels {
      trafficRegistrationPoints (counties: [",
      county,
      "], roadCategory: [",
      road_cat,
      "]) {
        id
        name
            location {
              municipality {
                county {
                  name
                }
              }
              roadReference {
                shortForm
              }
            }
        trafficType
        registrationFrequency
        manualLabels{
          id
          validFrom
          validTo
          created
          description
          labelLanes{
            lane
            states
          }
        }
      }
    }")

  response_parsed <- get_via_httr(api_query) %>%
    tidyr::unnest(cols = c(data.trafficRegistrationPoints.manualLabels)) %>%
    tidyr::unnest(cols = c(labelLanes)) %>%
    #dplyr::mutate(states = stringr::str_c(states, collapse = ", ")) %>%
    dplyr::select(-labelLanes) %>%
    dplyr::select(trp_id = data.trafficRegistrationPoints.id,
                  punktnavn = data.trafficRegistrationPoints.name,
                  fylke = data.trafficRegistrationPoints.location.municipality.county.name,
                  vegreferanse = data.trafficRegistrationPoints.location.roadReference.shortForm,
                  trafikanttype = data.trafficRegistrationPoints.trafficType,
                  registreringshyppighet = data.trafficRegistrationPoints.registrationFrequency,
                  merke_id = id,
                  gjelder_fra = validFrom,
                  gjelder_til = validTo,
                  lagt_inn = created,
                  beskrivelse = description,
                  felt = lane,
                  grunn = states) %>%
    dplyr::mutate(gjelder_fra =
                    lubridate::with_tz(
                      lubridate::ymd_hms(gjelder_fra)),
                  gjelder_til =
                    lubridate::with_tz(
                      lubridate::ymd_hms(gjelder_til)),
                  lagt_inn =
                    lubridate::with_tz(
                      lubridate::ymd_hms(lagt_inn)
                    )) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(grunn = stringr::str_c(grunn, collapse = ", "))

  return(response_parsed)
}


# Devices ####
get_trs_device <- function() {
  # Get all trs' and their device type history
  query_trs <-
    "query getTRS {
  trafficRegistrationStations(
    withTrps: true,
    trafficType: VEHICLE,
    stationType: CONTINUOUS) {
    id
    trafficRegistrationPoints {
      id
    }
    deviceTypeHistory {
      validFrom
      validTo
      deviceType
    }
  }
}
"

  myqueries <- Query$new()
  myqueries$query("points_trp", query_trs)

  trs <- cli_trp$exec(myqueries$queries$points_trp) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(2)) %>%
    tidyr::unnest(cols = c(3)) %>%
    # Keeping only last device
    dplyr::filter(is.na(validTo)) %>%
    dplyr::rename(trs_id = 1,
                  trp_id = 2)

  return(trs)
}

get_trs_history <- function() {
  # Get all trs' and their history
  api_query <-
    "query all_trs {
    trafficRegistrationStations {
	id
  name
  deviceTypeHistory {
    validFrom
    validTo
    deviceType
  }
  firmwareHistory {
    validFrom
    validTo
    firmwareVersion
  }
  commissions {
    validFrom
    validTo
  }
	}
}
"

response_parsed <- get_via_httr(api_query) %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.commissions),
                keep_empty = TRUE) %>%
  dplyr::rename(commission_valid_from = validFrom,
                commission_valid_to = validTo) %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.deviceTypeHistory),
                keep_empty = TRUE) %>%
  dplyr::rename(device_valid_from = validFrom,
                device_valid_to = validTo) %>%
  tidyr::unnest(cols = c(data.trafficRegistrationStations.firmwareHistory),
                keep_empty = TRUE) %>%
  dplyr::rename(firmware_valid_from = validFrom,
                firmware_valid_to = validTo,
                trs_id = 1)

  return(trs)
}


# Mutations ####
#trp_id <- "16334V971464"

get_specific_trp <- function(trp_id) {
  query_points_trp <-
    paste0(
    "query hentTRP{
  trafficRegistrationPointsById(trpIds: \"", trp_id,"\") {
    id
    name
    trafficType
    location{
      roadLink {
        id
        position
      }
    }
    direction {
      from
      to
    }
    legacyNortrafMpn
  }
}")

myqueries <- Query$new()
myqueries$query("points_trp", query_points_trp)

trp <- cli_trp$exec(myqueries$queries$points_trp) %>%
  jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
  as.data.frame() %>%
  dplyr::rename(trp_id = 1,
                trp_name = 2,
                traffic_type = 3,
                legacy_nortraf_mpn = 4,
                road_link_id = 5,
                road_link_position = 6,
                direction_from = 7,
                direction_to = 8)

return(trp)
}


mutate_trp <- function(trp_id, legacy_mpn) {

trp_info <- get_specific_trp(trp_id)

mutate_string <- paste0(
"mutation update_trp{
  updateTrafficRegistrationPoint(input: {
    id: \"", trp_info$trp_id, "\"
    trp: {
      name: \"", trp_info$trp_name, "\"
      trafficType: ", trp_info$traffic_type,
    	"
    	roadLink: {
        	id: ", trp_info$road_link_id, "
        	position:  ", trp_info$road_link_position, "
      }
    	direction: {
      		from: \"", trp_info$direction_from, "\"
      		to: \"", trp_info$direction_to, "\"
      }
    	legacyNortrafMpn: ", legacy_mpn, "
    }
  }) {
    id
    legacyNortrafMpn
  }
}")

myqueries <- Query$new()
myqueries$query("mutate_trp", mutate_string)
cli_trp$exec(myqueries$queries$mutate_trp)
}


# Manual trps ####
get_manual_points_from_trpapi_httr <- function() {
  # Get all traffic registration points
  api_query <-
    "query mtrps {
  manualTrafficRegistrationPoints {
    id
    name
    location {
      municipality {
        name
        county {
          name
        }
      }
      roadReference {
        shortForm
      }
    }
    direction {
      from
      to
    }
  }
}"

  # TODO: Revise from here
  points_trp <- get_via_httr(api_query) %>%
    dplyr::rename(
      mtrp_id = data.manualTrafficRegistrationPoints.id,
      name = data.manualTrafficRegistrationPoints.name,
      municipality_name = data.manualTrafficRegistrationPoints.location.municipality.name,
      county_name = data.manualTrafficRegistrationPoints.location.municipality.county.name,
      road_reference = data.manualTrafficRegistrationPoints.location.roadReference.shortForm,
      from = data.manualTrafficRegistrationPoints.direction.from,
      to = data.manualTrafficRegistrationPoints.direction.to)

  return(points_trp)
}




