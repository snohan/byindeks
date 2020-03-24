# Get data directly from TRP-API

library(httr)

source("H:/Programmering/R/byindeks/trp_api_cookies.R")

cli_trp <- GraphqlClient$new(
  url = "https://www.vegvesen.no/datainn/adm/traffic-registration-point/api/",
  headers =
    set_cookies(.cookies = trp_api_cookies
    ))

getPointsFromTRPAPI <- function() {
  # Get all traffic registration points
  query_points_trp <-
    "query allPoints{
  trafficRegistrationPoints{
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
      }
      roadLink{
        id
        position
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
    #tidyr::unnest()%>%
    dplyr::rename(
      trp_id = data.trafficRegistrationPoints.id,
      name = data.trafficRegistrationPoints.name,
      lat = data.trafficRegistrationPoints.location.coordinates.latlon.latitude,
      lon = data.trafficRegistrationPoints.location.coordinates.latlon.longitude,
      road_reference = data.trafficRegistrationPoints.location.roadReference.shortForm,
      road_network_position =
        data.trafficRegistrationPoints.location.roadLink.position,
      road_network_link =
        data.trafficRegistrationPoints.location.roadLink.id,
      legacyNortrafMpn = data.trafficRegistrationPoints.legacyNortrafMpn) %>%
    dplyr::mutate(road_link_position = paste0(road_network_position, "@",
                                              road_network_link)) %>%
    dplyr::select(trp_id, name, road_reference, road_link_position, lat, lon,
                  legacyNortrafMpn) %>%
    dplyr::mutate(road_reference = str_replace(road_reference, "HP ", "hp")) %>%
    dplyr::mutate(road_reference = str_replace(road_reference, "Meter ", "m"))

  return(points_trp)
}

getPointsFromTRPAPI_filtered <- function() {
  # Get all traffic registration points
  query_points_trp <-
    "query hentTRP{
  trafficRegistrationPoints(stationType: PERIODIC, trafficType: VEHICLE) {
    id
  }
}"

  myqueries <- Query$new()
  myqueries$query("points_trp", query_points_trp)

  points_trp <- cli_trp$exec(myqueries$queries$points_trp) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    dplyr::rename(
      trp_id = id)

  return(points_trp)
}

get_trp_for_vti <- function() {

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
      county {
        number
        name
      }
    }
    legacyNortrafMpn
    commissionElements {
      commission {
        validFrom
        validTo
      }
    }
  }
}"

  myqueries <- Query$new()
  myqueries$query("api_data", api_query)

  points_trp <- cli_trp$exec(myqueries$queries$api_data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
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
      legacyNortrafMpn = data.trafficRegistrationPoints.legacyNortrafMpn,
      valid_from = commission.validFrom,
      valid_to = commission.validTo,
      county_number = data.trafficRegistrationPoints.location.county.number,
      county_name = data.trafficRegistrationPoints.location.county.name,
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
    dplyr::select(county_number, county_name, trp_id, legacyNortrafMpn, name,
                  road_reference, road_category, #road_number, parsell, meter,
                  road_link_position, lat, lon, first_commission_datainn) %>%
    dplyr::arrange(county_number, road_category, road_reference,
                   #road_number, parsell, meter
                   ) %>%
    dplyr::ungroup()

  return(points_trp)
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
        roadCategory {
          id
        }
      }
      county {
        number
        name
      }
    }
    roadReferenceHistory {
      validFrom
      validTo
      roadReference {
        shortForm
      }
    }
    legacyNortrafMpn
    commissionElements {
      commission {
        validFrom
        validTo
      }
    }
  }
}"

  myqueries <- Query$new()
  myqueries$query("api_data", api_query)

  points_trp <- cli_trp$exec(myqueries$queries$api_data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    tidyr::unnest(cols =
                    c("data.trafficRegistrationPoints.commissionElements")) %>%
    dplyr::rename(
      trp_id = data.trafficRegistrationPoints.id,
      name = data.trafficRegistrationPoints.name,
      road_system_reference =
        data.trafficRegistrationPoints.location.roadReference.shortForm,
      legacyNortrafMpn = data.trafficRegistrationPoints.legacyNortrafMpn,
      valid_from = commission.validFrom,
      valid_to = commission.validTo,
      county_number = data.trafficRegistrationPoints.location.county.number,
      county_name = data.trafficRegistrationPoints.location.county.name,
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
    dplyr::mutate(first_commission_datainn = lubridate::floor_date(
                    valid_from, unit = "day")) %>%
    dplyr::select(-valid_from, - valid_to) %>%
    tidyr::unnest(cols = "data.trafficRegistrationPoints.roadReferenceHistory") %>%
    # Removing points without road_reference
    dplyr::filter(!is.na(validFrom)) %>%
    dplyr::mutate(validFrom = lubridate::floor_date(
      lubridate::with_tz(
        lubridate::ymd_hms(validFrom)),
      unit = "second")) %>%
    dplyr::slice(which.max(validFrom)) %>%
    dplyr::select(-validTo, -validFrom) %>%
    dplyr::rename(road_reference = roadReference.shortForm) %>%
    dplyr::select(county_number, county_name, trp_id, legacyNortrafMpn, name,
                  road_category, road_system_reference, road_reference,
                  first_commission_datainn) %>%
    dplyr::arrange(county_number, road_category, road_system_reference) %>%
    dplyr::ungroup()

  return(points_trp)
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

get_stations_and_trps_with_coordinates_from_TRPAPI <- function() {
  # Get all traffic registration stations
  query_points_trp <-
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

  myqueries <- Query$new()
  myqueries$query("points_trp", query_points_trp)

  points_trp <- cli_trp$exec(myqueries$queries$points_trp) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
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

get_manual_labels <- function() {
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
  query_points_trp <-
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
  # Get all trs' and their device type history
  query_trs <-
    "query all_trs{trafficRegistrationStations (withTrps: true) {
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

myqueries <- Query$new()
myqueries$query("points_trp", query_trs)

trs <- cli_trp$exec(myqueries$queries$points_trp) %>%
  jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
  as.data.frame() %>%
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

get_trp_with_commissions <- function() {
  # Get all trp's and their commissions (and trs)
  query_points_trp <-
    "query trp_commissions {
  trafficRegistrationPoints {
    id
    name
    legacyNortrafMpn
    commissionElements{
      commission{
        trs{
          id
          name
          willBeIncompatibleWithNortraf
        }
        validFrom
        validTo
      }
    }
  }
}
"

myqueries <- Query$new()
myqueries$query("points_trp", query_points_trp)

points_trp <- cli_trp$exec(myqueries$queries$points_trp) %>%
  jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
  as.data.frame()

trps <- points_trp %>%
  tidyr::unnest(cols = c(data.trafficRegistrationPoints.commissionElements)) %>%
  dplyr::rename(trp_id = 1,
                trp_name = 2,
                legacyNortrafMpn = 3,
                commission_valid_from = 4,
                commission_valid_to = 5,
                trs_id = 6,
                trs_name = 7,
                incompatible_with_nortraf = 8)

return(trps)
}

get_trs_info <- function() {

  query_trs <-
    "query trs {
  trafficRegistrationStations {
    id
    name
    operationalStatus
    stationType
    trafficType
  }
}
"

myqueries <- Query$new()
myqueries$query("trs", query_trs)

trs <- cli_trp$exec(myqueries$queries$trs) %>%
  jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
  as.data.frame() %>%
  dplyr::rename(trs_id = 1,
                name = 2,
                status = 3,
                station_type = 4,
                traffic_type = 5)

return(trs)
}

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