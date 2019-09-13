# Fetching data from Trafikkdata-API or TRP-API

library(ghql)
library(lubridate)

cli <- GraphqlClient$new(
  url = "https://www.vegvesen.no/trafikkdata/api/?query="
)

getPoints <- function() {
  # Get all traffic registration points
  query_points <-
    "query allPoints{
  trafficRegistrationPoints{
    id
    name
    trafficRegistrationType
    location{
      coordinates{
        latLon{
          lat
          lon
        }
      }
      currentRoadReference{
        roadReference532{
          shortForm
        }
      }
      roadNetworkReference{
        atPosition
        networkElementId
      }
    }
    commissions{
      validFrom
      validTo
    }
  }
}"

  myqueries <- Query$new()
  myqueries$query("points", query_points)

  points <- cli$exec(myqueries$queries$points) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    tidyr::unnest() %>%
    dplyr::rename(trp_id =
                    data.trafficRegistrationPoints.id,
                  name =
                    data.trafficRegistrationPoints.name,
                  traffic_type =
                    data.trafficRegistrationPoints.trafficRegistrationType,
                  lat =
                    data.trafficRegistrationPoints.location.coordinates.latLon.lat,
                  lon =
                    data.trafficRegistrationPoints.location.coordinates.latLon.lon,
                  road_reference =
                    data.trafficRegistrationPoints.location.currentRoadReference.roadReference532.shortForm,
                  road_network_position =
                    data.trafficRegistrationPoints.location.roadNetworkReference.atPosition,
                  road_network_link =
                    data.trafficRegistrationPoints.location.roadNetworkReference.networkElementId
                    ) %>%
    dplyr::select(trp_id, name, traffic_type, road_reference, lat, lon,
                  road_network_position, road_network_link, validFrom, validTo
                  ) %>%
    dplyr::mutate(road_reference = str_replace(road_reference, "HP ", "hp")
                  ) %>%
    dplyr::mutate(road_reference = str_replace(road_reference, "Meter ", "m"),
                  road_link_position = paste0(road_network_position, "@",
                                              road_network_link),
                  validFrom =
                    floor_date(with_tz(ymd_hms(validFrom)), unit = "day"),
                  validTo = floor_date(with_tz(ymd_hms(validTo)), unit = "day")
                  )

  return(points)
}

#trp_id = "55265V521064"

getTrpAadt <- function(trp_id) {
  # Get all AADTs for a trp
  query_aadt <- paste0(
    "query trp_adt{
    trafficData(trafficRegistrationPointId: \"", trp_id,"\"){
      trafficRegistrationPoint{
        id
      }
      volume{
        average{
          daily{
            byYear{
              year
              total{
                volume{
                  standardDeviation
                  average
                }
              }
            }
          }
        }
      }
    }
  }")

  myqueries <- Query$new()
  myqueries$query("aadts", query_aadt)

  # TODO: Må splitte opp her med en test om det ikke er noe ÅDT
  trp_aadt <- cli$exec(myqueries$queries$aadts) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  if(is_empty(trp_aadt$data$trafficData$volume$average$daily$byYear)){
    # hva gjør vi når det ikke er noe ÅDT?
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      tidyr::unnest() %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byYear.year,
        adt = data.trafficData.volume.average.daily.byYear.total.volume.average,
        sd = data.trafficData.volume.average.daily.byYear.total.volume.standardDeviation) %>%
      dplyr::mutate(trp_id = as.character(trp_id))
  }

  return(trp_aadt)
}

#trp_id <- "98963V1719019"

getTrpAadt_byLength <- function(trp_id) {
  # Get all AADTs for a trp
  query_aadt <- paste0(
    "query trp_adt {
    trafficData(trafficRegistrationPointId: \"", trp_id,"\") {
      trafficRegistrationPoint {
      id
    }
    volume {
      average {
        daily {
          byYear {
            year
            total {
              validLengthVolume {
                average
              }
              coverage {
                percentage
              }
              volume {
                average
              }
            }
            byLengthRange {
              lengthRange{
                representation
              }
              total {
                volume {
                  average
                  standardDeviation
                  confidenceInterval{
                    lowerBound
                    upperBound
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
")

  myqueries <- Query$new()
  myqueries$query("aadts", query_aadt)

  trp_aadt <- cli$exec(myqueries$queries$aadts) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  if(is_empty(trp_aadt$data$trafficData$volume$average$daily$byYear)){
    # hva gjør vi når det ikke er noe ÅDT?
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      tidyr::unnest() %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = 2,
        aadt_valid_length = 3,
        coverage = 4,
        aadt_total = 5,
        length_range = 6,
        aadt_length_range = 7,
        sd_length_range = 8,
        aadt_ci_lowerbound_length_range = 9,
        aadt_ci_upperbound_length_range = 10
        ) %>%
      dplyr::mutate(trp_id = as.character(trp_id))
  }

  return(trp_aadt)
}


# Hente ÅDt for mange punkter
getAdtForpoints <- function(trp_list) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             getTrpAadt(trp_list[trp_count]))
    trp_count <- trp_count + 1
  }

  trp_adt <- data_points %>%
    dplyr::mutate(adt = round(adt, digits = -1))

  return(trp_adt)
}

getAdtForpoints_by_length <- function(trp_list) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             getTrpAadt_byLength(trp_list[trp_count]))
    trp_count <- trp_count + 1
  }

  trp_adt <- data_points %>%
    dplyr::mutate(aadt_valid_length = round(aadt_valid_length, digits = -1),
                  aadt_total = round(aadt_total, digits = -1),
                  aadt_length_range = round(aadt_length_range, digits = -1),
                  aadt_ci_lowerbound_length_range =
                    round(aadt_ci_lowerbound_length_range, digits = -1),
                  aadt_ci_upperbound_length_range =
                    round(aadt_ci_upperbound_length_range, digits = -1)
                  )

  return(trp_adt)
}

