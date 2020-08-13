# Fetching data from Trafikkdata-API or TRP-API

library(tidyverse)
library(ghql)
library(lubridate)
library(magrittr)

cli <- GraphqlClient$new(
  url = "https://www.vegvesen.no/trafikkdata/api/?query="#,
  #headers = list(
  #  'content-type' = 'application/json')
)

get_counties <- function() {
  # Get all counties
  query_points <-
    "query counties {
       areas {
         counties {
           number
           name
         }
       }
    }"

  myqueries <- Query$new()
  myqueries$query("points", query_points)

  counties <- cli$exec(myqueries$queries$points) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    dplyr::rename(county_number =
                    data.areas.counties.number,
                  county_name =
                    data.areas.counties.name
    ) %>%
    mutate(geo_number = case_when(
      county_number ==  3 ~ 1,
      county_number == 30 ~ 2,
      county_number == 34 ~ 3,
      county_number == 38 ~ 4,
      county_number == 42 ~ 5,
      county_number == 11 ~ 6,
      county_number == 46 ~ 7,
      county_number == 15 ~ 8,
      county_number == 50 ~ 9,
      county_number == 18 ~ 10,
      county_number == 54 ~ 11
    )) %>%
    arrange(geo_number)

  return(counties)
}

get_municipalities <- function() {

  query_api <-
    "query municipalities {
       areas {
         municipalities {
           number
           name
         }
       }
     }"

  myqueries <- Query$new()
  myqueries$query("response", query_api)

  counties <- cli$exec(myqueries$queries$response) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    dplyr::rename(municipality_number = 1,
                  municipality_name = 2)
}


get_points <- function() {
  # Get all traffic registration points
  query_points <-
    "query all_trps {
  trafficRegistrationPoints {
    id
    name
    trafficRegistrationType
    location {
      coordinates {
        latLon {
          lat
          lon
        }
      }
      county {
        name
        number
      }
      municipality {
        name
        number
      }
      roadReference {
          shortForm
      }
      roadLinkSequence {
        relativePosition
        roadLinkSequenceId
      }
    }
    commissions {
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
    tidyr::unnest(cols = c(data.trafficRegistrationPoints.commissions)) %>%
    dplyr::rename(trp_id =
                    data.trafficRegistrationPoints.id,
                  name =
                    data.trafficRegistrationPoints.name,
                  traffic_type =
                    data.trafficRegistrationPoints.trafficRegistrationType,
                  county_name = data.trafficRegistrationPoints.location.county.name,
                  county_no = data.trafficRegistrationPoints.location.county.number,
                  municipality_name = data.trafficRegistrationPoints.location.municipality.name,
                  municipality_no = data.trafficRegistrationPoints.location.municipality.number,
                  lat =
                    data.trafficRegistrationPoints.location.coordinates.latLon.lat,
                  lon =
                    data.trafficRegistrationPoints.location.coordinates.latLon.lon,
                  road_reference =
                    data.trafficRegistrationPoints.location.roadReference.shortForm,
                  road_network_position =
                    data.trafficRegistrationPoints.location.roadLinkSequence.relativePosition,
                  road_network_link =
                    data.trafficRegistrationPoints.location.roadLinkSequence.roadLinkSequenceId
                    ) %>%
    dplyr::select(trp_id, name, traffic_type, road_reference, county_name,
                  county_no, municipality_name, municipality_no, lat, lon,
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

get_points_2 <- function() {
  # The thought was to fetch individual components of the road system reference,
  # but those are not available in the API...
  # Get all traffic registration points
  query_points <-
    "query all_trps {
  trafficRegistrationPoints {
    id
    name
    trafficRegistrationType
    location {
      coordinates {
        latLon {
          lat
          lon
        }
      }
      county {
        name
        number
      }
      municipality {
        name
        number
      }
      roadReference {
          shortForm
      }
      roadLinkSequence {
        relativePosition
        roadLinkSequenceId
      }
    }
    commissions {
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
    tidyr::unnest(cols = c(data.trafficRegistrationPoints.commissions)) %>%
    dplyr::rename(trp_id =
                    data.trafficRegistrationPoints.id,
                  name =
                    data.trafficRegistrationPoints.name,
                  traffic_type =
                    data.trafficRegistrationPoints.trafficRegistrationType,
                  county_name = data.trafficRegistrationPoints.location.county.name,
                  county_no = data.trafficRegistrationPoints.location.county.number,
                  municipality_name = data.trafficRegistrationPoints.location.municipality.name,
                  municipality_no = data.trafficRegistrationPoints.location.municipality.number,
                  lat =
                    data.trafficRegistrationPoints.location.coordinates.latLon.lat,
                  lon =
                    data.trafficRegistrationPoints.location.coordinates.latLon.lon,
                  road_reference =
                    data.trafficRegistrationPoints.location.roadReference.shortForm,
                  road_network_position =
                    data.trafficRegistrationPoints.location.roadLinkSequence.relativePosition,
                  road_network_link =
                    data.trafficRegistrationPoints.location.roadLinkSequence.roadLinkSequenceId
    ) %>%
    dplyr::select(trp_id, name, traffic_type, road_reference, county_name,
                  county_no, municipality_name, municipality_no, lat, lon,
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


get_trps_latest_data <- function() {
  # Get all traffic registration points
  query_points <-
    "query trp_latest_data {
  trafficRegistrationPoints {
    id
    latestData {
      volumeByHour
    }
  }
}"

  myqueries <- Query$new()
  myqueries$query("points", query_points)

  points <- cli$exec(myqueries$queries$points) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    dplyr::rename(trp_id =
                    data.trafficRegistrationPoints.id,
                  latest_data_by_hour =
                    data.trafficRegistrationPoints.latestData.volumeByHour
    ) %>%
    dplyr::mutate(latest_data_by_hour =
                    floor_date(with_tz(ymd_hms(latest_data_by_hour)),
                               unit = "hour")
    )

  return(points)
}

#trp_id = "55265V521064"
#trp_id <- "43849B2033722"

getTrpAadt <- function(trp_id) {
  # Get all AADTs for a trp
  # Might be replaced by next function, or keep it for bike
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
                coverage {
                  percentage
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

  trp_aadt <- cli$exec(myqueries$queries$aadts) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  if(is_empty(trp_aadt$data$trafficData$volume$average$daily$byYear)){
    # hva gjør vi når det ikke er noe ÅDT?
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      #tidyr::unnest() %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byYear.year,
        adt = data.trafficData.volume.average.daily.byYear.total.volume.average,
        sd = data.trafficData.volume.average.daily.byYear.total.volume.standardDeviation,
        coverage = data.trafficData.volume.average.daily.byYear.total.coverage.percentage) %>%
      dplyr::mutate(trp_id = as.character(trp_id))
  }

  return(trp_aadt)
}

get_trp_aadt_with_coverage <- function(trp_id) {
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
            coverage{
              percentage
            }
            validLengthVolume{
              average
            }
            validSpeedVolume{
              average
            }
            volume{
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

  if(is_empty(trp_aadt$data$trafficData$volume$average$daily$byYear) |
     is.null(trp_aadt$data$trafficData$volume$average$daily$byYear$total.volume.average)
     #ncol(trp_aadt$data$trafficData$volume$average$daily$byYear) < 5
     ){
    # hva gjør vi når det ikke er noe ÅDT?
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byYear.year,
        coverage = 3,
        valid_length_volume = 4,
        valid_speed_volume = 5,
        adt = 6) %>%
      dplyr::mutate(trp_id = as.character(trp_id))
  }

  return(trp_aadt)
}

get_trp_mdt_with_coverage <- function(trp_id, mdt_year) {
  # Get all MDTs for a trp
  query_aadt <- paste0(
    "query trp_mdt{
    trafficData(trafficRegistrationPointId: \"", trp_id,"\"){
      trafficRegistrationPoint{
        id
      }
      volume{
    average{
      daily{
        byMonth(dayType: ALL, year: ", mdt_year, "){
          year
          month
          total{
            coverage{
              percentage
            }
            validLengthVolume{
              average
            }
            validSpeedVolume{
              average
            }
            volume{
              average
              confidenceInterval{
                confidenceWidth
              }
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

  if(is_empty(trp_aadt$data$trafficData$volume$average$daily$byMonth) |
    # ncol(trp_aadt$data$trafficData$volume$average$daily$byMonth) < 5)
    is.null(trp_aadt$data$trafficData$volume$average$daily$byMonth$total.volume.average)){
    # Når det ikke er noe MDT
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byMonth.year,
        month = 3,
        coverage = 4,
        valid_length_volume = 5,
        valid_speed_volume = 6,
        mdt = 7,
        confidence_width = 8) #%>%
      #dplyr::mutate(trp_id = as.character(trp_id),
       #             coverage = round(coverage, digits = 1),
      #              uncertainty = signif(uncertainty, 2))
  }

  return(trp_aadt)
}


#mdt_year <- "2020"
get_trp_mdt_by_lane <- function(trp_id, mdt_year) {
  # Get all MDTs for a trp
  query_mdt <- paste0(
    "query trp_mdt{
    trafficData(trafficRegistrationPointId: \"", trp_id,"\"){
      trafficRegistrationPoint{
        id
      }
      volume{
    average{
      daily{
        byMonth(dayType: ALL, year: ", mdt_year, "){
          year
          month
          byLane {
              lane {
                laneNumber
              }
          total{
            coverage{
              percentage
            }
            validLengthVolume{
              average
            }
            validSpeedVolume{
              average
            }
            volume{
              average
              confidenceInterval{
                confidenceWidth
              }
            }
          }
        }
      }
    }
  }
}
}
}")

  myqueries <- Query$new()
  myqueries$query("mdts", query_mdt)

  # TODO: Må splitte opp her med en test om det ikke er noe verdi
  trp_aadt <- cli$exec(myqueries$queries$mdts) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  if(is_empty(trp_aadt$data$trafficData$volume$average$daily$byMonth$byLane[1]) |
     # ncol(trp_aadt$data$trafficData$volume$average$daily$byMonth) < 5)
     is.null(trp_aadt$data$trafficData$volume$average$daily$byMonth$byLane[[1]]$total.volume.average)){
    # Når det ikke er noe MDT
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      tidyr::unnest(cols = data.trafficData.volume.average.daily.byMonth.byLane) %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byMonth.year,
        month = data.trafficData.volume.average.daily.byMonth.month,
        lane = 4,
        coverage = 5,
        valid_length_volume = 6,
        valid_speed_volume = 7,
        mdt = 8,
        confidence_width = 9) %>%
    dplyr::mutate(trp_id = as.character(trp_id))#,
    #             coverage = round(coverage, digits = 1),
    #              uncertainty = signif(uncertainty, 2))
  }

  return(trp_aadt)
}

#mdt_test <- get_trp_mdt_with_coverage("91582V930281", "2020")
#mdt_test_2 <- get_trp_mdt_by_lane("91582V930281", "2020")

#trp_id <- "91582V930281"
#trp_id <- "01316V804837"
#trp_adt <- getTrpAadt_byLength(trp_id)

get_aadt_by_length_for_trp <- function(trp_id) {
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
                confidenceInterval {
                  lowerBound
                  upperBound
                }
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

  if(is_empty(trp_aadt$data$trafficData$volume$average$daily$byYear) |
     is_empty(trp_aadt$data$trafficData$volume$average$daily$byYear$byLengthRange[[1]])
     ){
    # hva gjør vi når det ikke er noe ÅDT?
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      tidyr::unnest(cols = c(data.trafficData.volume.average.daily.byYear.byLengthRange)) %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byYear.year,
        length_range = lengthRange.representation,
        aadt_length_range = total.volume.average,
        sd_length_range = total.volume.standardDeviation,
        aadt_ci_lowerbound_length_range = total.volume.confidenceInterval.lowerBound,
        aadt_ci_upperbound_length_range = total.volume.confidenceInterval.upperBound,
        aadt_valid_length = data.trafficData.volume.average.daily.byYear.total.validLengthVolume.average,
        coverage = data.trafficData.volume.average.daily.byYear.total.coverage.percentage,
        aadt_total = data.trafficData.volume.average.daily.byYear.total.volume.average,
        aadt_ci_lowerbound_total = data.trafficData.volume.average.daily.byYear.total.volume.confidenceInterval.lowerBound,
        aadt_ci_upperbound_total = data.trafficData.volume.average.daily.byYear.total.volume.confidenceInterval.upperBound
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

get_aadt_for_trp_list <- function(trp_list) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             get_trp_aadt_with_coverage(trp_list[trp_count]))
    trp_count <- trp_count + 1
  }

  trp_adt <- data_points %>%
    dplyr::mutate(adt = round(adt, digits = -1))

  return(trp_adt)
}

get_mdt_for_trp_list <- function(trp_list, mdt_year) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             get_trp_mdt_with_coverage(
                               trp_list[trp_count],
                               mdt_year))
    trp_count <- trp_count + 1
  }

  trp_mdt <- data_points %>%
    dplyr::mutate(mdt = round(mdt, digits = -1))

  return(trp_mdt)
}

get_mdt_by_lane_for_trp_list <- function(trp_list, mdt_year) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             get_trp_mdt_by_lane(
                               trp_list[trp_count],
                               mdt_year))
    trp_count <- trp_count + 1
  }

  trp_mdt <- data_points %>%
    dplyr::mutate(mdt = round(mdt, digits = -1))

  return(trp_mdt)
}

#trp_list <- trp_distinct$trp_id[1:2]
#test_list <- trp_id = c("91582V930281", "01316V804837")
#test <- get_mdt_by_lane_for_trp_list(trp_list, "2020")
#test_adt <- getAdtForpoints_by_length(test_list)

get_aadt_by_length_for_trp_list <- function(trp_list) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             get_aadt_by_length_for_trp(trp_list[trp_count]))
    trp_count <- trp_count + 1
  }

  trp_adt <- data_points %>%
    dplyr::mutate(aadt_valid_length = round(aadt_valid_length, digits = -1),
                  aadt_total = round(aadt_total, digits = -1),
                  aadt_length_range = round(aadt_length_range, digits = -1),
                  aadt_ci_lowerbound_length_range =
                    round(aadt_ci_lowerbound_length_range, digits = -1),
                  aadt_ci_upperbound_length_range =
                    round(aadt_ci_upperbound_length_range, digits = -1),
                  aadt_ci_upperbound_total =
                    round(aadt_ci_upperbound_total, digits = -1),
                  aadt_ci_lowerbound_total =
                    round(aadt_ci_lowerbound_total, digits = -1)
                  )

  return(trp_adt)
}

#indexyear <- "2020"
#trp_ids <- "\"44656V72812\", \"77022V72359\""
#trp_ids <- "35258V2475662"

get_pointindices <- function(trp_ids, indexyear) {
  # Get pointindex for trps
  api_query <- paste0(
    "query pointindex{
      trafficVolumeIndices(
        trafficRegistrationPointIds: [\"", trp_ids, "\"],
        year: ", indexyear, ") {
        trafficRegistrationPoint {
      id
      }
      calculationMonth {
        year
        month
      }
      roadCategory {
        id
      }
      volumeIndicesMonth {
      	...pointIndexFields
      }
      volumeIndicesYearToDate {
      	...pointIndexFields
      }
      volumeIndicesLast12Months {
        ...pointIndexFields
      }
    }
  }

  fragment pointIndexFields on TrafficVolumeIndexByDayType {
    dayType
    isExcluded
    totalTrafficVolumeIndex {
      indexNumber {
        index {
          indexNumber
          percentageChange
        }
        lengthRange {
          representation
        }
      }
      indexCoverage {
        hours {
          percentage
        }
      }
    }
    lengthRangesTrafficVolumeIndex {
      isExcluded
      indexNumbers {
        index {
          indexNumber
          percentageChange
        }
        lengthRange {
          representation
        }
      }
      indexCoverage {
        hours {
          percentage
        }
      }
    }
  }")

  myqueries <- Query$new()
  myqueries$query("data", api_query)

  trp_data <- cli$exec(myqueries$queries$data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  # Unwrap one part a time:
  # 1. month
  # 2. year to date
  # 3. last 12

  trp_data_data_1 <- trp_data$data %>%
    as.data.frame() %>%
    dplyr::select(trafficVolumeIndices.trafficRegistrationPoint.id,
                  trafficVolumeIndices.calculationMonth.year,
                  trafficVolumeIndices.calculationMonth.month,
                  trafficVolumeIndices.roadCategory.id,
                  trafficVolumeIndices.volumeIndicesMonth) %>%
    tidyr::unnest(cols = c(trafficVolumeIndices.volumeIndicesMonth)) %>%
    # Keeping only rows with actual results
    dplyr::filter(isExcluded == "FALSE") %>%
    dplyr::mutate(period = "month")

  trp_data_data_2 <- trp_data$data %>%
    as.data.frame() %>%
    dplyr::select(trafficVolumeIndices.trafficRegistrationPoint.id,
                  trafficVolumeIndices.calculationMonth.year,
                  trafficVolumeIndices.calculationMonth.month,
                  trafficVolumeIndices.roadCategory.id,
                  trafficVolumeIndices.volumeIndicesYearToDate) %>%
    tidyr::unnest(cols = c(trafficVolumeIndices.volumeIndicesYearToDate)) %>%
    # Keeping only rows with actual results
    dplyr::filter(isExcluded == "FALSE")%>%
    dplyr::mutate(period = "year_to_date")

  trp_data_data_3 <- trp_data$data %>%
    as.data.frame() %>%
    dplyr::select(trafficVolumeIndices.trafficRegistrationPoint.id,
                  trafficVolumeIndices.calculationMonth.year,
                  trafficVolumeIndices.calculationMonth.month,
                  trafficVolumeIndices.roadCategory.id,
                  trafficVolumeIndices.volumeIndicesLast12Months) %>%
    tidyr::unnest(cols = c(trafficVolumeIndices.volumeIndicesLast12Months)) %>%
    # Keeping only rows with actual results
    dplyr::filter(isExcluded == "FALSE")%>%
    dplyr::mutate(period = "last_12_months")

trp_data_data_all <- dplyr::bind_rows(trp_data_data_1,
                                      trp_data_data_2,
                                      trp_data_data_3)


  if(nrow(trp_data_data_all) == 0){
      # hva gjør vi når det ikke er noen indekser?
      trp_data_data_all <- data.frame()
    }else{
      trp_data_data_all <- trp_data_data_all %>%
        tidyr::unnest(cols = c(lengthRangesTrafficVolumeIndex.indexNumbers)) %>%
        dplyr::select(trp_id = trafficVolumeIndices.trafficRegistrationPoint.id,
                      year = trafficVolumeIndices.calculationMonth.year,
                      month = trafficVolumeIndices.calculationMonth.month,
                      road_category = trafficVolumeIndices.roadCategory.id,
                      day_type = dayType,
                      period = period,
                      index_total = totalTrafficVolumeIndex.indexNumber.index.indexNumber,
                      index_total_p = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
                      index_total_coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
                      length_is_excluded = lengthRangesTrafficVolumeIndex.isExcluded,
                      length_range = lengthRange.representation,
                      #length_index = index.indexNumber,
                      length_index_p = index.percentageChange,
                      length_index_coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage) %>%
        dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
        dplyr::mutate(length_range = dplyr::if_else(length_range == "[..,5.6)",
                                                    "short", "long")) %>%
        tidyr::pivot_wider(names_from = length_range, names_prefix = "index_",
                           values_from = length_index_p)
    }

  return(trp_data_data_all)
}

getHourlytraffic <- function(trpID, from, to) {
  # Default values
  hasNextPage <- TRUE
  cursor <- ""
  hourlyTraffic <- data.frame()

  build_query <- function() {
    query_hourlyTraffic <- paste0(
      'query point_hour_volumes {
    trafficData(trafficRegistrationPointId: "',
      trpID,
      '"){
        trafficRegistrationPoint {
          id
          name
        }
        volume {
        byHour(
        from: "',
      from,
      '",
        to: "',
      to,
      '",
        after: "',
      cursor,
      '"
        ) {
        edges {
          node {
            from
            total {
              volume
              coverage {
                percentage
              }
            }
              }
              }
              pageInfo {
                hasNextPage
                endCursor
              }
              }
              }
            }
        }
      ')
  }

  while(hasNextPage == TRUE){

    myqueries <- Query$new()
    myqueries$query("hourlyTraffic", build_query())

    trafficData <- cli$exec(myqueries$queries$hourlyTraffic) %>%
      fromJSON(simplifyDataFrame = T, flatten = T)

    if(length(trafficData$data$trafficData$volume$byHour$edges) == 0)
      break;

    trafficData %<>% as.data.frame()

    cursor <-
      trafficData$data.trafficData.volume.byHour.pageInfo.endCursor[1] %>%
      as.character()
    hasNextPage <-
      trafficData$data.trafficData.volume.byHour.pageInfo.hasNextPage[1]

    trafficData %<>% select(1:5)

    hourlyTraffic <- bind_rows(hourlyTraffic, trafficData)
  }

  if(nrow(hourlyTraffic) == 0) {
    hourlyTraffic <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                              c("point_id", "point_name", "hour_from",
                                "total_volume", "coverage"))
  }else{
    colnames(hourlyTraffic) <- c("point_id", "point_name", "hour_from",
                                 "total_volume", "coverage")
    hourlyTraffic %<>% mutate(hour_from = with_tz(ymd_hms(hour_from), "CET"))
  }

  return(hourlyTraffic)
}

# trpID <- "81077V72158"
# from <- "2020-06-09T00:00:00+01:00"
# to <- "2020-06-18T00:00:00+01:00"
# test1 <- get_daily_traffic(trps$trp_id[1], from, to)
# test2 <- get_daily_traffic(trps$trp_id[2], from, to)
#
# test_bind <- bind_rows(test1, test2)

get_daily_traffic <- function(trpID, from, to) {
  # Default values
  hasNextPage <- TRUE
  cursor <- ""
  dailyTraffic <- data.frame()

  build_query <- function() {
    query_traffic <- paste0(
      'query point_day_volumes {
    trafficData(trafficRegistrationPointId: "',
      trpID,
      '"){
        trafficRegistrationPoint {
          id
          name
        }
        volume {
        byDay(
        from: "',
      from,
      '",
        to: "',
      to,
      '",
        after: "',
      cursor,
      '"
        ) {
        edges {
          node {
            from
            total {
              volumeNumbers {
                volume
              }
              coverage {
                percentage
              }
            }
              }
              }
              pageInfo {
                hasNextPage
                endCursor
              }
              }
              }
            }
        }
      ')
  }

  while(hasNextPage == TRUE){

    myqueries <- Query$new()
    myqueries$query("dailyTraffic", build_query())

    trafficData <- cli$exec(myqueries$queries$dailyTraffic) %>%
      fromJSON(simplifyDataFrame = T, flatten = T)

    if(length(trafficData$data$trafficData$volume$byDay$edges) == 0)
      break;

    trafficData %<>% as.data.frame()

    cursor <-
      trafficData$data.trafficData.volume.byDay.pageInfo.endCursor[1] %>%
      as.character()
    hasNextPage <-
      trafficData$data.trafficData.volume.byDay.pageInfo.hasNextPage[1]

    trafficData %<>% select(1:5)

    dailyTraffic <- bind_rows(dailyTraffic, trafficData)
  }

  if(nrow(dailyTraffic) == 0) {
    dailyTraffic <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                              c("point_id", "point_name", "from",
                                "total_volume", "coverage"))
  }else{
    colnames(dailyTraffic) <- c("point_id", "point_name", "from",
                                 "total_volume", "coverage")
    #dailyTraffic %<>% mutate(from = with_tz(ymd_hms(from), "CET"))
  }

  # To avoid error when joining, cast column type
  dailyTraffic <- dailyTraffic %>%
    dplyr::mutate(point_id = as.character(point_id),
                  point_name = as.character(point_name),
                  from = with_tz(ymd_hms(from), "CET"),
                  total_volume = as.integer(total_volume),
                  coverage = as.numeric(coverage))

  return(dailyTraffic)
}


get_dt_for_trp_list <- function(trp_list, from, to) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             get_daily_traffic(
                               trp_list[trp_count],
                               from,
                               to))
    trp_count <- trp_count + 1
  }

  return(data_points)
}


#index_id <- 3953
#indexyear <- 2017
#indexmonth <- 12

get_published_index <- function(index_id, indexyear, indexmonth) {
  # Get published index for a given area, year and month
  api_query <- paste0(
    "query published_index {
      publishedAreaTrafficVolumeIndex (
        id: ", index_id, ",
        year: ", indexyear, ",
        month: ", indexmonth, ") {
        id
        name
        period {
          calculationMonth {
            year
            month
          }
        }
        aggregatedTrafficVolumeIndex {
          area {
            name
          }
          byRoadCategoryCombination(combinations:
          EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG) {
            roadCategoryCombination
            monthIndicesByDayType {
              ...indexFields
            }
            yearToDateIndicesByDayType {
              ...indexFields
            }
          }
        }
      }
    }

    fragment indexFields on AggregatedVolumeIndexByDayType {
      dayType
      byLengthRange {
        lengthRange {
          representation
        }
        volumeIndexNumber {
          percentageChange
        }
        confidenceInterval {
          confidenceWidth
        }
      }
    }
    ")

  myqueries <- Query$new()
  myqueries$query("data", api_query)

  trp_data <- cli$exec(myqueries$queries$data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  # Unwrap one part at a time
  # 1. monthIndicesByDayType
  # 2. yearToDateIndicesByDayType

  monthly_data <- trp_data$data %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(publishedAreaTrafficVolumeIndex.aggregatedTrafficVolumeIndex)) %>%
    tidyr::unnest(cols = c(byRoadCategoryCombination)) %>%
    tidyr::unnest(cols = c(monthIndicesByDayType)) %>%
    dplyr::select(-yearToDateIndicesByDayType) %>%
    tidyr::unnest(cols = c(byLengthRange)) %>%
    dplyr::rename(
      day_type = dayType,
      length_range = lengthRange.representation,
      #index_i = trafficVolumeIndex.index.indexNumber,
      index_p = volumeIndexNumber.percentageChange,
      confidence_width = confidenceInterval.confidenceWidth,
      area_name = area.name,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month
      ) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::select(area_name, year, month, length_range, index_p,
                  confidence_width) %>%
    dplyr::mutate(period = "month")

  year_to_date_data <- trp_data$data %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(publishedAreaTrafficVolumeIndex.aggregatedTrafficVolumeIndex)) %>%
    tidyr::unnest(cols = c(byRoadCategoryCombination)) %>%
    tidyr::unnest(cols = c(yearToDateIndicesByDayType)) %>%
    dplyr::select(-monthIndicesByDayType) %>%
    tidyr::unnest(cols = c(byLengthRange)) %>%
    dplyr::rename(
      day_type = dayType,
      length_range = lengthRange.representation,
      #index_i = trafficVolumeIndex.index.indexNumber,
      index_p = volumeIndexNumber.percentageChange,
      confidence_width = confidenceInterval.confidenceWidth,
      area_name = area.name,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month
    ) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::select(area_name, year, month, length_range, index_p,
                  confidence_width) %>%
    dplyr::mutate(period = "year_to_date")

  published_index <- bind_rows(monthly_data,
                               year_to_date_data)

  return(published_index)
}


get_published_pointindex <- function(index_id, indexyear, indexmonth) {
  # Get published index for a given area, year and month
  # Response is paginated if more than 100 points!
  # Pagination is ignored here
  # Returns: list with two elements; trp_ids, pointindices

  api_query <- paste0(
    "query published_index {
      publishedAreaTrafficVolumeIndex (
        id: ", index_id, ",
        year: ", indexyear, ",
        month: ", indexmonth, ") {
        id
        name
        period {
          calculationMonth {
            year
            month
          }
        }
        containsPointTrafficVolumeIndices (first: 100) {
          edges {
            node {
              pointTrafficVolumeIndex {
                trafficRegistrationPoint {
                  id
                }
                volumeIndicesMonth {
                  ...indexFields
                }
                volumeIndicesYearToDate {
                  ...indexFields
                }
              }
            }
          }
        }
      }
    }

    fragment indexFields on TrafficVolumeIndexByDayType {
      dayType
      isExcluded
      totalTrafficVolumeIndex {
        indexNumber {
          lengthRange {
            representation
          }
          index {
            percentageChange
          }
        }
        indexCoverage {
          hours {
            percentage
          }
        }
      }
    lengthRangesTrafficVolumeIndex {
      isExcluded
      indexNumbers {
        lengthRange {
          representation
        }
        index {
          percentageChange
        }
      }
      indexCoverage {
        hours {
          percentage
        }
      }
    }
  }")

  myqueries <- Query$new()
  myqueries$query("data", api_query)

  trp_data <- cli$exec(myqueries$queries$data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  # Unwrap one part at a time
  # 1. monthIndicesByDayType
  # 2. yearToDateIndicesByDayType

  unnested_data <- trp_data$data %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(
      publishedAreaTrafficVolumeIndex.containsPointTrafficVolumeIndices.edges))

  indexpoints <- unnested_data$node.pointTrafficVolumeIndex.trafficRegistrationPoint.id

   monthly_data <- unnested_data %>%
    tidyr::unnest(cols = c(node.pointTrafficVolumeIndex.volumeIndicesMonth)) %>%
    dplyr::select(-node.pointTrafficVolumeIndex.volumeIndicesYearToDate) %>%
    tidyr::unnest(cols = c(lengthRangesTrafficVolumeIndex.indexNumbers)) %>%
    dplyr::select(area_name = publishedAreaTrafficVolumeIndex.name,
                  trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
                  year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
                  month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month,
                  day_type = dayType,
                  is_excluded = isExcluded,
                  total_index = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
                  coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
                  length_excluded = lengthRangesTrafficVolumeIndex.isExcluded,
                  length_range = lengthRange.representation,
                  length_index = index.percentageChange,
                  length_coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage
                  ) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
    dplyr::mutate(length_range = if_else(length_range == "[..,5.6)",
                                         "short", "long")) %>%
    tidyr::pivot_wider(names_from = length_range, names_prefix = "index_",
                       values_from = length_index) %>%
    dplyr::mutate(period = "month")


   # HERE

    year_to_date_data <- unnested_data %>%
      tidyr::unnest(cols = c(node.pointTrafficVolumeIndex.volumeIndicesYearToDate)) %>%
      dplyr::select(- node.pointTrafficVolumeIndex.volumeIndicesMonth) %>%
      tidyr::unnest(cols = c(lengthRangesTrafficVolumeIndex.indexNumbers)) %>%
      dplyr::select(area_name = publishedAreaTrafficVolumeIndex.name,
                    trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
                    year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
                    month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month,
                    day_type = dayType,
                    is_excluded = isExcluded,
                    total_index = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
                    coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
                    length_excluded = lengthRangesTrafficVolumeIndex.isExcluded,
                    length_range = lengthRange.representation,
                    length_index = index.percentageChange,
                    length_coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage
      ) %>%
      dplyr::filter(day_type == "ALL") %>%
      dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
      dplyr::mutate(length_range = if_else(length_range == "[..,5.6)",
                                           "short", "long")) %>%
      tidyr::pivot_wider(names_from = length_range, names_prefix = "index_",
                         values_from = length_index) %>%
      dplyr::mutate(period = "year_to_date")

  published_index <- bind_rows(monthly_data,
                               year_to_date_data)

  published_points <- list(indexpoints, published_index)

  return(published_points)
}

