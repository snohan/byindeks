# Fetching data from Trafikkdata-API or TRP-API

library(ghql)
library(lubridate)
library(magrittr)

cli <- GraphqlClient$new(
  url = "https://www.vegvesen.no/trafikkdata/api/?query="
)

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
                    round(aadt_ci_upperbound_length_range, digits = -1),
                  aadt_ci_upperbound_total =
                    round(aadt_ci_upperbound_total, digits = -1),
                  aadt_ci_lowerbound_total =
                    round(aadt_ci_lowerbound_total, digits = -1)
                  )

  return(trp_adt)
}

#indexyear <- "2019"
#trp_ids <- "\"44656V72812\", \"77022V72359\""
#trp_ids <- "90390V443603"

get_pointindices <- function(trp_ids, indexyear) {
  # Get pointindex for a number of trps
  api_query <- paste0(
    "query pointindex{
      trafficVolumeIndices(
        trafficRegistrationPointIds: [\"", trp_ids, "\"],
        year: ", indexyear, ") {
        calculationMonth {
          month
          year
        }
        roadCategory {
          id
        }
        trafficRegistrationPoint {
          id
        }
        volumeIndexByDayType {
          dayType
          trafficVolumeIndex {
            index {
              baseVolume
              calculationVolume
              indexNumber
              percentageChange
            }
            lengthRange {
              representation
            }
            indexCoverage {
              hours {
                percentage
                numerator
                denominator
              }
            }
          }
        }
      }
    }")

  myqueries <- Query$new()
  myqueries$query("data", api_query)

  trp_data <- cli$exec(myqueries$queries$data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  trp_data_data <- trp_data$data %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(trafficVolumeIndices.volumeIndexByDayType)) %>%
    dplyr::rename(
      day_type = dayType,
      base_volume = trafficVolumeIndex.index.baseVolume,
      calculation_volume = trafficVolumeIndex.index.calculationVolume,
      index_i = trafficVolumeIndex.index.indexNumber,
      index_p = trafficVolumeIndex.index.percentageChange,
      length_range = trafficVolumeIndex.lengthRange.representation,
      coverage_percentage = trafficVolumeIndex.indexCoverage.hours.percentage,
      month = trafficVolumeIndices.calculationMonth.month,
      year = trafficVolumeIndices.calculationMonth.year,
      road_category = trafficVolumeIndices.roadCategory.id,
      trp_id = trafficVolumeIndices.trafficRegistrationPoint.id) %>%
    dplyr::mutate(trp_id = as.character(trp_id)) %>%
    dplyr::select(trp_id, month, year, road_category, day_type, length_range,
                  base_volume, calculation_volume, index_i, index_p,
                  coverage_percentage) %>%
    dplyr::filter(length_range %in% c("[..,..)", "[..,5.6)", "[5.6,..)")) %>%
    dplyr::filter(day_type == "ALL")

  return(trp_data_data)
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


