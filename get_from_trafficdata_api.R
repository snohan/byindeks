# Fetching data from Trafikkdata-API or TRP-API

# Libraries and helper functions ####
library(tidyverse)
library(jsonlite)
library(ghql)
library(lubridate)
library(magrittr)

cli <- ghql::GraphqlClient$new(
  url = "https://trafikkdata-api.atlas.vegvesen.no/?query="
  #url = "https://www.vegvesen.no/trafikkdata/api/?query="
  #headers = list(
  #  'content-type' = 'application/json')
)

# Helper functions
is_even <- function(x) x[x %% 2 == 0]
is_odd <- function(x) x[x %% 2 == 1]


# Areas ####
get_counties_deprecated <- function() {
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
    mutate(
      geo_number =
        case_when(
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
        )
    ) %>%
    arrange(geo_number)

  return(counties)
}

get_counties <- function() {
  # Get all counties
  api_query <-
    "query counties {
       areas {
         counties {
           number
           name
           geographicNumber
           countryPart {
            id
            name
          }
         }
       }
    }"

  myqueries <- Query$new()
  myqueries$query("data", api_query)

  counties <-
    cli$exec(myqueries$queries$data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    dplyr::rename(
      county_number = data.areas.counties.number,
      geo_number = data.areas.counties.geographicNumber,
      county_name = data.areas.counties.name,
      country_part_number = data.areas.counties.countryPart.id,
      country_part_name = data.areas.counties.countryPart.name
    ) %>%
    arrange(geo_number)

  return(counties)
}

get_country_parts <- function() {

  query_api <-
    "query country_parts {
      areas {
        countryParts {
          id
          name
        }
      }
    }"

  myqueries <- Query$new()
  myqueries$query("response", query_api)

  country_parts <- cli$exec(myqueries$queries$response) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    dplyr::rename(country_part_number = 1,
                  country_part_name = 2) %>%
    dplyr::arrange(country_part_number)
}


get_municipalities <- function() {

  query_api <-
    "query municipalities {
       areas {
         municipalities {
           number
           name
           county {
            number
           }
         }
       }
     }"

  myqueries <- Query$new()
  myqueries$query("response", query_api)

  counties <- cli$exec(myqueries$queries$response) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    dplyr::rename(municipality_number = 1,
                  municipality_name = 2,
                  county_number = 3)
}


# Traffic registration points (TRPs) ####

get_points <- function() {
  # Get all traffic registration points
  query_points <-
    "query all_trps {
  trafficRegistrationPoints {
    id
    name
    trafficRegistrationType
    registrationFrequency
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
        geographicNumber
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
      lanes {
        laneNumber
      }
    }
    operationalStatus
  }
}"

  myqueries <- Query$new()
  myqueries$query("points", query_points)

  points <-
    cli$exec(myqueries$queries$points) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(data.trafficRegistrationPoints.commissions)) %>%
    dplyr::rename(
      trp_id = data.trafficRegistrationPoints.id,
      name =
        data.trafficRegistrationPoints.name,
      traffic_type =
        data.trafficRegistrationPoints.trafficRegistrationType,
      registration_frequency = data.trafficRegistrationPoints.registrationFrequency,
      county_name = data.trafficRegistrationPoints.location.county.name,
      county_no = data.trafficRegistrationPoints.location.county.number,
      county_geono = data.trafficRegistrationPoints.location.county.geographicNumber,
      #country_part_name =
      #  data.trafficRegistrationPoints.location.county.countryPart.name,
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
        data.trafficRegistrationPoints.location.roadLinkSequence.roadLinkSequenceId,
      operational_status =
        data.trafficRegistrationPoints.operationalStatus
      ) %>%
    dplyr::select(
      trp_id, name, traffic_type, registration_frequency,
      road_reference, county_geono, county_name,
      county_no, municipality_name, municipality_no, lanes, lat, lon,
      road_network_position, road_network_link, validFrom, validTo,
      operational_status
      ) %>%
    dplyr::mutate(
      lane_numbers = purrr::map(lanes, ~ purrr::pluck(., 1)),
      direction_with = purrr::map(lane_numbers, ~ length(is_odd(.)) > 0),
      direction_against = purrr::map(lane_numbers, ~ length(is_even(.)) > 0),
      number_of_directions = dplyr::if_else(direction_with == TRUE, 1, 0) +
        dplyr::if_else(direction_against == TRUE, 1, 0),
      road_link_position = paste0(road_network_position, "@",
                                  road_network_link),
      validFrom =
        floor_date(with_tz(ymd_hms(validFrom)), unit = "day"),
      validTo = floor_date(with_tz(ymd_hms(validTo)), unit = "day")
      )

  return(points)
}

#trp_list <- trp_chosen
get_trp_metadata_by_list <- function(trp_list) {
  # Get chosen traffic registration points

  input_variables <-
    list(
      "trpIds" = trp_list
    )

  query <-
    "query trps ($trpIds: [String!]!) {
        trafficRegistrationPoints (trafficRegistrationPointIds: $trpIds) {
      id
      name
      trafficRegistrationType
      registrationFrequency
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
          geographicNumber
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
        lanes {
          laneNumber
        }
      }
      operationalStatus
      direction {
          from
          fromAccordingToMetering
          to
          toAccordingToMetering
        }
    }
  }"

  my_query <- ghql::Query$new()$query(name = "my_query", query)

  response <- cli$exec(my_query$my_query, input_variables) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(data.trafficRegistrationPoints.commissions)) %>%
    dplyr::rename(
      trp_id = data.trafficRegistrationPoints.id,
      name =
        data.trafficRegistrationPoints.name,
      traffic_type =
        data.trafficRegistrationPoints.trafficRegistrationType,
      registration_frequency = data.trafficRegistrationPoints.registrationFrequency,
      county_name = data.trafficRegistrationPoints.location.county.name,
      county_no = data.trafficRegistrationPoints.location.county.number,
      county_geono = data.trafficRegistrationPoints.location.county.geographicNumber,
      #country_part_name =
      #  data.trafficRegistrationPoints.location.county.countryPart.name,
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
        data.trafficRegistrationPoints.location.roadLinkSequence.roadLinkSequenceId,
      operational_status =
        data.trafficRegistrationPoints.operationalStatus,
      from = data.trafficRegistrationPoints.direction.fromAccordingToMetering,
      to = data.trafficRegistrationPoints.direction.toAccordingToMetering
    ) %>%
    dplyr::select(
      trp_id, name, from, to, traffic_type, registration_frequency,
      road_reference, county_geono, county_name,
      county_no, municipality_name, municipality_no, lanes, lat, lon,
      road_network_position, road_network_link, validFrom, validTo,
      operational_status
    ) %>%
    dplyr::mutate(
      lane_numbers = purrr::map(lanes, ~ purrr::pluck(., 1)),
      direction_with = purrr::map(lane_numbers, ~ length(is_odd(.)) > 0),
      direction_against = purrr::map(lane_numbers, ~ length(is_even(.)) > 0),
      number_of_directions = dplyr::if_else(direction_with == TRUE, 1, 0) +
        dplyr::if_else(direction_against == TRUE, 1, 0),
      road_link_position = paste0(road_network_position, "@",
                                  road_network_link),
      validFrom =
        floor_date(with_tz(ymd_hms(validFrom)), unit = "day"),
      validTo = floor_date(with_tz(ymd_hms(validTo)), unit = "day")
    )

  return(response)
}


get_trp_data_time_span <- function() {

  query_points <-
    "query all_trps {
      trafficRegistrationPoints {
        id
        dataTimeSpan {
          firstData
          firstDataWithQualityMetrics
          latestData {
            volumeByDay
          }
        }
      }
    }"

  myqueries <- Query$new()
  myqueries$query("points", query_points)

  points <-
    cli$exec(myqueries$queries$points) %>%
    jsonlite::fromJSON(
      simplifyDataFrame = T,
      flatten = T
    ) %>%
    as.data.frame() %>%
    dplyr::rename(
      trp_id = data.trafficRegistrationPoints.id,
      first_data =
        data.trafficRegistrationPoints.dataTimeSpan.firstData,
      first_data_with_quality_metrics =
        data.trafficRegistrationPoints.dataTimeSpan.firstDataWithQualityMetrics,
      latest_daily_traffic =
        data.trafficRegistrationPoints.dataTimeSpan.latestData.volumeByDay

    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = !trp_id,
        .fns = ~ floor_date(with_tz(ymd_hms(.x)), unit = "day")
      )
    )

  return(points)
}

#trp_id <- "45979V1175826"
get_trp_labels <- function(trp_id) {

  query_points <-
    paste0(
      "query labels {
        trafficRegistrationPoints (trafficRegistrationPointIds: \"", trp_id,"\"){
      id
      manualLabels {
        validFrom
        validTo
        affectedLanes {
          lane {
            laneNumber
          }
          states
        }
      }
    }
  }")

  myqueries <- Query$new()
  myqueries$query("points", query_points)

  response <-
    cli$exec(myqueries$queries$points) %>%
    jsonlite::fromJSON(
      simplifyDataFrame = T,
      flatten = T
    )

  if (base::length(response$data$trafficRegistrationPoints) == 0) {
    labels <- tibble::tibble()
  }
  else {
  labels <-
    response %>%
    as.data.frame() %>%
    tidyr::unnest(
      data.trafficRegistrationPoints.manualLabels,
      keep_empty = FALSE
    )
  }

  return(labels)
}


get_labels_for_trp_list <- function(trp_list) {

  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {

    base::print(trp_count)
    base::print(trp_list[trp_count])

    data_points <-
      dplyr::bind_rows(
        data_points,
        get_trp_labels(trp_list[trp_count])
      )

    trp_count <- trp_count + 1

  }

  # labels_1 <-
  #   data_points %>%
  #   tidyr::unnest(
  #     data.trafficRegistrationPoints.manualLabels,
  #     keep_empty = FALSE
  #   )

  if (nrow(data_points) == 0) {
    labels = data.frame()
  } else {
    labels <- data_points %>%
      tidyr::unnest_wider(
        affectedLanes
      ) %>%
      dplyr::select(
        trp_id = data.trafficRegistrationPoints.id,
        label_start = validFrom,
        label_end = validTo,
        lane = lane.laneNumber
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c(label_start, label_end),
          .fns = ~ floor_date(with_tz(ymd_hms(.x)), unit = "hour")
        ),
        date_interval = lubridate::interval(label_start, label_end)
      ) %>%
      tidyr::unnest_longer(
        lane
      )
  }

  return(labels)
}


get_points_with_direction <- function() {
  # Get all traffic registration points
  api_query <-
    "query punkter_med_retning {
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
        geographicNumber
        name
      }
      municipality {
        name
      }
      roadReference {
        roadCategory {
          id
        }
        shortForm
      }
    }
    direction {
      from
      to
    }
    operationalStatus
    latestData {
      volumeByDay
    }
  }
}"

  myqueries <- Query$new()
  myqueries$query("api_data", api_query)

  api_response <-
    cli$exec(myqueries$queries$api_data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    dplyr::select(
      trp_id = data.trafficRegistrationPoints.id,
      name = data.trafficRegistrationPoints.name,
      traffic_type =
        data.trafficRegistrationPoints.trafficRegistrationType,
      road_category =
        data.trafficRegistrationPoints.location.roadReference.roadCategory.id,
      road_reference =
        data.trafficRegistrationPoints.location.roadReference.shortForm,
      operational_status =
        data.trafficRegistrationPoints.operationalStatus,
      latest_day_with_data =
        data.trafficRegistrationPoints.latestData.volumeByDay,
      county_name = data.trafficRegistrationPoints.location.county.name,
      county_no = data.trafficRegistrationPoints.location.county.geographicNumber,
      municipality_name = data.trafficRegistrationPoints.location.municipality.name,
      from = data.trafficRegistrationPoints.direction.from,
      to = data.trafficRegistrationPoints.direction.to,
      lat =
        data.trafficRegistrationPoints.location.coordinates.latLon.lat,
      lon =
        data.trafficRegistrationPoints.location.coordinates.latLon.lon
    ) %>%
    dplyr::mutate(
      latest_day_with_data =
        floor_date(with_tz(ymd_hms(latest_day_with_data)), unit = "day")
    )

  return(api_response)
}

get_trps_with_direction <- function() {
  # Get all traffic registration points
  api_query <-
    "query trps_and_direction {
      trafficRegistrationPoints {
        id
        meteringDirectionChanged
        direction {
          from
          fromAccordingToMetering
          to
          toAccordingToMetering
        }
      }
    }"

  myqueries <- Query$new()
  myqueries$query("api_data", api_query)

  api_response <- cli$exec(myqueries$queries$api_data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    dplyr::select(
      trp_id = data.trafficRegistrationPoints.id,
      metering_direction_changed = data.trafficRegistrationPoints.meteringDirectionChanged,
      #from = data.trafficRegistrationPoints.direction.from,
      #to = data.trafficRegistrationPoints.direction.to,
      from_according_to_metering = data.trafficRegistrationPoints.direction.fromAccordingToMetering,
      to_according_to_metering = data.trafficRegistrationPoints.direction.toAccordingToMetering
    ) %>%
    dplyr::mutate(
      from_according_to_metering = stringr::str_to_title(from_according_to_metering, locale = "no"),
      to_according_to_metering = stringr::str_to_title(to_according_to_metering, locale = "no"),
      odd =
        paste0("Fra ", from_according_to_metering,
               " til ", to_according_to_metering),
      even =
        paste0("Fra ", to_according_to_metering,
               " til ", from_according_to_metering)
    ) %>%
    tidyr::pivot_longer(cols = c("odd", "even"),
                        names_to = "lane_parity_api",
                        values_to = "direction") %>%
    dplyr::mutate(lane_parity_kibana = dplyr::case_when(
      metering_direction_changed == FALSE & lane_parity_api == "odd" ~ "odd",
      metering_direction_changed == FALSE & lane_parity_api == "even" ~ "even",
      metering_direction_changed == TRUE & lane_parity_api == "odd" ~ "even",
      metering_direction_changed == TRUE & lane_parity_api == "even" ~ "odd"
    ))

  return(api_response)
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



# Average traffic ####
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

#trp_id <- "16184V249637"
#day_type <- "ALL"
get_trp_aadt_with_coverage <- function(trp_id, day_type = "ALL") {

  print(trp_id)
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
        byYear (dayType: ", day_type, "){
          year
          dayType
          total{
            coverage{
              percentage
              included {
                numerator
                denominator
              }
            }
            validLengthVolume{
              average
            }
            validSpeedVolume{
              average
            }
            volume{
              average
              standardDeviation
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

  if(is_empty(trp_aadt$data$trafficData$volume$average$daily$byYear) |
     is.null(trp_aadt$data$trafficData$volume$average$daily$byYear$total.volume.average) |
     is.null(trp_aadt$data$trafficData$volume$average$daily$byYear$total.coverage.percentage) |
     # TODO: this last condition removes old data - need not be - look at query for directional AADT
     # prevents query from failing when just old data are available
     #ncol(trp_aadt$data$trafficData$volume$average$daily$byYear) < 5
     is.null(trp_aadt$data$trafficData$volume$average$daily$byYear$total.validLengthVolume.average)
     ){
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byYear.year,
        day_type = data.trafficData.volume.average.daily.byYear.dayType,
        coverage = data.trafficData.volume.average.daily.byYear.total.coverage.percentage,
        n_days = data.trafficData.volume.average.daily.byYear.total.coverage.included.numerator,
        n_days_of_year = data.trafficData.volume.average.daily.byYear.total.coverage.included.denominator,
        valid_length_volume = data.trafficData.volume.average.daily.byYear.total.validLengthVolume.average,
        valid_speed_volume = data.trafficData.volume.average.daily.byYear.total.validSpeedVolume.average,
        adt = data.trafficData.volume.average.daily.byYear.total.volume.average,
        standard_deviation = data.trafficData.volume.average.daily.byYear.total.volume.standardDeviation
      ) %>%
      dplyr::mutate(
        trp_id = as.character(trp_id),
        standard_error =
          round(standard_deviation / sqrt(n_days) * sqrt((n_days_of_year - n_days) / (n_days_of_year - 1)), digits = 2))
  }

  return(trp_aadt)
}

#trp_id <-"43623V704583"
get_trp_aadt_by_direction <- function(trp_id) {

  print(trp_id)
  my_query <- paste0(
    "query aadt {
    trafficData(trafficRegistrationPointId: \"", trp_id,"\"){
      trafficRegistrationPoint{
        id
      }
      volume{
    average{
      daily{
        byYear (dayType: ALL){
          year
          dayType
          byDirection {
            heading
            total{
              coverage{
                percentage
                included {
                  numerator
                  denominator
                }
              }
              validLengthVolume{
                average
              }
              validSpeedVolume{
                average
              }
              volume{
                average
                standardDeviation
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
  myqueries$query("aadts", my_query)

  trp_aadt <- cli$exec(myqueries$queries$aadts) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  # Check if no value
  if(
    is_empty(
      trp_aadt$data$trafficData$volume$average$daily$byYear$byDirection[1]
    ) |
     is.null(
       trp_aadt$data$trafficData$volume$average$daily$byYear$byDirection[[1]]$total.volume.average
     )
    ){
    # Når det ikke er noe data
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      tidyr::unnest(
        cols = data.trafficData.volume.average.daily.byYear.byDirection
      ) %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byYear.year,
        day_type = data.trafficData.volume.average.daily.byYear.dayType,
        #coverage = total.coverage.percentage,
        #valid_length_volume = total.validLengthVolume.average,
        #valid_speed_volume = total.validSpeedVolume.average,
        adt = total.volume.average,
        standard_deviation = total.volume.standardDeviation
      ) %>%
      dplyr::mutate(
        trp_id = as.character(trp_id),
        heading = stringr::str_to_title(heading, locale = "no"),
      )
  }

    if(
      "total.coverage.included.numerator" %in% colnames(trp_aadt)
    ){
      trp_aadt <-
        trp_aadt %>%
        dplyr::rename(
          days_included = total.coverage.included.numerator,
          days_in_year = total.coverage.included.denominator
        ) %>%
        dplyr::mutate(
          se_mean =
            standard_deviation /
            sqrt(days_included) *
            sqrt((days_in_year - days_included) / (days_in_year - 1))
        )
    }else{
      trp_aadt <- trp_aadt
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


get_trp_sdt <- function(trp_id, given_year) {

  dt_query <- paste0(
    "query trp_sdt {
    trafficData(trafficRegistrationPointId: \"", trp_id,"\"){
      trafficRegistrationPoint{
        id
      }
      volume{
    average{
      daily{
        bySeason(dayType: ALL, year: ", given_year, "){
          season {
              name
            }
            dayType
            total{
            coverage{
              percentage
            }
            volume{
              average
              standardDeviation
            }
          }
        }
      }
    }
  }
}
}")

  myqueries <- Query$new()
  myqueries$query("dts", dt_query)

  # Splitter opp her med en test om det ikke er noe verdi
  trp_data <- cli$exec(myqueries$queries$dts) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  if(is_empty(trp_data$data$trafficData$volume$average$daily$bySeason) |
     is.null(trp_data$data$trafficData$volume$average$daily$bySeason$total.volume.average)){
    # Når det ikke er noe MDT
    trp_data <- data.frame()
  }else{
    trp_data <- trp_data %>%
      as.data.frame() %>%
      dplyr::rename(
        trp_id =
          data.trafficData.id,
        day_type =
          data.trafficData.volume.average.daily.bySeason.dayType,
        season =
          data.trafficData.volume.average.daily.bySeason.season.name,
        coverage =
          data.trafficData.volume.average.daily.bySeason.total.coverage.percentage,
        sdt =
          data.trafficData.volume.average.daily.bySeason.total.volume.average,
        standard_deviation =
          data.trafficData.volume.average.daily.bySeason.total.volume.standardDeviation
      ) %>%
      dplyr::mutate(
        year = given_year
      )
    #dplyr::mutate(trp_id = as.character(trp_id),
    #             coverage = round(coverage, digits = 1),
    #              uncertainty = signif(uncertainty, 2))
  }

  return(trp_data)
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


#trp_id <- "61425V181294"
#mdt_year <- "2020"
get_trp_mdt_by_direction <- function(trp_id, mdt_year) {
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
          byDirection {
            heading
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

  trp_aadt <- cli$exec(myqueries$queries$mdts) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  # Check if no value
  if(is_empty(trp_aadt$data$trafficData$volume$average$daily$byMonth$byDirection[1]) |
     is.null(trp_aadt$data$trafficData$volume$average$daily$byMonth$byDirection[[1]]$total.volume.average)){
    # Når det ikke er noe MDT
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      tidyr::unnest(cols = data.trafficData.volume.average.daily.byMonth.byDirection) %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byMonth.year,
        month = data.trafficData.volume.average.daily.byMonth.month,
        coverage = total.coverage.percentage,
        valid_length_volume = total.validLengthVolume.average,
        valid_speed_volume = total.validSpeedVolume.average,
        mdt = total.volume.average) %>%
      dplyr::mutate(trp_id = as.character(trp_id),
                    heading = stringr::str_to_title(heading, locale = "no"))
  }

  return(trp_aadt)
}


#mdt_test <- get_trp_mdt_with_coverage("91582V930281", "2020")
#mdt_test_2 <- get_trp_mdt_by_lane("91582V930281", "2020")

#trp_id <- "66220V72824"
#trp_id <- "41078V805609"
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
                standardDeviation
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

  trp_aadt <-
    cli$exec(myqueries$queries$aadts) %>%
    jsonlite::fromJSON(
      simplifyDataFrame = T,
      flatten = T
    )

  trp_aadt_length <-
    length(trp_aadt$data$trafficData$volume$average$daily$byYear$byLengthRange)

  if(
    is_empty(
      trp_aadt$data$trafficData$volume$average$daily$byYear
    ) |
    is_empty(
      trp_aadt$data$trafficData$volume$average$daily$byYear$byLengthRange[[trp_aadt_length]]
    )
     ){
    # if no total aadt
    # if no length range aadt
    trp_aadt <- data.frame()
  }else{
    trp_aadt <- trp_aadt %>%
      as.data.frame() %>%
      tidyr::unnest(
        cols = c(data.trafficData.volume.average.daily.byYear.byLengthRange),
        names_sep = "."
      ) %>%
      # rename only columns that will exist if solely old data
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byYear.year,
        length_range = data.trafficData.volume.average.daily.byYear.byLengthRange.lengthRange.representation,
        aadt_length_range = data.trafficData.volume.average.daily.byYear.byLengthRange.total.volume.average,
        # include sd to test if all data are old in next step
        sd_length_range = data.trafficData.volume.average.daily.byYear.byLengthRange.total.volume.standardDeviation,
        aadt_total = data.trafficData.volume.average.daily.byYear.total.volume.average
      ) %>%
      dplyr::mutate(trp_id = as.character(trp_id))
  }

  if(all(is.na(trp_aadt$sd_length_range))
    ){
    trp_aadt <-
      trp_aadt %>%
      # if all old data, create empty columns (they are missing from API response)
      dplyr::mutate(
        aadt_valid_length = NA,
        coverage = NA
      )
  }else{
    trp_aadt <-
      trp_aadt %>%
      dplyr::rename(
        aadt_valid_length = data.trafficData.volume.average.daily.byYear.total.validLengthVolume.average,
        coverage = data.trafficData.volume.average.daily.byYear.total.coverage.percentage,
        sd_total = data.trafficData.volume.average.daily.byYear.total.volume.standardDeviation
      )
  }

  return(trp_aadt)
}


#trp_id <- "28891V121760"

get_periodic_aadt_by_length <- function(trp_id) {

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
          byYear (dayType: ALL) {
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
                standardDeviation
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
                }
              }
            }
            calculationMethod {
              methodName
              ... on FactorCurveCalculationMethod {
                factorCurve
                numberOfDaysWithAtLeastOneHour {
                  total
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

  trp_aadt <-
    cli$exec(myqueries$queries$aadts) %>%
    jsonlite::fromJSON(
      simplifyDataFrame = T,
      flatten = T
    )

  if (
    purrr::is_empty(trp_aadt$data$trafficData$volume$average$daily$byYear)
  ){
    trp_aadt <- tibble::tibble()
  }else{
    trp_aadt <-
      trp_aadt |>
      base::as.data.frame() |>
      tibble::as_tibble()
  }

  return(trp_aadt)
}


get_periodic_aadt_by_length_for_trp_list <- function(trp_list) {

  number_of_points <- length(trp_list)
  #trp_data <- data.frame()
  trp_data <- tibble::tibble()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    trp_data <-
      bind_rows(
        trp_data,
        get_periodic_aadt_by_length(trp_list[trp_count])
      )

    trp_count <- trp_count + 1
  }

  trp_aadt <-
    trp_data |>
    dplyr::select(
      trp_id = data.trafficData.id,
      year = data.trafficData.volume.average.daily.byYear.year,
      by_length = data.trafficData.volume.average.daily.byYear.byLengthRange,
      coverage = data.trafficData.volume.average.daily.byYear.total.coverage.percentage,
      days = data.trafficData.volume.average.daily.byYear.calculationMethod.numberOfDaysWithAtLeastOneHour.total,
      aadt_total = data.trafficData.volume.average.daily.byYear.total.volume.average,
      method = data.trafficData.volume.average.daily.byYear.calculationMethod.methodName,
      factor_curve = data.trafficData.volume.average.daily.byYear.calculationMethod.factorCurve
    ) |>
    dplyr::mutate(
      length_of_list = lengths(by_length)
    )

  trp_aadt_no_length <-
    trp_aadt |>
    dplyr::filter(
      length_of_list == 0
    ) |>
    dplyr::select(
      -by_length
    )

  trp_aadt_length <-
    trp_aadt |>
    dplyr::filter(
      length_of_list != 0
    ) |>
    tidyr::unnest(
      cols = c(by_length),
      keep_empty = TRUE,
      names_sep = "."
    )

  trp_aadt_all <-
    dplyr::bind_rows(
      trp_aadt_length,
      trp_aadt_no_length
    ) |>
    dplyr::select(
      -length_of_list
    )

  return(trp_aadt_all)
}

# test_list <-
#   c(
#     "39215V320588", # kontinuerlig
#     "79404V1175648", # radar med lengde
#     "41078V805609" # radar uten lengde
#   )
#
# test_aadt <-
#   get_periodic_aadt_by_length_for_trp_list(test_list)



#trp_id <- "01316V804837"
#mdt_year <- 2022

# test_all_nortraf <- get_mdt_by_length_for_trp("43623V704583", 2014)
# test_mix_nortraf_new <- get_mdt_by_length_for_trp("43623V704583", 2015)
# test_all_new <- get_mdt_by_length_for_trp("43623V704583", 2016)
# test_none <- get_mdt_by_length_for_trp("01316V804837", 2022)

get_mdt_by_length_for_trp <- function(trp_id, mdt_year) {

  # Get all MDTs for a trp
  api_query <-
    paste0(
      "query trp_mdt {
        trafficData(trafficRegistrationPointId: \"", trp_id,"\") {
          trafficRegistrationPoint {
            id
          }
    volume {
      average {
        daily {
          byMonth(dayType: ALL, year: ", mdt_year, "){
            year
            month
            total {
              validLengthVolume {
                average
              }
              coverage {
                percentage
              }
              volume {
                average
                standardDeviation
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
  }
}
")

  myqueries <- Query$new()
  myqueries$query("mdts", api_query)

  trp_mdt <-
    cli$exec(myqueries$queries$mdts) %>%
    jsonlite::fromJSON(
      simplifyDataFrame = T,
      flatten = T
    )

  # trp_mdt_length <-
  #   length(trp_mdt$data$trafficData$volume$average$daily$byMonth$byLengthRange)

  # Is there any length range MDTs?
  n_length_range <-
    purrr::map_int(
      trp_mdt$data$trafficData$volume$average$daily$byMonth$byLengthRange,
      ~ length(.x)
    ) |>
    sum() / 3


  if(
    is.null(trp_mdt$data$trafficData$volume$average$daily$byMonth$total.volume.average)
    #all(is.na(trp_mdt$data$trafficData$volume$average$daily$byMonth$total))
    #rlang::is_empty(
    #  trp_mdt$data$trafficData$volume$average$daily$byMonth
    #)
    |
    n_length_range == 0
#    rlang::is_empty(
#      trp_mdt$data$trafficData$volume$average$daily$byMonth$byLengthRange
      #trp_mdt$data$trafficData$volume$average$daily$byMonth$byLengthRange[[trp_mdt_length]]
#    )
  ){
    # if no total mdt
    # if no length range mdt
    trp_mdt <- data.frame()
  }else{
    trp_mdt <-
      trp_mdt %>%
      as.data.frame() %>%
      tidyr::unnest(
        cols = c(data.trafficData.volume.average.daily.byMonth.byLengthRange),
        names_sep = "."
      ) %>%
      # rename only columns that will exist if solely old data
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.daily.byMonth.year,
        month = data.trafficData.volume.average.daily.byMonth.month,
        length_range = data.trafficData.volume.average.daily.byMonth.byLengthRange.lengthRange.representation,
        mdt_length_range = data.trafficData.volume.average.daily.byMonth.byLengthRange.total.volume.average,
        # include sd to test if all data are old in next step
        sd_length_range = data.trafficData.volume.average.daily.byMonth.byLengthRange.total.volume.standardDeviation,
        mdt_total = data.trafficData.volume.average.daily.byMonth.total.volume.average
      ) %>%
      dplyr::mutate(trp_id = as.character(trp_id))
  }

  if(all(is.na(trp_mdt$sd_length_range))
  ){
    trp_mdt <-
      trp_mdt %>%
      # if all old data, create empty columns (they are missing from API response)
      dplyr::mutate(
        mdt_valid_length = NA,
        total_coverage = NA,
        sd_total = NA
        #length_coverage = NA
      )
  }else{
    trp_mdt <-
      trp_mdt %>%
      dplyr::rename(
        mdt_valid_length = data.trafficData.volume.average.daily.byMonth.total.validLengthVolume.average,
        total_coverage = data.trafficData.volume.average.daily.byMonth.total.coverage.percentage,
        sd_total = data.trafficData.volume.average.daily.byMonth.total.volume.standardDeviation
        #length_coverage = data.trafficData.volume.average.daily.byMonth.byLengthRange.total.coverage.percentage
      )
  }

  if(n_length_range == 0
  ){
    trp_mdt <- trp_mdt
  }else{
    trp_mdt <-
      trp_mdt |>
      dplyr::select(
        trp_id,
        year,
        month,
        length_range,
        mdt_length_range,
        sd_length_range,
        mdt_valid_length,
        total_coverage,
        mdt_total,
        sd_total
        #length_coverage
      )
  }

  return(trp_mdt)
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

#trp_list <- border_trps$trp_id
get_aadt_for_trp_list <- function(trp_list, day_type = "ALL") {

  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {

    print(trp_count)

    data_points <-
      dplyr::bind_rows(
        data_points,
        get_trp_aadt_with_coverage(trp_list[trp_count], day_type))

    trp_count <- trp_count + 1
  }

  trp_adt <- data_points %>%
    dplyr::mutate(adt = round(adt, digits = -1))

  return(trp_adt)
}

get_aadt_by_direction_for_trp_list <- function(trp_list) {

  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {

    print(trp_count)

    data_points <-
      bind_rows(
        data_points,
        get_trp_aadt_by_direction(trp_list[trp_count])
      )

    trp_count <- trp_count + 1
  }

  trp_adt <- data_points %>%
    dplyr::mutate(adt = round(adt, digits = 0))

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


get_mdt_by_direction_for_trp_list <- function(trp_list, mdt_year) {

  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             get_trp_mdt_by_direction(
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

    data_points <-
      bind_rows(
        data_points,
        get_aadt_by_length_for_trp(
          trp_list[trp_count]
        )
      )

    trp_count <- trp_count + 1
  }

  number_of_digits = 0

  trp_adt <-
    data_points %>%
    dplyr::mutate(
      aadt_valid_length =
        round(aadt_valid_length, digits = number_of_digits),
      aadt_total =
        round(aadt_total, digits = number_of_digits),
      aadt_length_range =
        round(aadt_length_range, digits = number_of_digits)
    )

  return(trp_adt)
}


get_mdt_by_length_for_trp_list <- function(trp_list, mdt_year) {

  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {

    data_points <-
      bind_rows(
        data_points,
        get_mdt_by_length_for_trp(
          trp_list[trp_count],
          mdt_year
        )
      )

    trp_count <- trp_count + 1
  }

  number_of_digits = 0 #-1
  # Should not round here as these are just to calculate sliding index

  trp_mdt <-
    data_points %>%
    dplyr::mutate(
      mdt_valid_length =
        round(mdt_valid_length, digits = number_of_digits),
      mdt_total =
        round(mdt_total, digits = number_of_digits),
      mdt_length_range =
        round(mdt_length_range, digits = number_of_digits)
    )

  return(trp_mdt)
}


#trp_list <- trp_distinct$trp_id[1:3]
#mdt_year <- "2021"
get_sdt_for_trp_list <- function(trp_list, mdt_year) {

  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <-
      bind_rows(
        data_points,
        get_trp_sdt(
          trp_list[trp_count],
          mdt_year
        )
      )
    trp_count <- trp_count + 1
  }
  trp_dt <- data_points %>%
    dplyr::mutate(
      sdt = round(sdt, digits = -1)
    )

  return(trp_dt)
}


# Point indices ####
#indexyear <- "2020"
#trp_ids <- "\"44656V72812\", \"77022V72359\""
#trp_ids <- "35258V2475662"
#trp_ids <- "47719V443837" # Vollsveien med alle lengdetall ekskludert
#trp_ids <- "17909V41450" # trp helt uten data

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
          baseVolume
          calculationVolume
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

  if(length(trp_data$data$trafficVolumeIndices) == 0){
    # If no index
    trp_data_data_all <- data.frame()
  }else{
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
  }

  if(nrow(trp_data_data_all) == 0){
      # If no index
      trp_data_data_all <- data.frame()
    }else{
      trp_data_data_all <- trp_data_data_all %>%
        tidyr::unnest(cols = c(lengthRangesTrafficVolumeIndex.indexNumbers))
      # Stop to check if length is excluded
      #if(is.null(trp_data_data_all$index.percentageChange)) {
      if(!("index.percentageChange" %in% colnames(trp_data_data_all))) {
        trp_data_data_all <- trp_data_data_all %>%
          dplyr::select(trp_id = trafficVolumeIndices.trafficRegistrationPoint.id,
                        year = trafficVolumeIndices.calculationMonth.year,
                        month = trafficVolumeIndices.calculationMonth.month,
                        road_category = trafficVolumeIndices.roadCategory.id,
                        day_type = dayType,
                        period = period,
                        index_total = totalTrafficVolumeIndex.indexNumber.index.indexNumber,
                        index_total_p = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
                        base_volume = totalTrafficVolumeIndex.indexNumber.index.baseVolume,
                        calculation_volume = totalTrafficVolumeIndex.indexNumber.index.calculationVolume,
                        index_total_coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
                        length_excluded = lengthRangesTrafficVolumeIndex.isExcluded,
                        length_range = lengthRange.representation,
                        length_coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage) %>%
          dplyr::mutate(length_index_p = NA) %>%
          dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
          dplyr::mutate(length_range = dplyr::if_else(length_range == "[..,5.6)",
                                                      "short", "long")) %>%
          tidyr::pivot_wider(names_from = length_range, names_prefix = "index_",
                             values_from = length_index_p)
      }else{
        trp_data_data_all <- trp_data_data_all %>%
        dplyr::select(trp_id = trafficVolumeIndices.trafficRegistrationPoint.id,
                      year = trafficVolumeIndices.calculationMonth.year,
                      month = trafficVolumeIndices.calculationMonth.month,
                      road_category = trafficVolumeIndices.roadCategory.id,
                      day_type = dayType,
                      period = period,
                      index_total = totalTrafficVolumeIndex.indexNumber.index.indexNumber,
                      index_total_p = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
                      base_volume = totalTrafficVolumeIndex.indexNumber.index.baseVolume,
                      calculation_volume = totalTrafficVolumeIndex.indexNumber.index.calculationVolume,
                      index_total_coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
                      length_excluded = lengthRangesTrafficVolumeIndex.isExcluded,
                      length_range = lengthRange.representation,
                      #length_index = index.indexNumber,
                      length_index_p = index.percentageChange,
                      length_coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage) %>%
        dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
        dplyr::mutate(length_range = dplyr::if_else(length_range == "[..,5.6)",
                                                    "short", "long")) %>%
        tidyr::pivot_wider(names_from = length_range, names_prefix = "index_",
                           values_from = length_index_p)
      }
    }

  return(trp_data_data_all)
}

get_pointindices_for_trp_list <- function(trp_list, index_year) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             get_pointindices(trp_list[trp_count], index_year))
    trp_count <- trp_count + 1
  }

  return(data_points)
}


# Hourly and daily traffic ####

#trp_id <- "92719V1125906"
#from <- "2019-01-01T00:00:00.000+01:00"
#to <- "2019-02-01T00:00:00.000+01:00"

trp_id <- "30868V1109333"
from <- "2023-05-30T08:00:00.000+02:00"
to <- "2023-05-30T09:00:00.000+02:00"

get_hourly_traffic_by_length_lane <- function(trp_id, from, to) {

  # ZonedDateTime:
  # A datetime with timezone, e.g. "2017-01-24T00:00:00.000+01:00"

  # Response is paginated
  # Default values
  hasNextPage <- TRUE
  end_cursor <- ""
  hourly_traffic <- data.frame()

  query <-
    "query hourly_traffic ($trpId: String!, $from: ZonedDateTime!, $to: ZonedDateTime!, $endCursor: String!) {
       trafficData (trafficRegistrationPointId: $trpId) {
         trafficRegistrationPoint {
           id
         }
        volume {
          byHour (from: $from, to: $to, after: $endCursor) {
            edges {
              node {
                from
                byLane {
                  lane {
                    laneNumberAccordingToMetering
                  }
                  total {
										volumeNumbers {
                      validLength {
                        percentage
                      }
										}
                    coverage {
                      percentage
                    }
                  }
                  byLengthRange {
                    lengthRange {
                      representation
                    }
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
              }
            }
            pageInfo {
              hasNextPage
              endCursor
            }
          }
        }
      }
    }"

  my_query <- ghql::Query$new()$query(name = "my_query", query)

  while(hasNextPage == TRUE){

    input_variables <-
      list(
        "trpId" = trp_id,
        "from" = from,
        "to" = to,
        "endCursor" = end_cursor
      )

    response <-
      cli$exec(my_query$my_query, input_variables) |>
      jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

    if(length(response$data$trafficData$volume$byHour$edges) == 0)
      break;

    end_cursor <- response$data$trafficData$volume$byHour$pageInfo$endCursor

    hasNextPage <- response$data$trafficData$volume$byHour$pageInfo$hasNextPage

    response_data <-
      response |>
      base::as.data.frame() |>
      tibble::as_tibble() |>
      dplyr::select(
        -tidyselect::ends_with("hasNextPage"),
        -tidyselect::ends_with("endCursor")
      ) |>
      tidyr::unnest(cols = data.trafficData.volume.byHour.edges.node.byLane) |>
      # If some hours have no data
      dplyr::rowwise() |>
      dplyr::mutate(
        data_length = list(length(purrr::pluck(byLengthRange, 1)))
      ) |>
      dplyr::ungroup() |>
      tidyr::unnest(cols = data_length) |>
      dplyr::filter(
        data_length != 0
      ) |>
      tidyr::unnest(
        cols = byLengthRange,
        names_sep = "_length_"
      )

    hourly_traffic <- dplyr::bind_rows(hourly_traffic, response_data)
  }

  hourly_traffic_tidy <-
    hourly_traffic |>
    dplyr::select(
      trp_id = data.trafficData.id,
      from = data.trafficData.volume.byHour.edges.node.from,
      total_coverage = total.coverage.percentage,
      length_quality = total.volumeNumbers.validLength.percentage,
      length_coverage = byLengthRange_length_total.coverage.percentage,
      length_range = byLengthRange_length_lengthRange.representation,
      traffic = byLengthRange_length_total.volumeNumbers.volume,
      lane = lane.laneNumberAccordingToMetering
    ) |>
    dplyr::mutate(from = lubridate::with_tz(lubridate::ymd_hms(from), "CET"))

 return(hourly_traffic_tidy)

}


get_hourly_traffic_by_length <- function(trp_id, from, to) {

  # ZonedDateTime:
  # A datetime with timezone, e.g. "2017-01-24T00:00:00.000+01:00"

  # Response is paginated
  # Default values
  hasNextPage <- TRUE
  end_cursor <- ""
  hourly_traffic <- data.frame()

  query <-
    "query hourly_traffic ($trpId: String!, $from: ZonedDateTime!, $to: ZonedDateTime!, $endCursor: String!) {
       trafficData (trafficRegistrationPointId: $trpId) {
         trafficRegistrationPoint {
           id
         }
        volume {
          byHour (from: $from, to: $to, after: $endCursor) {
            edges {
              node {
                from
                total {
									volumeNumbers {
                    validLength {
                      percentage
                    }
									}
                  coverage {
                    percentage
                  }
                }
                byLengthRange {
                  lengthRange {
                    representation
                  }
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
            }
            pageInfo {
              hasNextPage
              endCursor
            }
          }
        }
      }
    }"

  my_query <- ghql::Query$new()$query(name = "my_query", query)

  while(hasNextPage == TRUE){

    input_variables <-
      list(
        "trpId" = trp_id,
        "from" = from,
        "to" = to,
        "endCursor" = end_cursor
      )

    response <-
      cli$exec(my_query$my_query, input_variables) |>
      jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

    if(length(response$data$trafficData$volume$byHour$edges) == 0)
      break;

    end_cursor <- response$data$trafficData$volume$byHour$pageInfo$endCursor

    hasNextPage <- response$data$trafficData$volume$byHour$pageInfo$hasNextPage

    response_data <-
      response |>
      base::as.data.frame() |>
      tibble::as_tibble() |>
      dplyr::select(
        -tidyselect::ends_with("hasNextPage"),
        -tidyselect::ends_with("endCursor")
      ) |>
      tidyr::unnest(cols = data.trafficData.volume.byHour.edges.node.byLengthRange) #|>
      # If some hours have no data
      # dplyr::rowwise() |>
      # dplyr::mutate(
      #   data_length = list(length(purrr::pluck(byLengthRange, 1)))
      # ) |>
      # dplyr::ungroup() |>
      # tidyr::unnest(cols = data_length) |>
      # dplyr::filter(
      #   data_length != 0
      # ) |>
      # tidyr::unnest(
      #   cols = byLengthRange,
      #   names_sep = "_length_"
      # )

    hourly_traffic <- dplyr::bind_rows(hourly_traffic, response_data)
  }

  hourly_traffic_tidy <-
    hourly_traffic |>
    dplyr::select(
      trp_id = data.trafficData.id,
      from = data.trafficData.volume.byHour.edges.node.from,
      total_coverage = data.trafficData.volume.byHour.edges.node.total.coverage.percentage,
      length_quality = data.trafficData.volume.byHour.edges.node.total.volumeNumbers.validLength.percentage,
      length_coverage = total.coverage.percentage,
      length_range = lengthRange.representation,
      traffic = total.volumeNumbers.volume
    ) |>
    dplyr::mutate(from = lubridate::with_tz(lubridate::ymd_hms(from), "CET"))

  return(hourly_traffic_tidy)

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
    myqueries$query("hourlyTraffic", build_query())

    trafficData <-
      cli$exec(myqueries$queries$hourlyTraffic) %>%
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

 # trp_id <- "68068V521218"
 # from <- "2020-01-01T00:00:00+01:00"
 # to <- "2021-01-01T00:00:00+01:00"
# test1 <- get_daily_traffic(trps$trp_id[1], from, to)
# test2 <- get_daily_traffic(trps$trp_id[2], from, to)
#
# test_bind <- bind_rows(test1, test2)

get_daily_traffic <- function(trp_id, from, to) {

  # Default values
  hasNextPage <- TRUE
  cursor <- ""
  dailyTraffic <- data.frame()

  build_query <- function() {
    query_traffic <- paste0(
      'query point_day_volumes {
    trafficData(trafficRegistrationPointId: "',
      trp_id,
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
    dplyr::select(point_id, point_name, from, total_volume, coverage) %>%
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


get_dt_by_length_for_trp <- function(trp_id, from, to) {

  # Default values
  hasNextPage <- TRUE
  cursor <- ""
  dailyTraffic <- data.frame()

  build_query <- function() {

    query_traffic <- paste0(
      'query dt_with_length {
        trafficData (trafficRegistrationPointId: "', trp_id, '"){
          trafficRegistrationPoint {
            id
          }
          volume {
            byDay (
              from: "', from, '",
              to: "', to, '",
              after: "', cursor, '") {
          edges {
            node {
              from
              total {
                volumeNumbers {
                  volume
                  validLength {
                    percentage
                  }
                }
                coverage {
                  percentage
                }
              }
              byLengthRange {
                lengthRange {
                  representation
                }
                total {
                  volumeNumbers {
                    volume
                  }
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
  }')
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

    # Skip parsing page if no volume numbers
    if(length(trafficData$data.trafficData.volume.byDay.edges.node.total.volumeNumbers.volume) == 0) {
      trafficData <- data.frame()
    }else{
      trafficData %<>%
        select(
          -data.trafficData.volume.byDay.pageInfo.hasNextPage,
          -data.trafficData.volume.byDay.pageInfo.endCursor
        ) %>%
        tidyr::unnest(
          cols = data.trafficData.volume.byDay.edges.node.byLengthRange
        ) %>%
        dplyr::select(
          point_id = data.trafficData.id,
          from = data.trafficData.volume.byDay.edges.node.from,
          total_volume = data.trafficData.volume.byDay.edges.node.total.volumeNumbers.volume,
          total_coverage = data.trafficData.volume.byDay.edges.node.total.coverage.percentage,
          length_range = lengthRange.representation,
          length_range_volume = total.volumeNumbers.volume,
          length_quality = data.trafficData.volume.byDay.edges.node.total.volumeNumbers.validLength.percentage
          )
    }

    dailyTraffic <- dplyr::bind_rows(dailyTraffic, trafficData)
  }

  colunm_names <- c("point_id", "from", "total_volume", "total_coverage",
                    "length_range", "length_range_volume", "length_quality")

  if(nrow(dailyTraffic) == 0) {
    dailyTraffic <- setNames(data.frame(matrix(ncol = 7, nrow = 0)),
                             colunm_names)
  }else{
    #colnames(dailyTraffic) <- colunm_names
  }

  # To avoid error when joining, cast column type
  dailyTraffic <-
    dailyTraffic |>
    dplyr::filter(!is.na(total_volume)) |>
    dplyr::mutate(
      point_id = as.character(point_id),
      #from = with_tz(ymd_hms(from), "CET"),
      from = clock::date_parse(from),
      total_volume = as.integer(total_volume),
      total_coverage = as.numeric(total_coverage),
      length_range = as.character(length_range),
      length_range_volume = as.integer(length_range_volume),
      length_quality = as.numeric(length_quality)
    )

  return(dailyTraffic)
}


get_dt_by_length_for_trp_list <- function(trp_list, from, to) {

  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             get_dt_by_length_for_trp(
                               trp_list[trp_count],
                               from,
                               to))
    trp_count <- trp_count + 1
  }

  return(data_points)
}

# trp_id <- "17681V704560"
# from_day <- "2019-01-01"
# to_day <- "2019-01-02"

get_dt_by_direction <- function(trp_id, from_day, to_day) {

  daily_traffic <- tibble::tibble()
  # from and to: date as string, e.g. "2023-01-01"
  from <- paste0(from_day, "T00:00:00.000Z")
  to <- paste0(to_day, "T00:00:00.000Z")
  cursor <- ""
  hasNextPage <- TRUE

  query <-
    "query get_dt_by_direction ($trpId: String!, $from: ZonedDateTime!, $to: ZonedDateTime!, $afterEndCursor: String!) {
  trafficData (trafficRegistrationPointId: $trpId) {
    trafficRegistrationPoint {
      id
    }
    volume {
      byDay (from: $from, to: $to, after: $afterEndCursor) {
        edges {
          node {
            from
            byDirection {
              heading
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
        }
        pageInfo {
          hasNextPage
          endCursor
        }
      }
    }
  }
}"

  while(hasNextPage == TRUE){

    input_variables <-
      list(
        "trpId" = trp_id,
        "from" = from,
        "to" =  to,
        "afterEndCursor" = cursor
      )

    my_query <- ghql::Query$new()$query(name = "my_query", query)

    response <-
      cli$exec(my_query$my_query, input_variables) |>
      jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

    if(length(response$data$trafficData$volume$byDay$edges) == 0)
      break;

    response_tibble <-
      response |>
      as.data.frame() |>
      tibble::as_tibble()

    cursor <-
      response_tibble$data.trafficData.volume.byDay.pageInfo.endCursor[1] %>%
      as.character()

    hasNextPage <-
      response_tibble$data.trafficData.volume.byDay.pageInfo.hasNextPage[1]

    response_tidy <-
      response_tibble |>
      dplyr::select(
          -data.trafficData.volume.byDay.pageInfo.hasNextPage,
          -data.trafficData.volume.byDay.pageInfo.endCursor
        ) |>
      tidyr::unnest(cols = data.trafficData.volume.byDay.edges.node.byDirection) |>
      dplyr::select(
        trp_id = data.trafficData.id,
        day = data.trafficData.volume.byDay.edges.node.from,
        heading,
        volume = total.volumeNumbers.volume,
        coverage = total.coverage.percentage
      ) |>
      dplyr::mutate(
        day = lubridate::ymd_hms(day) |>  lubridate::with_tz() |> lubridate::as_date()
      )

    daily_traffic <-
      dplyr::bind_rows(daily_traffic, response_tidy)
  }

  # colunm_names <- c("trp_id", "day", "heading", "volume", "coverage")
  #
  # if(nrow(response_tidy) == 0) {
  #   daily_traffic <-
  #     stats::setNames(
  #       data.frame(
  #         matrix(ncol = 5, nrow = 0)
  #       ),
  #     colunm_names)
  # }else{
  #   #colnames(dailyTraffic) <- colunm_names
  # }

  # To avoid error when joining, cast column type
  # daily_traffic <-
  #   daily_traffic |>
  #   dplyr::filter(!is.na(volume)) |>
  #   dplyr::mutate(
  #     trp_id = as.character(trp_id),
  #     day = clock::date_parse(day),
  #     heading = as.character(heading),
  #     volume = as.integer(volume),
  #     coverage = as.numeric(coverage)
  #   )

  return(daily_traffic)
}


# Published indices ####
# index_id <- 953
# indexyear <- 2023
# indexmonth <- 9

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
          [
            EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG,
            EUROPAVEG_RIKSVEG,
            FYLKESVEG
          ]) {
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
          indexNumber
          calculationVolume
          baseVolume
        }
        volumeIndexCoverage {
          hours {
            percentage
          }
        }
        confidenceInterval {
          confidenceWidth
        }
        standardDeviation
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
      road_category = roadCategoryCombination,
      length_range = lengthRange.representation,
      index_i = volumeIndexNumber.indexNumber,
      index_p = volumeIndexNumber.percentageChange,
      calc_volume = volumeIndexNumber.calculationVolume,
      base_volume = volumeIndexNumber.baseVolume,
      #coverage = volumeIndexCoverage.hours.percentage,
      confidence_width = confidenceInterval.confidenceWidth,
      standard_deviation = standardDeviation,
      area_name = area.name,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month
      ) %>%
    dplyr::filter(day_type == "ALL") %>%
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
      road_category = roadCategoryCombination,
      length_range = lengthRange.representation,
      index_i = volumeIndexNumber.indexNumber,
      index_p = volumeIndexNumber.percentageChange,
      calc_volume = volumeIndexNumber.calculationVolume,
      base_volume = volumeIndexNumber.baseVolume,
      #coverage = volumeIndexCoverage.hours.percentage,
      confidence_width = confidenceInterval.confidenceWidth,
      standard_deviation = standardDeviation,
      area_name = area.name,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month
    ) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::mutate(period = "year_to_date")

  published_index <- bind_rows(monthly_data,
                               year_to_date_data)

  return(published_index)
}


get_published_index_for_months <- function(index_id, index_year, last_month) {

  index_table <- tibble::tibble()

  i <- 1

  while (i < last_month + 1) {
    index_table <-
      dplyr::bind_rows(
        index_table,
        get_published_index(index_id, index_year, i)
      )
    i = i + 1
  }

  return(index_table)
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
              isManuallyExcluded
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
            calculationVolume
            baseVolume
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
          calculationVolume
          baseVolume
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

  unnested_data <-
    trp_data$data %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(
      publishedAreaTrafficVolumeIndex.containsPointTrafficVolumeIndices.edges))

  indexpoints <- unnested_data$node.pointTrafficVolumeIndex.trafficRegistrationPoint.id

   monthly_data <-
    unnested_data %>%
    tidyr::unnest(cols = c(node.pointTrafficVolumeIndex.volumeIndicesMonth)) %>%
    dplyr::select(-node.pointTrafficVolumeIndex.volumeIndicesYearToDate) %>%
    tidyr::unnest(cols = c(lengthRangesTrafficVolumeIndex.indexNumbers)) %>%
    dplyr::select(
      area_name = publishedAreaTrafficVolumeIndex.name,
      trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month,
      day_type = dayType,
      is_excluded = isExcluded,
      is_manually_excluded = node.isManuallyExcluded,
      index_total_p = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
      calc_volume = totalTrafficVolumeIndex.indexNumber.index.calculationVolume,
      base_volume = totalTrafficVolumeIndex.indexNumber.index.baseVolume,
      index_total_coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
      length_excluded = lengthRangesTrafficVolumeIndex.isExcluded,
      length_range = lengthRange.representation,
      length_index = index.percentageChange,
      length_calc_volume = index.calculationVolume,
      length_base_volume = index.baseVolume,
      length_coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage
      ) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
    dplyr::mutate(
      length_range =
        dplyr::if_else(
          length_range == "[..,5.6)",
          "short", "long")
    ) %>%
    tidyr::pivot_wider(
      names_from = length_range,
      #names_prefix = "index_",
      #values_from = length_index
      values_from = c(length_index, length_calc_volume, length_base_volume)
    ) %>%
    dplyr::mutate(period = "month") |>
     dplyr::rename(index_short = length_index_short)

    year_to_date_data <-
      unnested_data %>%
      tidyr::unnest(cols = c(node.pointTrafficVolumeIndex.volumeIndicesYearToDate)) %>%
      dplyr::select(- node.pointTrafficVolumeIndex.volumeIndicesMonth) %>%
      tidyr::unnest(cols = c(lengthRangesTrafficVolumeIndex.indexNumbers)) %>%
      dplyr::select(
        area_name = publishedAreaTrafficVolumeIndex.name,
        trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
        year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
        month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month,
        day_type = dayType,
        is_excluded = isExcluded,
        is_manually_excluded = node.isManuallyExcluded,
        index_total_p = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
        calc_volume = totalTrafficVolumeIndex.indexNumber.index.calculationVolume,
        base_volume = totalTrafficVolumeIndex.indexNumber.index.baseVolume,
        index_total_coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
        length_excluded = lengthRangesTrafficVolumeIndex.isExcluded,
        length_range = lengthRange.representation,
        length_index = index.percentageChange,
        length_calc_volume = index.calculationVolume,
        length_base_volume = index.baseVolume,
        length_coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage
      ) %>%
      dplyr::filter(day_type == "ALL") %>%
      dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
      dplyr::mutate(
        length_range =
          dplyr::if_else(
            length_range == "[..,5.6)",
            "short", "long")
      ) %>%
      tidyr::pivot_wider(
        names_from = length_range,
        #names_prefix = "index_",
        #values_from = length_index
        values_from = c(length_index, length_calc_volume, length_base_volume)
      ) %>%
      dplyr::mutate(period = "year_to_date") |>
      dplyr::rename(index_short = length_index_short)

  published_index <-
    dplyr::bind_rows(
      monthly_data,
      year_to_date_data
    )

  published_points <- list(indexpoints, published_index)

  return(published_points)
}


#indexmonth <- "10"
#indexyear <- "2020"
#index_id <- "960"
get_published_pointindex_trondheim <- function(index_id, indexyear, indexmonth) {
  # Need volume numbers for length classes in order to calculate city index
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
              isManuallyExcluded
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
            calculationVolume
            baseVolume
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
          calculationVolume
          baseVolume
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

  unnested_data <-
    trp_data$data %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(
      publishedAreaTrafficVolumeIndex.containsPointTrafficVolumeIndices.edges))

  indexpoints <- unnested_data$node.pointTrafficVolumeIndex.trafficRegistrationPoint.id

  monthly_data <-
    unnested_data %>%
    tidyr::unnest(cols = c(node.pointTrafficVolumeIndex.volumeIndicesMonth)) %>%
    dplyr::select(-node.pointTrafficVolumeIndex.volumeIndicesYearToDate) %>%
    tidyr::unnest(cols = c(lengthRangesTrafficVolumeIndex.indexNumbers))

  # Need to have class in long form
  monthly_data_all <-
    monthly_data %>%
    dplyr::select(
      area_name = publishedAreaTrafficVolumeIndex.name,
      trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month,
      day_type = dayType,
      is_excluded = isExcluded,
      is_manually_excluded = node.isManuallyExcluded,
      #length_range = totalTrafficVolumeIndex.indexNumber.lengthRange.representation,
      index = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
      calc_volume = totalTrafficVolumeIndex.indexNumber.index.calculationVolume,
      base_volume = totalTrafficVolumeIndex.indexNumber.index.baseVolume,
      coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
      length_excluded = lengthRangesTrafficVolumeIndex.isExcluded
    ) %>%
    dplyr::mutate(length_range = "alle") %>%
    dplyr::relocate(length_range, .before = index) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::distinct(trp_id, .keep_all = T)

  monthly_data_short_long <-
    monthly_data %>%
    dplyr::select(
      area_name = publishedAreaTrafficVolumeIndex.name,
      trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month,
      day_type = dayType,
      is_excluded = isExcluded,
      is_manually_excluded = node.isManuallyExcluded,
      length_range = lengthRange.representation,
      index = index.percentageChange,
      calc_volume = index.calculationVolume,
      base_volume = index.baseVolume,
      coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage,
      length_excluded = lengthRangesTrafficVolumeIndex.isExcluded
    ) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
    dplyr::mutate(
      length_range =
        dplyr::if_else(
          length_range == "[..,5.6)",
          "lette",
          "tunge"
        )
    )

  monthly_data_long <-
    dplyr::bind_rows(
      monthly_data_all,
      monthly_data_short_long
    ) %>%
    dplyr::mutate(period = "month")

  year_to_date_data <- unnested_data %>%
    tidyr::unnest(cols = c(node.pointTrafficVolumeIndex.volumeIndicesYearToDate)) %>%
    dplyr::select(- node.pointTrafficVolumeIndex.volumeIndicesMonth) %>%
    tidyr::unnest(cols = c(lengthRangesTrafficVolumeIndex.indexNumbers))

  # Need to have class in long form
  year_to_date_data_all <-
    year_to_date_data %>%
    dplyr::select(
      area_name = publishedAreaTrafficVolumeIndex.name,
      trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month,
      day_type = dayType,
      is_excluded = isExcluded,
      is_manually_excluded = node.isManuallyExcluded,
      #length_range = totalTrafficVolumeIndex.indexNumber.lengthRange.representation,
      index = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
      calc_volume = totalTrafficVolumeIndex.indexNumber.index.calculationVolume,
      base_volume = totalTrafficVolumeIndex.indexNumber.index.baseVolume,
      coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
      length_excluded = lengthRangesTrafficVolumeIndex.isExcluded
    ) %>%
    dplyr::mutate(length_range = "alle") %>%
    dplyr::relocate(length_range, .before = index) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::distinct(trp_id, .keep_all = T)

  year_to_date_data_short_long <-
    year_to_date_data %>%
    dplyr::select(
      area_name = publishedAreaTrafficVolumeIndex.name,
      trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month,
      day_type = dayType,
      is_excluded = isExcluded,
      is_manually_excluded = node.isManuallyExcluded,
      length_range = lengthRange.representation,
      index = index.percentageChange,
      calc_volume = index.calculationVolume,
      base_volume = index.baseVolume,
      coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage,
      length_excluded = lengthRangesTrafficVolumeIndex.isExcluded
    ) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
    dplyr::mutate(length_range = if_else(length_range == "[..,5.6)",
                                         "lette", "tunge"))

  year_to_date_data_long <-
    dplyr::bind_rows(
      year_to_date_data_all,
      year_to_date_data_short_long
    ) %>%
    dplyr::mutate(period = "year_to_date")


  published_index <-
    dplyr::bind_rows(
      monthly_data_long,
      year_to_date_data_long
    )

  published_points <- list(indexpoints, published_index)

  return(published_points)
}

query_published_pointindex_page <- function(index_id, indexyear, indexmonth,
                                            cursor) {

  # TODO: report a bug in the API, as it doesn't work with cursors other than null
  api_query <- paste0(
    'query published_pointindex {
      publishedAreaTrafficVolumeIndex (
        id: ', index_id, ',
        year: ', indexyear, ',
        month: ', indexmonth, ') {
        id
        name
        period {
          calculationMonth {
            year
            month
          }
        }
        containsPointTrafficVolumeIndices (after: ', cursor, ') {
          pageInfo {
            endCursor
            hasNextPage
          }
          edges {
            node {
              isManuallyExcluded
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
            calculationVolume
            baseVolume
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
          calculationVolume
          baseVolume
        }
      }
      indexCoverage {
        hours {
          percentage
        }
      }
    }
  }')

}


#index_id <- 962
#indexyear <- 2020
#indexmonth <- 1

get_published_pointindex_paginated <- function(index_id, indexyear, indexmonth) {
  # Get published index for a given area, year and month
  # Response is paginated if more than 100 points!
  # Pagination is ignored here
  # Returns: list with two elements; trp_ids, pointindices

  # Initial values of pagination variables
  hasNextPage <- TRUE
  cursor <- "null"
  fetched_pages_data <- data.frame()

  while(hasNextPage == TRUE) {

    myqueries <- Query$new()
    myqueries$query("data", query_published_pointindex_page(index_id,
                                                            indexyear,
                                                            indexmonth,
                                                            cursor))

    trp_data <- cli$exec(myqueries$queries$data) %>%
      jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

    index_year_page <- trp_data$data$publishedAreaTrafficVolumeIndex$period.calculationMonth.year
    index_month_page <- trp_data$data$publishedAreaTrafficVolumeIndex$period.calculationMonth.month

    trp_data_page <-
      trp_data$data$publishedAreaTrafficVolumeIndex$containsPointTrafficVolumeIndices.edges %>%
      as.data.frame() %>%
      dplyr::mutate(index_year_page = index_year_page,
                    index_month_page = index_month_page)

    end_cursor_string <-
      trp_data$data$publishedAreaTrafficVolumeIndex$containsPointTrafficVolumeIndices.pageInfo.endCursor %>%
      as.character()

    cursor <- paste0('"', end_cursor_string, '"')

    hasNextPage <-
      trp_data$data$publishedAreaTrafficVolumeIndex$containsPointTrafficVolumeIndices.pageInfo.hasNextPage

    fetched_pages_data <- bind_rows(fetched_pages_data, trp_data_page)
  }

  indexpoints <- fetched_pages_data$node.pointTrafficVolumeIndex.trafficRegistrationPoint.id

  # Unwrap one part at a time
  # 1. month
  # 2. yearToDate

  monthly_data <- fetched_pages_data %>%
    tidyr::unnest(cols = c(node.pointTrafficVolumeIndex.volumeIndicesMonth)) %>%
    dplyr::select(-node.pointTrafficVolumeIndex.volumeIndicesYearToDate) %>%
    tidyr::unnest(cols = c(lengthRangesTrafficVolumeIndex.indexNumbers)) %>%
    dplyr::select(trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
                  year = index_year_page,
                  month = index_month_page,
                  day_type = dayType,
                  is_excluded = isExcluded,
                  is_manually_excluded = node.isManuallyExcluded,
                  index_total_p = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
                  calc_volume = totalTrafficVolumeIndex.indexNumber.index.calculationVolume,
                  base_volume = totalTrafficVolumeIndex.indexNumber.index.baseVolume,
                  index_total_coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
                  length_excluded = lengthRangesTrafficVolumeIndex.isExcluded,
                  length_range = lengthRange.representation,
                  length_index = index.percentageChange,
                  length_coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage
    ) %>%
    #dplyr::filter(day_type == "ALL") %>%
    dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
    dplyr::mutate(length_range = if_else(length_range == "[..,5.6)",
                                         "short", "long")) %>%
    tidyr::pivot_wider(names_from = length_range, names_prefix = "index_",
                       values_from = length_index) %>%
    dplyr::mutate(period = "month")

  year_to_date_data <- fetched_pages_data %>%
    tidyr::unnest(cols = c(node.pointTrafficVolumeIndex.volumeIndicesYearToDate)) %>%
    dplyr::select(- node.pointTrafficVolumeIndex.volumeIndicesMonth) %>%
    tidyr::unnest(cols = c(lengthRangesTrafficVolumeIndex.indexNumbers)) %>%
    dplyr::select(trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
                  year = index_year_page,
                  month = index_month_page,
                  day_type = dayType,
                  is_excluded = isExcluded,
                  is_manually_excluded = node.isManuallyExcluded,
                  index_total_p = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
                  calc_volume = totalTrafficVolumeIndex.indexNumber.index.calculationVolume,
                  base_volume = totalTrafficVolumeIndex.indexNumber.index.baseVolume,
                  index_total_coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage,
                  length_excluded = lengthRangesTrafficVolumeIndex.isExcluded,
                  length_range = lengthRange.representation,
                  length_index = index.percentageChange,
                  length_coverage = lengthRangesTrafficVolumeIndex.indexCoverage.hours.percentage
    ) %>%
    #dplyr::filter(day_type == "ALL") %>%
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

#test_pi <- get_published_pointindex_paginated(962, 2020, 1)

get_published_pointindex_for_months <- function(index_id, index_year, last_month) {

  published_pointindex <- tibble::tibble()
  i <- 1

  # Saving only one version of indexpoints
  indexpoints <- get_published_pointindex(index_id, index_year, last_month)[[1]]

  while (i < last_month + 1) {

    published_pointindex <-
      dplyr::bind_rows(
        published_pointindex,
        get_published_pointindex(index_id, index_year, i)[[2]]
        )

    i = i + 1
  }

  published_points <- list(indexpoints, published_pointindex)

  return(published_points)
}

get_published_pointindex_for_months_trondheim <- function(index_id, index_year, last_month) {

  published_pointindex <- tibble::tibble()
  i <- 1

  # Saving only one version of indexpoints
  indexpoints <- get_published_pointindex_trondheim(index_id, index_year, last_month)[[1]]

  while (i < last_month + 1) {

    published_pointindex <-
      dplyr::bind_rows(
        published_pointindex,
        get_published_pointindex_trondheim(index_id, index_year, i)[[2]]
      )

    i = i + 1
  }

  published_points <- list(indexpoints, published_pointindex)

  return(published_points)
}

get_published_pointindex_for_months_paginated <- function(index_id, index_year, last_month) {

  published_pointindex <- tibble::tibble()
  i <- 1

  # Saving only one version of indexpoints
  indexpoints <-
    get_published_pointindex_paginated(
      index_id,
      index_year,
      last_month)[[1]]

  while (i < last_month + 1) {

    published_pointindex <-
      dplyr::bind_rows(
        published_pointindex,
        get_published_pointindex_paginated(index_id, index_year, i)[[2]]
      )

    i = i + 1
  }

  published_points <- list(indexpoints, published_pointindex)

  return(published_points)
}

# Specific for bike point index
get_published_pointindex_bike <- function(index_id, indexyear, indexmonth) {
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
              isManuallyExcluded
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
            baseVolume
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
    dplyr::select(area_name = publishedAreaTrafficVolumeIndex.name,
                  trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
                  year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
                  month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month,
                  day_type = dayType,
                  is_excluded = isExcluded,
                  is_manually_excluded = node.isManuallyExcluded,
                  index_total_p = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
                  base_volume = totalTrafficVolumeIndex.indexNumber.index.baseVolume,
                  index_total_coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage
    ) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::mutate(period = "month")

  year_to_date_data <- unnested_data %>%
    tidyr::unnest(cols = c(node.pointTrafficVolumeIndex.volumeIndicesYearToDate)) %>%
    dplyr::select(- node.pointTrafficVolumeIndex.volumeIndicesMonth) %>%
    dplyr::select(area_name = publishedAreaTrafficVolumeIndex.name,
                  trp_id = node.pointTrafficVolumeIndex.trafficRegistrationPoint.id,
                  year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
                  month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month,
                  day_type = dayType,
                  is_excluded = isExcluded,
                  is_manually_excluded = node.isManuallyExcluded,
                  index_total_p = totalTrafficVolumeIndex.indexNumber.index.percentageChange,
                  base_volume = totalTrafficVolumeIndex.indexNumber.index.baseVolume,
                  index_total_coverage = totalTrafficVolumeIndex.indexCoverage.hours.percentage
    ) %>%
    dplyr::filter(day_type == "ALL") %>%
    dplyr::mutate(period = "year_to_date")

  published_index <- bind_rows(monthly_data,
                               year_to_date_data)

  published_points <- list(indexpoints, published_index)

  return(published_points)
}

get_published_bikepointindex_for_months <- function(index_id, index_year, last_month) {

  published_pointindex <- tibble::tibble()
  i <- 1

  # Saving only one version of indexpoints
  indexpoints <-
    get_published_pointindex_bike(index_id, index_year, last_month)[[1]]

  while (i < last_month + 1) {

    published_pointindex <-
      dplyr::bind_rows(
        published_pointindex,
        get_published_pointindex_bike(index_id, index_year, i)[[2]]
      )

    i = i + 1
  }

  published_points <- list(indexpoints, published_pointindex)

  return(published_points)
}

# Specific for road traffic index
# TODO: Generalize to one set of functions
# Only difference being that this includes area_type and
# uses EUROPA_RIKS_FYLKESVEG instead of the one with KOMMUNALVEG also

#index_id <- 962
#indexyear <- 2020
#indexmonth <- 8

get_published_road_traffic_index <- function(index_id, indexyear, indexmonth) {
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
            type
          }
          byRoadCategoryCombination(combinations:
          [
            EUROPAVEG_RIKSVEG_FYLKESVEG,
            EUROPAVEG_RIKSVEG,
            FYLKESVEG
          ]) {
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
        standardDeviation
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
      road_category = roadCategoryCombination,
      length_range = lengthRange.representation,
      #index_i = trafficVolumeIndex.index.indexNumber,
      index_p = volumeIndexNumber.percentageChange,
      standard_deviation = standardDeviation,
      confidence_width = confidenceInterval.confidenceWidth,
      area_name = area.name,
      area_type = area.type,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month
    ) %>%
    dplyr::select(area_name, area_type, year, month, road_category, length_range, day_type,
                  index_p, standard_deviation) %>%
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
      road_category = roadCategoryCombination,
      length_range = lengthRange.representation,
      #index_i = trafficVolumeIndex.index.indexNumber,
      index_p = volumeIndexNumber.percentageChange,
      standard_deviation = standardDeviation,
      confidence_width = confidenceInterval.confidenceWidth,
      area_name = area.name,
      area_type = area.type,
      year = publishedAreaTrafficVolumeIndex.period.calculationMonth.year,
      month = publishedAreaTrafficVolumeIndex.period.calculationMonth.month
    ) %>%
    dplyr::select(area_name, area_type, year, month, road_category, length_range, day_type,
                  index_p, standard_deviation) %>%
    dplyr::mutate(period = "year_to_date")

  published_index <- bind_rows(monthly_data,
                               year_to_date_data)

  return(published_index)
}


get_published_road_traffic_index_for_months <- function(index_id, index_year, last_month) {

  index_table <- tibble::tibble()
  i <- 1
  while (i < last_month + 1) {
    index_table <- dplyr::bind_rows(index_table,
                                    get_published_road_traffic_index(index_id, index_year, i))
    i = i + 1
  }

  return(index_table)
}



# Average hourly and daily traffic ####
#trp_id <- #"79743V1125914"
 # "78481V42532"
#the_year <- "2019"
#day_type = "ALL"

get_trp_average_hour_of_day_traffic <- function(trp_id, the_year, day_type) {

  # Get all AADTs for a trp
  api_query <- paste0(
    "query hour_traffic {
  trafficData (trafficRegistrationPointId: \"", trp_id,"\"){
    trafficRegistrationPoint {
      id
    }
    volume {
      average {
        hourOfDay {
          byYear (year: ", the_year, ", dayType: ", day_type, "){
            year
            dayType
            total {
              startOfHour
              volume {
                average
                confidenceInterval {
                  confidenceWidth
                }
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
  myqueries$query("data", api_query)

  # TODO: Må splitte opp her med en test om det ikke er noe ÅDT
  trp_data <- cli$exec(myqueries$queries$data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  if(is_empty(trp_data$data$trafficData$volume$average$hourOfDay$byYear) |
     is.null(trp_data$data$trafficData$volume$average$hourOfDay$byYear$total) |
     length(trp_data$data$trafficData$volume$average$hourOfDay$byYear$total) == 0
     #ncol(trp_aadt$data$trafficData$volume$average$daily$byYear) < 5
  ){
    # hva gjør vi når det ikke er noe ÅDT?
    trp_data <- data.frame()
  }else{
    trp_data <- trp_data %>%
      as.data.frame() %>%
      dplyr::rename(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.hourOfDay.byYear.year,
        day_type = data.trafficData.volume.average.hourOfDay.byYear.dayType,
        coverage = data.trafficData.volume.average.hourOfDay.byYear.total.coverage.percentage,
        start_of_hour = data.trafficData.volume.average.hourOfDay.byYear.total.startOfHour,
        average_hour_of_day_traffic =
          data.trafficData.volume.average.hourOfDay.byYear.total.volume.average,
        confidence_width =
          data.trafficData.volume.average.hourOfDay.byYear.total.volume.confidenceInterval.confidenceWidth) %>%
      dplyr::mutate(trp_id = as.character(trp_id),
                    start_of_hour = lubridate::hms(start_of_hour) %>%
                      lubridate::hour(),
                    sum_of_hours = sum(average_hour_of_day_traffic),
                    average_hour_of_day_traffic_relative =
                      100 * average_hour_of_day_traffic / sum_of_hours)
  }

  return(trp_data)
}


get_trp_average_hour_of_day_traffic_for_all_day_types <- function(trp_id, the_year) {

 trp_data <- dplyr::bind_rows(
   get_trp_average_hour_of_day_traffic(trp_id, the_year, "ALL"),
   get_trp_average_hour_of_day_traffic(trp_id, the_year, "WEEKDAY"),
   get_trp_average_hour_of_day_traffic(trp_id, the_year, "WEEKEND")
 )
}


get_trp_average_hour_of_day_traffic_for_all_day_types_for_trp_list <- function(trp_list, the_year) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <-
      bind_rows(
        data_points,
        get_trp_average_hour_of_day_traffic_for_all_day_types(trp_list[trp_count], the_year))
    trp_count <- trp_count + 1
  }


  return(data_points)
}


#trp_id <- "79743V1125914"
# "78481V42532"
#the_year <- "2019"
#the_month <- 1
#day_type = "ALL"

get_trp_average_day_of_week_traffic_by_month <- function(trp_id,
                                                         the_year,
                                                         the_month,
                                                         day_type = "ALL") {

  api_query <- paste0(
    "
    query day_of_week_traffic {
      trafficData (trafficRegistrationPointId: \"", trp_id,"\"){
    trafficRegistrationPoint {
      id
    }
    volume {
      average {
        dayOfWeek {
          byMonth (year: ", the_year,
          ", month:", the_month,
          ", dayType: ", day_type, "){
            yearMonth {
              year
              month
            }
            dayType
            total {
              day
              volume {
                average
                standardDeviation
              }
              coverage {
                percentage
                included {
                  numerator
                  denominator
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
  myqueries$query("data", api_query)

  # TODO: Må splitte opp her med en test om det ikke er noe ÅDT
  trp_data <- cli$exec(myqueries$queries$data) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  if(is_empty(trp_data$data$trafficData$volume$average$dayOfWeek$byMonth) |
     is.null(trp_data$data$trafficData$volume$average$dayOfWeek$byMonth$total) |
     length(trp_data$data$trafficData$volume$average$dayOfWeek$byMonth$total) == 0
  ){
    # returnerer tom tabell for å ikke krasje loopen
    trp_data <- data.frame()
  }else{
    trp_data <- trp_data %>%
      as.data.frame() %>%
      dplyr::select(
        trp_id = data.trafficData.id,
        year = data.trafficData.volume.average.dayOfWeek.byMonth.yearMonth.year,
        month = data.trafficData.volume.average.dayOfWeek.byMonth.yearMonth.month,
        day_name = data.trafficData.volume.average.dayOfWeek.byMonth.total.day,
        day_type = data.trafficData.volume.average.dayOfWeek.byMonth.dayType,
        coverage = data.trafficData.volume.average.dayOfWeek.byMonth.total.coverage.percentage,
        included_days = data.trafficData.volume.average.dayOfWeek.byMonth.total.coverage.included.numerator,
        possible_days = data.trafficData.volume.average.dayOfWeek.byMonth.total.coverage.included.denominator,
        average_day_of_week_traffic =
          data.trafficData.volume.average.dayOfWeek.byMonth.total.volume.average,
        standard_deviation =
          data.trafficData.volume.average.dayOfWeek.byMonth.total.volume.standardDeviation)
  }

  return(trp_data)
}



get_trp_average_day_of_week_traffic_by_month_for_a_year <- function(trp_id, the_year) {

  data_points <- data.frame()
  month_count <- 1

  while (month_count <= 12) {

    data_points <- dplyr::bind_rows(
      data_points,
      get_trp_average_day_of_week_traffic_by_month(trp_id, the_year, month_count)
    )

    month_count <- month_count + 1
  }

  return(data_points)
}



get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list <- function(trp_list, the_year) {

  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {

    data_points <- dplyr::bind_rows(
      data_points,
      get_trp_average_day_of_week_traffic_by_month_for_a_year(trp_list[trp_count], the_year))

    trp_count <- trp_count + 1

  }

  return(data_points)
}

# year = 2022
# week_no = 2
# day_type = "WEEKDAY"
# trp_id = "17681V704560"

get_hour_of_day_week_direction_total <- function(year, week_no, day_type, trp_id) {

  # day_type: "WEEKDAY", "WEEKEND", "ALL
  input_variables <-
    list(
      "year" = year,
      "week" =  week_no,
      "dayType" = day_type,
      "trpId" = trp_id
    )

  query <-
    "query hour_of_day ($year: Year!, $week: Int!, $dayType: DayType!, $trpId: String!) {
  trafficData(trafficRegistrationPointId: $trpId) {
    trafficRegistrationPoint {
      id
    }
    volume {
      average {
        hourOfDay {
          byWeek(year: $year, week: $week, dayType: $dayType) {
            yearWeek {
              year
              week
            }
            dayType
            byDirection {
              heading
              total {
                startOfHour
                volume {
                  average
                }
                validLengthVolume {
                  average
                }
                validSpeedVolume {
                  average
                }
                coverage {
                  percentage
                  included {
                    numerator
                    denominator
                  }
                }
              }
              }
            }
          }
        }
      }
    }
  }"

  my_query <- ghql::Query$new()$query(name = "my_query", query)

  response <- cli$exec(my_query$my_query, input_variables) |>
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) |>
    as.data.frame() |>
    tibble::as_tibble() |>
    tidyr::unnest(cols = data.trafficData.volume.average.hourOfDay.byWeek.byDirection.total) |>
    dplyr::mutate(
      length_range = "all"
    ) |>
    dplyr::select(
      trp_id = data.trafficData.id,
      heading = data.trafficData.volume.average.hourOfDay.byWeek.byDirection.heading,
      length_range,
      year = data.trafficData.volume.average.hourOfDay.byWeek.yearWeek.year,
      week = data.trafficData.volume.average.hourOfDay.byWeek.yearWeek.week,
      hour = startOfHour,
      day_type = data.trafficData.volume.average.hourOfDay.byWeek.dayType,
      volume = volume.average,
      volume_with_length = validLengthVolume.average,
      coverage = coverage.percentage,
      n_days = coverage.included.numerator
    )

  return(response)
}


get_hour_of_day_week_direction_length <- function(year, week_no, day_type, trp_id) {

  # day_type: "WEEKDAY", "WEEKEND", "ALL
  input_variables <-
    list(
      "year" = year,
      "week" =  week_no,
      "dayType" = day_type,
      "trpId" = trp_id
    )

  query <-
    "query hour_of_day ($year: Year!, $week: Int!, $dayType: DayType!, $trpId: String!) {
  trafficData(trafficRegistrationPointId: $trpId) {
    trafficRegistrationPoint {
      id
    }
    volume {
      average {
        hourOfDay {
          byWeek(year: $year, week: $week, dayType: $dayType) {
            yearWeek {
              year
              week
            }
            dayType
            byDirection {
              heading
              byLengthRange {
                lengthRange {
                  representation
                }
                total {
                startOfHour
                volume {
                  average
                }
                validLengthVolume {
                  average
                }
                validSpeedVolume {
                  average
                }
                coverage {
                  percentage
                   included {
                    numerator
                    denominator
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
}"

  my_query <- ghql::Query$new()$query(name = "my_query", query)

  response <- cli$exec(my_query$my_query, input_variables) |>
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) |>
    as.data.frame() |>
    tibble::as_tibble() |>
    tidyr::unnest(cols = data.trafficData.volume.average.hourOfDay.byWeek.byDirection.byLengthRange) |>
    tidyr::unnest(cols = total) |>
    dplyr::select(
      trp_id = data.trafficData.id,
      heading = data.trafficData.volume.average.hourOfDay.byWeek.byDirection.heading,
      length_range = lengthRange.representation,
      year = data.trafficData.volume.average.hourOfDay.byWeek.yearWeek.year,
      week = data.trafficData.volume.average.hourOfDay.byWeek.yearWeek.week,
      hour = startOfHour,
      day_type = data.trafficData.volume.average.hourOfDay.byWeek.dayType,
      volume = volume.average,
      volume_with_length = validLengthVolume.average,
      coverage = coverage.percentage,
      n_days = coverage.included.numerator
    )

  return(response)

}



