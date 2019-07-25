# Fetching data from Trafikkdata-API.

# Packages ####
# library(ghql)
# library(jsonlite)
# library(httr)
# library(tidyverse)
# library(lubridate)

# Functions ####

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
      roadReferences{
        validFrom
        validTo
        roadReference532{
          shortForm
        }
      }
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
             roadReference532.shortForm) %>%
    dplyr::filter(is.na(validTo)) %>%
    dplyr::select(-starts_with("valid"))

  return(points)
}

#trp_id = "32135V604101"

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
                volume
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
    dplyr::rename(trp_id = 1,
                  year = 2,
                  adt = 3) %>%
    dplyr::mutate(trp_id = as.character(trp_id))
  }

  return(trp_aadt)
}

# Test: Hente ÅDt for mange punkter
#trp_list <- indekspunktene_oslo$trp_id

getAdtForpoints <- function(trp_list) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {
    data_points <- bind_rows(data_points,
                             getTrpAadt(trp_list[trp_count]))
    trp_count <- trp_count + 1
  }
  return(data_points)
}
