# Fetching data from Trafikkdata-API.

# Packages ####
library(ghql)
library(jsonlite)
library(httr)
library(tidyverse)
library(lubridate)

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

# Hent ÅDT ####