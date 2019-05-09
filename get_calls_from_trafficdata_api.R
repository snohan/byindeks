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
    fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    rename(point_id = data.trafficRegistrationPoints.id,
           point_name = data.trafficRegistrationPoints.name,
           traffic_type =
             data.trafficRegistrationPoints.trafficRegistrationType)

  return(points)
}

# test
points <- getPoints() %>%
  unnest()

# Get points ####
query allPoints{
  trafficRegistrationPoints{
    id
    name
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
}



# Hent ÅDT ####