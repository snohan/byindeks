library(tidyverse)
library(ghql)
library(jsonlite)
library(httr)

cli <- GraphqlClient$new(
  url = "https://www.vegvesen.no/trafikkdata/api/?query="
)


hent_alle_punkter <- function() {
  query_punkter <-
    "query punkter {
  trafficRegistrationPoints {
    id
    name
    trafficRegistrationType
    location {
      roadReference {
        shortForm
      }
      county {
        name
      }
    }
  }
}"

  myqueries <- Query$new()
  myqueries$query("points", query_punkter)

  punkter <- cli$exec(myqueries$queries$points) %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    as.data.frame() %>%
    dplyr::rename(trp_id = data.trafficRegistrationPoints.id,
                  name = data.trafficRegistrationPoints.name,
                  traffic_type = data.trafficRegistrationPoints.trafficRegistrationType,
                  county_name = data.trafficRegistrationPoints.location.county.name,
                  road_reference = data.trafficRegistrationPoints.location.roadReference.shortForm)
}


alle_punkter <- hent_alle_punkter()

utvalgte_punkter <- alle_punkter %>%
  filter(traffic_type == "VEHICLE") %>%
  filter(!stringr::str_detect(road_reference, "KD")) %>%
  filter(county_name == "Nordland")


hent_aadt_for_punkt <- function(trp_id) {

  query_aadt <- paste0(
    "query aadt {
      trafficData(trafficRegistrationPointId: \"",
        trp_id,
        "\") {
        trafficRegistrationPoint {
          id
        }
        volume {
          average {
            daily {
              byYear {
                year
                total {
                  volume {
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
    }"
  )

  myqueries <- Query$new()
  myqueries$query("aadt", query_aadt)

  aadt <- cli$exec(myqueries$queries$aadt) %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    as.data.frame()

}


eksempel_aadt <- hent_aadt_for_punkt("99483V705274")



