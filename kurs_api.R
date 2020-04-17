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
  # Henter ÅDT for ett punkt
  # Inn: en trp_id

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
    jsonlite::fromJSON(flatten = TRUE)

  # Feilhåndtering
  if (is_empty(aadt$data$trafficData$volume$average$daily$byYear) |
      is.null(aadt$data$trafficData$volume$average$daily$byYear$total.volume.average)) {

    aadt <- data.frame()

  }else{

    aadt <- aadt %>%
    as.data.frame() %>%
    rename(trp_id = 1,
           year = 2,
           aadt = 3,
           coverage = 4)

  }



}

# Henter ÅDT for ett punkt
eksempel_aadt <- hent_aadt_for_punkt(utvalgte_punkter$trp_id[2])

# Setter sammen info
punkter_med_adt <- eksempel_aadt %>%
  left_join(utvalgte_punkter) %>%
  select(county_name,
         name,
         road_reference,
         year,
         aadt,
         coverage)

# Hente ÅDT for flere punkter
hent_aadt_for_punktliste  <- function(trp_list) {

  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp_count <- 1

  while (trp_count <= number_of_points) {

    data_points <- dplyr::bind_rows(
      data_points,
      hent_aadt_for_punkt(trp_list[trp_count]))

    trp_count <- trp_count + 1
  }

  return(data_points)

}

aadt_nordland <- hent_aadt_for_punktliste(utvalgte_punkter$trp_id)



punkter_med_adt_nordland <- aadt_nordland %>%
  left_join(utvalgte_punkter) %>%
  select(county_name,
         name,
         road_reference,
         year,
         aadt,
         coverage)


# Skrive ut til CSV eller Excel




#JUKSELAPP
####
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


