library(tidyverse)
library(ghql)
library(jsonlite)
library(httr)
library(tictoc)

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

#trp_id = "44656V72812"
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
                  validLengthVolume {
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
                    }
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
    jsonlite::fromJSON(flatten = TRUE, simplifyDataFrame = TRUE)

  # test
  #aadt_test <- aadt %>%
  #  as.data.frame()

  # Feilhåndtering
  if (is_empty(aadt$data$trafficData$volume$average$daily$byYear) |
      is.null(aadt$data$trafficData$volume$average$daily$byYear$total.volume.average)) {

    aadt <- data.frame()

  }else{

    aadt <- aadt %>%
    as.data.frame() %>%
    tidyr::unnest(cols = c(data.trafficData.volume.average.daily.byYear.byLengthRange)) %>%
    rename(trp_id = 1,
           year = 2,
           length_group = 3,
           length_group_aadt = 4,
           aadt = 5,
           coverage = 6,
           valid_length = 7
           )

  }
}

#test <- hent_aadt_for_punkt(utvalgte_punkter$trp_id[2])

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

  data_points <- data_points %>%
    mutate(length_quality = round(valid_length / aadt * 100,
                                  digits = 1))

  return(data_points)

}

tic()
aadt_nordland <- hent_aadt_for_punktliste(utvalgte_punkter$trp_id)
toc()

punkter_med_adt_nordland <- aadt_nordland %>%
  left_join(utvalgte_punkter) %>%
  select(county_name,
         name,
         road_reference,
         year,
         aadt,
         coverage,
         length_quality,
         length_group,
         length_group_aadt)


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


# ÅDT til UP
all_points <- get_points()

all_distinct_points <- all_points %>%
#  dplyr::distinct(trp_id, .keep_all = T)
  filter(is.na(validTo))

up_trp <- c("26266V443149",
            "78251V1719132",
            "72753V1896595",
            "62514V971819",
            "69356V971439",
            "82998V971817",
            "76429V2548000")

up_aadt <- get_aadt_for_trp_list(up_trp)

up_aadt_meta <- up_aadt %>%
  filter(year == 2019) %>%
  left_join(all_distinct_points) %>%
  as_tibble()

up_aadt_speed <- up_aadt_meta %>%
  mutate(speed_limit = purrr::map_chr(road_link_position, get_speedlimit_by_roadlink))

up_aadt_speed %>%
  select(trp_id, name, road_reference, municipality_name, year, adt, speed_limit) %>%
  write.csv2(file = "up_punkt_med_aadt.csv",
             row.names = FALSE)

speed_test <- get_speedlimit_by_roadlink(up_aadt_meta$road_link_position[2])

roadlink <- up_aadt_meta$road_link_position[2]
