# Fetching data from Trafikkdata-API or TRP-API

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
                  lat =
                    data.trafficRegistrationPoints.location.coordinates.latLon.lat,
                  lon =
                    data.trafficRegistrationPoints.location.coordinates.latLon.lon,
                  road_reference =
                    data.trafficRegistrationPoints.location.currentRoadReference.roadReference532.shortForm) %>%
    dplyr::select(trp_id, name, road_reference, lat, lon) %>%
    dplyr::mutate(road_reference = str_replace(road_reference, "HP ", "hp")) %>%
    dplyr::mutate(road_reference = str_replace(road_reference, "Meter ", "m"))

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
      dplyr::rename(trp_id = 1,
                    year = 2,
                    adt = 3,
                    sd = 4) %>%
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
