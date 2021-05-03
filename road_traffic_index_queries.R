# Query Traffic Data API for road traffic index

# Libraries and helper functions ####
library(tidyverse)
library(jsonlite)
library(ghql)
library(lubridate)
library(magrittr)

cli <- GraphqlClient$new(
  url = "https://www.vegvesen.no/trafikkdata/api/?query=")

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

# Fetching published index from Traffic Data API
index_2021 <- get_published_road_traffic_index_for_months(962, 2021, 2) %>%
  dplyr::mutate(area_type = dplyr::case_when(area_type == "COUNTY" ~ "Fylke",
                                             area_type == "COUNTRY_PART" ~ "Landsdel",
                                             area_type == "COUNTRY" ~ "Hele landet"),
                Lengdeklasse = dplyr::case_when(length_range == "[..,..)" ~ "Alle kjøretøy",
                                                length_range == "[..,5.6)" ~ "Lette kjøretøy",
                                                length_range == "[5.6,..)" ~ "Tunge kjøretøy"),
                Dagtype = dplyr::case_when(day_type == "ALL" ~ "Alle døgn",
                                           day_type == "WEEKDAY" ~ "Yrkedøgn",
                                           day_type == "WEEKEND" ~ "Helgedøgn"),
                Vegkategori = dplyr::case_when(
                  road_category == "FYLKESVEG" ~ "Fylkesveg",
                  road_category == "EUROPAVEG_RIKSVEG" ~ "Europa- og riksveg",
                  road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG" ~ "Europa-, riks- og fylkesveg"),
                Indeks = round(index_p, digits = 1),
                Standardavvik = round(standard_deviation, digits = 1),
                Periode = dplyr::case_when(period == "month" ~ "Måned",
                                           period == "year_to_date" ~ "Hittil i år")
                ) %>%
  dplyr::select(area_name, area_type, year, month, Vegkategori, Lengdeklasse, Dagtype,
                Indeks, Standardavvik, Periode)