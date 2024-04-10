# Data from RTM
library(tidyverse)
library(readxl)

rtm_trd_20 <-
  readxl::read_excel("rtm/rtm_trondheim_2020.xlsx") |>
  dplyr::select(
    start = A,
    end= B,
    distance = DISTANCE,
    road_category_id = LINKTYPE,
    aadt = CD_ADT,
    #CD_ADT2,
    direction = NVDBRETNIN
    #TEST_ID,
    #TEST2
  ) |>
  dplyr::filter(
    aadt > 0,
    road_category_id %in% c(1, 2, 3)
  )

test <-
  rtm_trd_20 |>
  dplyr::rowwise() |>
  dplyr::mutate(
    id = list(stringr::str_sort(c(start, end)))
  )
