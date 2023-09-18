base::Sys.setlocale(locale = "nb.utf8")
library(tidyverse)
library(writexl)


# City index ----
city_files <-
  list.files(
    path = "trd_bike",
    pattern = "byindeks.*csv",
    full.names = TRUE
  )

read_bike_index_csv <- function(filename) {

  year <-
    stringr::str_extract(
      filename,
      pattern = "[:digit:]{4}"
    ) |>
    base::as.numeric()

  month <-
    stringr::str_extract(
      filename,
      pattern = "[:digit:]{2}\\.csv"
    ) |>
    stringr::str_sub(1, 2) |>
    base::as.numeric()

  result <-
    readr::read_csv2(
      filename,
      locale = readr::locale(
        encoding = "latin1",
        grouping_mark = " "
      )
    ) |>
    dplyr::filter(
      vegkategori == "E+R+F+K"
    ) |>
    dplyr::mutate(
      year = year,
      month = month,
      index = stringr::str_replace(as.character(indeks), ",", "\\.")
    ) |>
    dplyr::select(
      year,
      month,
      day_type = 'døgn',
      period = periode,
      index,
      traffic_volume_calc_year = 'trafikkmengde indeksår',
      traffic_volume_base_year = 'trafikkmengde basisår',
      standard_deviation = standardavvik,
      confidence_interval = konfidensintervall
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(index:confidence_interval),
        .fns = ~ base::as.numeric(.x)
      )
    )

  return(result)
}

city_index <-
  purrr::map(
    city_files,
    ~ read_bike_index_csv(.x)
  ) |>
  purrr::list_rbind()



# TRP index ----
trp_files <-
  list.files(
    path = "trd_bike",
    pattern = "punktindeks.*csv",
    full.names = TRUE
  )


read_bike_trp_index_csv <- function(filename) {

  year <-
    stringr::str_extract(
      filename,
      pattern = "[:digit:]{4}"
    ) |>
    base::as.numeric()

  month <-
    stringr::str_extract(
      filename,
      pattern = "[:digit:]{2}\\.csv"
    ) |>
    stringr::str_sub(1, 2) |>
    base::as.numeric()

  result <-
    readr::read_csv2(
      filename,
      locale = readr::locale(
        encoding = "latin1",
        grouping_mark = " "
      )
    ) |>
    dplyr::mutate(
      year = year,
      month = month,
      index = stringr::str_replace(as.character(indeks), ",", "\\.")
    ) |>
    dplyr::select(
      trp_id = trpid,
      trp_name = trpnavn,
      road_reference = vegref,
      year,
      month,
      day_type = 'døgn',
      period = periode,
      index,
      traffic_volume_calc_year = 'trafikkmengde indeksår',
      traffic_volume_base_year = 'trafikkmengde basisår'
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(index:traffic_volume_base_year),
        .fns = ~ base::as.numeric(.x)
      )
    ) |>
    dplyr::filter(
      !is.na(index)
    )

  return(result)
}

trp_index <-
  purrr::map(
    trp_files,
    ~ read_bike_trp_index_csv(.x)
  ) |>
  purrr::list_rbind()


# N TRP ----
n_trp <-
  trp_index |>
  dplyr::group_by(
    year,
    month,
    day_type,
    period
  ) |>
  dplyr::summarise(
    n_trp = n(),
    .groups = "drop"
  )

city_index_with_n_trp <-
  city_index |>
  dplyr::left_join(
    n_trp,
    by = dplyr::join_by(
      year,
      month,
      day_type,
      period
    )
  )

writexl::write_xlsx(
  list(
    "byindeks" = city_index_with_n_trp,
    "punktindeks" = trp_index
  ),
  path = "trd_bike/trondheim_sykkelindeks.xlsx"
)
