# Make one Excel file
cities <-
  tibble::tibble(
    city_number = c(
      "952",
      "959",
      "8952",
      "960",
      "1952",
      "955",
      "16952",
      "19953",
      "18952",
      "19954",
      "20952"
    ),
    area_name = c(
      "Nord-Jæren",
      "Osloområdet",
      "Bergensområdet",
      "Trondheimsområdet",
      "Buskerudbyen",
      "Grenland",
      "Tromsø",
      "Kristiansand",
      "Nedre Glomma",
      "Bodø",
      "Ålesund"
    )
  )

# Rolling indices
read_rolling_indices <- function(city_no) {

  readr::read_rds(
    file =
      paste0(
        "data_indexpoints_tidy/rolling_indices_",
        city_no,
        ".rds"
      )
  ) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    city_number = city_no
  ) |>
  dplyr::left_join(
    cities,
    by = dplyr::join_by(city_number)
  )

}

all_rolling_indices <-
  purrr::map(
    cities$city_number,
    ~ read_rolling_indices(.)
  ) |>
  purrr::list_rbind() |>
  dplyr::relocate(area_name)

all_rolling_indices_chosen <-
  all_rolling_indices |> 
  dplyr::filter(
    window == "12_months",
    month_object == "2026-04-01"
  ) |> 
  dplyr::select(
    area_name,
    index_period,
    index_p, ci_lower, ci_upper, n_trp
  )

writexl::write_xlsx(
  all_rolling_indices_chosen,
  path = "spesialuttak/byindekser_glidende_2026_T1.xlsx"
)
