# Make one Excel file


cities <-
  tibble::tibble(
    city_number = c(
      "952",
      "959",
      "8952",
      "960"
    ),
    area_name = c(
      "Nord-Jæren",
      "Osloområdet",
      "Bergensområdet",
      "Trondheimsområdet"
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


# aftenbladet <-
#   all_rolling_indices |>
#   dplyr::select(-city_number) |>
#   dplyr::filter(
#     window == "12_months",
#     month_object <= "2023-08-01"
#   )
#
# writexl::write_xlsx(
#   aftenbladet,
#   path = "spesialuttak/aftenbladet.xlsx"
# )

opa <-
  all_rolling_indices |>
  dplyr::select(-city_number) |>
  dplyr::filter(
    window == "36_months",
    month_object <= "2023-10-01"
  )

writexl::write_xlsx(
  opa,
  path = "spesialuttak/bva_byindekser.xlsx"
)
