base::Sys.setlocale(locale = "nb.utf8")
library(tidyverse)
library(writexl)


# Read prepared data
city_number <- c(960, 952, 959, 8952)
city_name <- c("Trondheim", "Nord-Jæren", "Osloområdet", "Bergensområdet")

city_tibble <-
  tibble::tibble(
    city_number = city_number,
    area_name = city_name
  )

index_yearly <-
  purrr::map(
    city_number,
    ~ readr::read_rds(
      paste0("data_indexpoints_tidy/byindeks_", .x, ".rds")
    )
  ) |>
  purrr::list_rbind()

index_yearly_direct <-
  index_yearly |>
  dplyr::filter(
    year <= 2023,
    index_type == "direct"
  ) |>
  dplyr::select(
    area_name,
    n_trp
  )

index_yearly_chained <-
  index_yearly |>
  dplyr::filter(
    year == 2023,
    index_type == "chained"
  ) |>
  dplyr::select(
    area_name,
    index_period = year_from_to,
    index_type,
    index_p,
    n_trp,
    standard_error,
    ci_lower,
    ci_upper
  )

index_rolling <-
  purrr::map(
    city_number,
    ~ readr::read_rds(
      paste0("data_indexpoints_tidy/rolling_indices_", .x, ".rds")
    ) |>
      purrr::list_rbind() |>
      dplyr::mutate(
        city_number = .x
      )
  ) |>
  purrr::list_rbind() |>
  dplyr::left_join(
    city_tibble,
    by = dplyr::join_by(city_number)
  ) |>
  dplyr::filter(
    window == "12_months",
    month_object == "2023-12-01"
  ) |>
  dplyr::select(
    area_name,
    index_period,
    index_p,
    n_trp,
    standard_error = standard_error_p,
    ci_lower,
    ci_upper
  ) |>
  dplyr::mutate(
    index_type = "12_month_rolling"
  )

index_compare <-
  bind_rows(
    index_rolling,
    index_yearly_chained
  ) |>
  dplyr::arrange(
    area_name
  ) |>
  dplyr::mutate(
    index_p = round(index_p, 1),
    standard_error = round(standard_error, 1)
  )


writexl::write_xlsx(
  index_compare,
  "spesialuttak/sammenlign_kjedet_og_glidende.xlsx"
)
