# Prosentandel tunge kjøretøy i by

# Byer med BVA, foruten Tromsø.

# 2017 NJær   952
# 2018 Oslo   959
# 2018 Berg  8952
# 2019 Tron   960

city_numbers <- c(
  952,
  959,
  8952,
  960
)

city_names <- c(
  "Nord-Jæren",
  "Oslo",
  "Bergen",
  "Trondheim"
)

# Data for 2023
city_trp_all <-
  purrr::map2(
    city_numbers,
    city_names,
    ~ readr::read_rds(
      file = paste0(
        "index_trp_metadata/trp_",
        .x,
        ".rds"
      )
    ) |>
      dplyr::mutate(
        city = .y
      )
  ) |>
  purrr::list_rbind() |>
  dplyr::filter(
    station_type_short %in% c("T", NA_real_)
  )


adt_heavy <-
  get_aadt_by_length_for_trp_list(city_trp_all$trp_id) |>
  dplyr::filter(
    year == 2023,
    coverage > 50,
    length_range == "[5.6,..)"
  )


#
heavy_ratio <-
  city_trp_all |>
  dplyr::select(
    trp_id,
    city
  ) |>
  dplyr::inner_join(
    adt_heavy,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::summarise(
    n_trp = n(),
    heavy_percentage = 100 * (sum(aadt_length_range) / sum(aadt_total)) |> round(2),
    #.by = c(city)
  )

