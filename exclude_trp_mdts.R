mdt_validated <-
  mdt_filtered |>
  dplyr::filter(
    # Bybrua sør
    !(trp_id == "17949V320695" & year > 2019)
  ) |>
  dplyr::filter(
    # Åsedalen
    !(trp_id == "43296V319721" & year > 2018)
  ) |>
  dplyr::filter(
    # Storhaugtunnelen
    !(trp_id == "57279V320244" & year > 2020)
  ) |>
  dplyr::filter(
    # Randabergveien
    !(trp_id == "10795V320297" &
        year_month %in% base::seq.Date(
          lubridate::make_date(2020, 11, 01),
          lubridate::make_date(2022, 9, 01),
          by = "month")
    )
  ) |>
  dplyr::filter(
    # Nordvik
    !(trp_id == "62279V805789" &
        year_month %in% base::seq.Date(
          lubridate::make_date(2022, 3, 01),
          lubridate::make_date(2022, 4, 01),
          by = "month")
    )
  ) |>
  dplyr::filter(
    !(trp_id %in% c(
      "18573V444291", # Fjellsrud
      "18788V1811746", # Mogreina pårampe
      "63515V1811747", # Mogreina avrampe
      "42754V444240" # Slattum
    )
    )
  ) |>
  dplyr::filter(
    # Torsvik
    !(trp_id == "18321V121493" &
        year_month %in% base::seq.Date(
          lubridate::make_date(2016, 9, 01),
          lubridate::make_date(2017, 3, 01),
          by = "month")
    )
  )
