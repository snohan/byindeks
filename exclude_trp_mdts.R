mdt_validated <-
  mdt_filtered |>
  # Nord-Jæren
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
          lubridate::make_date(2023, 12, 01),
          by = "month")
    )
  ) |>
  # Bergen
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
      "68351V319882", # Kannik
      "18573V444291", # Fjellsrud
      "18788V1811746", # Mogreina pårampe
      "63515V1811747", # Mogreina avrampe
      "42754V444240", # Slattum
      "35376V181262", # Veungsdalen
      "70046V180859", # Kongsberg kro
      "26634V181322", # Krekling
      "98723V971842", # Veumveien
      "66206V805614", # Røykenes
      "56658V804775", # Vallaheiane
      "02940V805615", # Nesttuntunnelen
      "99254V1696548", # Troldhaugtunnelen - Lagunen
      "16868V805119", # Lagunen - Troldhaugtunnelen og Sørås
      "61215V2782426", # Fana ved Kirkevoll
      "62279V805789" # Nordvik
      )
    )
  ) |>
  # KRS
  dplyr::filter(
    # Torsvik
    !(trp_id == "18321V121493" &
        year_month %in% c(
          lubridate::make_date(2016, 9, 01),
          lubridate::make_date(2017, 3, 01)
          )
    )
  ) |>
  dplyr::filter(
    # Mjåvann
    !(trp_id == "35258V2475662" &
        year_month >= lubridate::make_date(2022, 11, 01)
    )
  ) |>
  dplyr::filter(
    # Volleberg
    !(trp_id == "35258V2475662" &
        year_month >= lubridate::make_date(2022, 11, 01)
    )
  ) |>
  # Buskerudbyen
  dplyr::filter(
    # Nymoen
    !(trp_id == "04544V181344" &
        year_month %in% c(
          lubridate::make_date(2016, 5, 01),
          lubridate::make_date(2016, 6, 01),
          lubridate::make_date(2016, 9, 01)
        )
    )
  ) |>
  dplyr::filter(
    # Darbu vest
    !(trp_id == "08002V181261" &
        year_month %in% c(
          lubridate::make_date(2016, 6, 01),
          lubridate::make_date(2016, 7, 01)
        )
    )
  ) |>
  dplyr::filter(
    # Lierbyen nord
    !(trp_id == "23026V181320" &
        year_month %in% c(
          lubridate::make_date(2021, 11, 01),
          lubridate::make_date(2021, 12, 01)
        )
    )
  ) |>
  dplyr::filter(
    # Hokksund vest
    !(trp_id == "63545V180918" &
        year_month %in% base::seq.Date(
          lubridate::make_date(2018, 6, 01),
          lubridate::make_date(2019, 12, 01),
          by = "month")
    )
  ) |>
  dplyr::filter(
    # Gulskogen
    !(trp_id == "77387V181050" &
        year_month %in% c(
          lubridate::make_date(2017, 01, 01),
          lubridate::make_date(2017, 02, 01)
        )
    )
  ) |>
  dplyr::filter(
    # Herstrøm
    !(trp_id == "77103V181318" &
        year_month %in% c(
          lubridate::make_date(2017, 01, 01),
          lubridate::make_date(2017, 02, 01)
        )
    )
  ) |>
  dplyr::filter(
    # Drammen travbane
    !(trp_id == "46268V181318" &
        year_month %in% c(
          lubridate::make_date(2017, 01, 01),
          lubridate::make_date(2017, 02, 01)
        )
    )
  ) |>
  dplyr::filter(
    # Øvre Sund bru
    !(trp_id == "94099V2038395" &
        year_month %in% c(
          lubridate::make_date(2017, 01, 01),
          lubridate::make_date(2017, 02, 01)
        )
    )
  ) |>
  # Grenland
  dplyr::filter(
    # Bambletunnelen syd
    !(trp_id == "00344V521377" &
        year_month %in% base::seq.Date(
          lubridate::make_date(2021, 4, 01),
          lubridate::make_date(2022, 4, 01),
          by = "month")
    )
  ) |>
  dplyr::filter(
    # Høgenheitunnelen
    !(trp_id == "86022V521170" &
        year_month %in% base::seq.Date(
          lubridate::make_date(2021, 4, 01),
          lubridate::make_date(2022, 4, 01),
          by = "month")
    )
  ) |>
  dplyr::filter(
    # Høgenheitunnelen
    !(trp_id == "86022V521170" &
        year_month %in% base::seq.Date(
          lubridate::make_date(2017, 4, 01),
          lubridate::make_date(2018, 8, 01),
          by = "month")
    )
  ) |>
  dplyr::filter(
    # Sverresgate
    !(trp_id == "62672V491082" &
        year_month %in% base::seq.Date(
          lubridate::make_date(2019, 4, 01),
          lubridate::make_date(2019, 12, 01),
          by = "month")
    )
  ) |>
  dplyr::filter(
    # Menstadbrua
    !(trp_id == "79492V521118" &
        year_month %in% base::seq.Date(
          lubridate::make_date(2019, 4, 01),
          lubridate::make_date(2019, 12, 01),
          by = "month")
    )
  ) |>
  # Nedre Glomma
  dplyr::filter(
    # Værstebrua
    !(trp_id == "08132V1984223" &
        year_month %in% base::seq.Date(
          lubridate::make_date(2021, 8, 01),
          lubridate::make_date(2021, 10, 01),
          by = "month")
    )
  )


