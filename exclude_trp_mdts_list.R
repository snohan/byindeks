# Filtering from Excel file

mdt_manual_exclusions <-
  readxl::read_excel(
    "trp_mdt_manual_exclusions.xlsx"
  )


# Add metainfo and write Excel for sharing
points <- readr::read_rds("trps_for_city_index.rds")

mdt_manual_exclusions_meta <-
  mdt_manual_exclusions |>
  dplyr::left_join(
    points,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    name,
    county_name,
    municipality_name,
    road_category_and_number,
    from_year,
    from_month,
    to_year,
    to_month
  ) |>
  dplyr::arrange(
    county_name,
    name
  ) |>
  dplyr::filter(
    trp_id != "dummy"
  )

writexl::write_xlsx(
  mdt_manual_exclusions_meta,
  path = "trp_mdt_manual_exclusions_meta.xlsx"
)


# Filter for city
mdt_manual_exclusions <-
  mdt_manual_exclusions |>
  dplyr::filter(
    is.na(index_id) | index_id == city_number
    # An exclusion is meant for either all indexes (empty index_id) or a specific index
  )

# Exclude TRP all time
all_timers <-
  mdt_manual_exclusions |>
  dplyr::filter(
    is.na(from_year)
  )

# Exclude TRP from year
# Make all months to exclude explicit
year_fromers <-
  mdt_manual_exclusions |>
  dplyr::filter(
    !(is.na(from_year)) &
    is.na(to_year)
  ) |>
  dplyr::mutate(
    from_date = lubridate::make_date(from_year, from_month, 1)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    year_month = list(
      dates = base::seq.Date(
        from_date,
        last_year_month,
        by = "month")
    )
  ) |>
  dplyr::select(
    trp_id,
    year_month
  ) |>
  tidyr::unnest(
    year_month
  )


# Exclude TRP month sequence
# Make all months to exclude explicit
month_sequencers <-
  mdt_manual_exclusions |>
  dplyr::filter(
    !(is.na(to_year))
  ) |>
  dplyr::mutate(
    from_date = lubridate::make_date(from_year, from_month, 1),
    to_date = lubridate::make_date(to_year, to_month, 1)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    year_month = list(
      dates = base::seq.Date(
        from_date,
        to_date,
        by = "month")
    )
  ) |>
  dplyr::select(
    trp_id,
    year_month
  ) |>
  tidyr::unnest(
    year_month
  )

#
mdt_validated <-
  mdt_filtered |>
  dplyr::filter(
    !(trp_id %in% all_timers$trp_id)
  ) |>
  dplyr::anti_join(
    year_fromers,
    by = c("trp_id", "year_month")
  ) |>
  dplyr::anti_join(
    month_sequencers,
    by = c("trp_id", "year_month")
  )





