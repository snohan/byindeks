# Filtering from Excel file ----

# keep months as number, put easter as 34 and pentecost as 56
mdt_manual_exclusions <-
  readxl::read_excel(
    "trp_mdt_manual_exclusions.xlsx"
  ) |>
  dplyr::left_join(
    universal_calendar_periods |>
      dplyr::select(
        from_universal_year_period_id = universal_year_period_id,
        year,
        month_id
      ),
    by = dplyr::join_by(
      from_year == year,
      from_month == month_id
    )
  ) |>
  dplyr::left_join(
    universal_calendar_periods |>
      dplyr::select(
        to_universal_year_period_id = universal_year_period_id,
        year,
        month_id
      ),
    by = dplyr::join_by(
      to_year == year,
      to_month == month_id
    )
  ) |>
  # Since Easter and Pentecost hasn't been explicitly given in the Excel file,
  # these must be added when the exclusion period is beginning with April, or ending with March.
  # And same thing for Pentecost
  dplyr::mutate(
    from_universal_year_period_id =
      dplyr::case_when(
        from_month == 4 ~ from_universal_year_period_id - 1,
        from_month == 6 ~ from_universal_year_period_id - 1,
        TRUE ~ from_universal_year_period_id
      ),
    to_universal_year_period_id =
      dplyr::case_when(
        to_month == 3 ~ to_universal_year_period_id + 1,
        to_month == 5 ~ to_universal_year_period_id + 1,
        TRUE ~ to_universal_year_period_id
      )
  )


# Add metainfo to Excel ----
# TODO: make a new Excel based on cmdt
# points <- readr::read_rds("trps_for_city_index.rds")
#
# mdt_manual_exclusions_meta <-
#   mdt_manual_exclusions |>
#   dplyr::left_join(
#     points,
#     by = "trp_id"
#   ) |>
#   dplyr::select(
#     trp_id,
#     name,
#     county_name,
#     municipality_name,
#     road_category_and_number,
#     from_year,
#     from_month,
#     to_year,
#     to_month
#   ) |>
#   dplyr::arrange(
#     county_name,
#     name
#   ) |>
#   dplyr::filter(
#     trp_id != "dummy"
#   )
#
# writexl::write_xlsx(
#   mdt_manual_exclusions_meta,
#   path = "trp_mdt_manual_exclusions_meta.xlsx"
# )


# Filter by city id ----
mdt_manual_exclusions <-
  mdt_manual_exclusions |>
  dplyr::filter(
    is.na(index_id) | index_id == city_number
    # An exclusion is meant for either all indexes (empty index_id) or a specific index
  )


# Exclude TRP all time ----
all_timers <-
  mdt_manual_exclusions |>
  dplyr::filter(
    is.na(from_year)
  ) |>
  dplyr::select(trp_id)


# Exclude TRP from and indefinitely onwards ----
# Make all months to exclude explicit
year_fromers <-
  mdt_manual_exclusions |>
  dplyr::filter(
    !(is.na(from_year)) &
    is.na(to_year)
  ) |>
  dplyr::mutate(
    to_universal_year_period_id = latest_universal_year_period_id
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    universal_year_period_id = list(
      uids = c(from_universal_year_period_id:to_universal_year_period_id)
    )
  ) |>
  dplyr::select(
    trp_id,
    universal_year_period_id
  ) |>
  tidyr::unnest(
    universal_year_period_id
  )


# Exclude TRP period sequence ----
# Make all months to exclude explicit
month_sequencers <-
  mdt_manual_exclusions |>
  dplyr::filter(
    !(is.na(to_year))
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    universal_year_period_id = list(
      uids = c(from_universal_year_period_id:to_universal_year_period_id)
    )
  ) |>
  dplyr::select(
    trp_id,
    universal_year_period_id
  ) |>
  tidyr::unnest(
    universal_year_period_id
  )


# Validated MDTs ----
mdt_validated <-
  mdt_filtered |>
  dplyr::filter(
    !(trp_id %in% all_timers$trp_id)
  ) |>
  dplyr::left_join(
    universal_calendar_periods |>
      dplyr::select(
        universal_year_period_id,
        year,
        period_name
      ),
    by = dplyr::join_by(year, month == period_name)
  ) |>
  dplyr::anti_join(
    year_fromers,
    by = c("trp_id", "universal_year_period_id")
  ) |>
  dplyr::anti_join(
    month_sequencers,
    by = c("trp_id", "universal_year_period_id")
  )
