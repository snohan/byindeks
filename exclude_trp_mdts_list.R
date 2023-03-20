# TODO: recreate filtering from Excel file

mdt_manual_exclusions <-
  readxl::read_excel(
    "trp_mdt_manual_exclusions.xlsx"
  )


# Add metainfo and write Excel for sharing
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
  )

writexl::write_xlsx(
  mdt_manual_exclusions_meta,
  path = "trp_mdt_manual_exclusions_meta.xlsx"
)


# Exclude TRP all time
all_timers <-
  mdt_manual_exclusions |>
  dplyr::filter(
    is.na(from_year)
  )

# Exclude TRP from year
year_fromers <-
  mdt_manual_exclusions |>
  dplyr::filter(
    !(is.na(from_year)) &
    is.na(to_year)
  )

# Exclude TRP month sequence
month_sequencers <-
  mdt_manual_exclusions |>
  dplyr::filter(
    !(is.na(to_year))
  ) |>
  dplyr::mutate(
    from_date = lubridate::make_date(from_year, from_month, 1),
    to_date = lubridate::make_date(to_year, to_month, 1)
  )

#
mdt_validated <-
  mdt_filtered |>
  dplyr::filter(
    !(trp_id %in% all_timers$trp_id)
  ) |>
  # year_fromers
  # anti join?
  dplyr::filter(

  )
# month sequencers
# anti join?





