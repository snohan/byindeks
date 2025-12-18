{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
}

# Get data ----
counties <-
  get_counties() |>
  dplyr::select(
    county_name,
    country_part_name
  )

country_parts <- get_country_parts()

points <-
  get_points() |>
  dplyr::distinct(trp_id, .keep_all = T) |>
  split_road_system_reference() |>
  dplyr::select(
    trp_id,
    name,
    road_category,
    road_reference,
    county_name,
    lat, lon
  ) |>
  dplyr::mutate(
    road_category = dplyr::case_when(
      road_category == "F" ~ "Fylkesveg",
      road_category == "E" ~ "Europa- og riksveg",
      road_category == "R" ~ "Europa- og riksveg"
    )
  ) |>
  dplyr::left_join(
    counties,
    by = "county_name"
  )

## Choose month ----
this_year <- 2025
latest_month_number <- 11

index_this_year <-
  get_published_road_traffic_index_for_months(
    962,
    this_year,
    latest_month_number
  )


# Fetching pointindices for last month's year_to_date
# in order to calculate the standard error
pointindex_this_year <-
  get_published_pointindex_paginated(
    962,
    this_year,
    latest_month_number
  )


# Prepare ----
pointindices <- pointindex_this_year[[2]]

# Sidetrack: Spotfire
# pointindices_spotfire <-
#   pointindices |>
#   dplyr::filter(
#     is_excluded == FALSE,
#     is_manually_excluded == FALSE,
#     length_excluded == FALSE,
#     period == "month",
#     day_type == "ALL",
#     !is.na(index_total_p)
#   ) |>
#   dplyr::left_join(
#     points,
#     by = dplyr::join_by(trp_id)
#   ) |>
#   dplyr::select(
#     trp_id,
#     name,
#     road_reference,
#     county_name,
#     lat, lon,
#     year,
#     month,
#     index_p = index_total_p,
#     index_total_coverage,
#     length_coverage
#   )
#
# readr::write_excel_csv2(
#   pointindices_spotfire,
#   "spesialuttak/vti_trp_index_spotfire_25-05.csv"
# )


# Sidetrack: Manually excluded
# pointindices_manually_excluded <-
#   pointindices |>
#   dplyr::filter(
#     #is_excluded == FALSE,
#     is_manually_excluded == TRUE,
#     period == "year_to_date",
#     day_type == "ALL",
#     !is.na(index_total_p)
#   ) |>
#   dplyr::select(
#     trp_id,
#     year,
#     month,
#     period,
#     day_type,
#     index_p = index_total_p
#   ) |>
#   dplyr::left_join(
#     points,
#     by = join_by(trp_id)
#   )
#
# writexl::write_xlsx(
# #readr::write_csv2(
#   pointindices_manually_excluded,
#   "manuelt_ekskluderte_punkter_2024.xlsx"
# )

# Back on track
pointindices_all <-
  pointindices |>
  dplyr::filter(
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    period == "year_to_date",
    !is.na(index_total_p)
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    period,
    day_type,
    index_p = index_total_p
  ) |>
  dplyr::mutate(
    length_range = "alle"
  )

pointindices_light_n_heavy <-
  pointindices |>
  dplyr::filter(
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "year_to_date"
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    period,
    day_type,
    index_short,
    index_long
  ) |>
  tidyr::pivot_longer(
    cols = c("index_short", "index_long"),
    names_to = "length_range",
    values_to = "index_p"
  ) |>
  dplyr::mutate(
    length_range =
      dplyr::case_when(
        length_range == "index_short" ~ "lette",
        length_range == "index_long" ~ "tunge"
      )
  )

pointindices_year_to_date <-
  dplyr::bind_rows(
    pointindices_all,
    pointindices_light_n_heavy
  ) |>
  dplyr::mutate(
    day_type =
      dplyr::case_when(
        day_type == "ALL" ~ "alle",
        day_type == "WEEKDAY" ~ "yrkedøgn",
        day_type == "WEEKEND" ~ "helgedøgn"
      )
  ) |>
  dplyr::left_join(
    points,
    by = join_by(trp_id)
  )


# N TRPs ----
# Need to find number of TRPs for each aggregated index variant
# County, r, f
# County, rf
# Country_part, r, f
# Country_part, rf
# Country, r, f
# Country, rf

number_of_pointindices_county_r_f <-
  pointindices_year_to_date |>
  dplyr::group_by(
    year,
    month,
    period,
    day_type,
    length_range,
    road_category,
    county_name
  ) |>
  dplyr::summarise(no_points = n()) |>
  dplyr::mutate(area_type = "COUNTY") |>
  dplyr::rename(area_name = county_name) |>
  dplyr::ungroup()

number_of_pointindices_county_rf <-
  pointindices_year_to_date |>
  dplyr::group_by(
    year,
    month,
    period,
    day_type,
    length_range,
    county_name
  ) |>
  dplyr::summarise(no_points = n()) |>
  dplyr::mutate(
    area_type = "COUNTY",
    road_category = "Europa-, riks- og fylkesveg"
  ) |>
  dplyr::rename(area_name = county_name) |>
  dplyr::ungroup()

number_of_pointindices_county_part_r_f <-
  pointindices_year_to_date |>
  dplyr::group_by(
    year,
    month,
    period,
    day_type,
    length_range,
    road_category,
    country_part_name
  ) |>
  dplyr::summarise(no_points = n()) |>
  dplyr::mutate(area_type = "COUNTRY_PART") |>
  dplyr::rename(area_name = country_part_name) |>
  dplyr::ungroup()

number_of_pointindices_county_part_rf <-
  pointindices_year_to_date |>
  dplyr::group_by(
    year,
    month,
    period,
    day_type,
    length_range,
    country_part_name
  ) |>
  dplyr::summarise(no_points = n()) |>
  dplyr::mutate(
    area_type = "COUNTRY_PART",
    road_category = "Europa-, riks- og fylkesveg"
  ) |>
  dplyr::rename(area_name = country_part_name) |>
  dplyr::ungroup()

number_of_pointindices_country_r_f <-
  pointindices_year_to_date |>
  dplyr::group_by(
    year,
    month,
    period,
    day_type,
    length_range,
    road_category
  ) |>
  dplyr::summarise(no_points = n()) |>
  dplyr::mutate(
    area_type = "COUNTRY",
    area_name = "Norge"
  ) |>
  dplyr::ungroup()

number_of_pointindices_country_rf <-
  pointindices_year_to_date |>
  dplyr::group_by(
    year,
    month,
    period,
    day_type,
    length_range
  ) |>
  dplyr::summarise(no_points = n()) |>
  dplyr::mutate(
    area_type = "COUNTRY",
    road_category = "Europa-, riks- og fylkesveg",
    area_name = "Norge") |>
  dplyr::ungroup()

number_of_point_indices <- dplyr::bind_rows(
  number_of_pointindices_county_r_f,
  number_of_pointindices_county_rf,
  number_of_pointindices_county_part_r_f,
  number_of_pointindices_county_part_rf,
  number_of_pointindices_country_r_f,
  number_of_pointindices_country_rf
)

index_this_year_prepared <-
  index_this_year |>
  dplyr::mutate(
    length_range =
      dplyr::case_when(
        length_range == "[..,..)" ~ "alle",
        length_range == "[..,5.6)" ~ "lette",
        length_range == "[5.6,..)" ~ "tunge"
      ),
    day_type =
      dplyr::case_when(
        day_type == "ALL" ~ "alle",
        day_type == "WEEKDAY" ~ "yrkedøgn",
        day_type == "WEEKEND" ~ "helgedøgn"
      )
  ) |>
  dplyr::mutate(
    road_category = dplyr::case_when(
      road_category == "FYLKESVEG" ~ "Fylkesveg",
      road_category == "EUROPAVEG_RIKSVEG" ~ "Europa- og riksveg",
      road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG" ~ "Europa-, riks- og fylkesveg")
  ) |>
  dplyr::mutate(
    area_name = dplyr::case_when(
      area_name == "OSLO_OG_VIKEN" ~ "Oslo og Viken",
      area_name == "INNLANDET" ~ "Innlandet",
      area_name == "AGDER_OG_SOROSTLANDET" ~ "Agder og Sør-Østlandet",
      area_name == "VESTLANDET" ~ "Vestlandet",
      area_name == "TRONDELAG" ~ "Trøndelag",
      area_name == "NORD_NORGE" ~ "Nord-Norge",
      TRUE ~ area_name)
  ) |>
  dplyr::mutate(
    month_object = lubridate::make_date(year = year, month = month),
    month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)
  ) |>
  dplyr::left_join(
    number_of_point_indices,
    by = c(
      "area_name",
      "area_type",
      "year",
      "month",
      "period",
      "road_category",
      "length_range",
      "day_type"
    )
  ) |>
  dplyr::mutate(
    road_category =
      factor(
        road_category,
        levels =
          c(
            "Fylkesveg",
            "Europa- og riksveg",
            "Europa-, riks- og fylkesveg"
          ),
        ordered = TRUE
      ),
    standard_error =
      round(standard_deviation / sqrt(no_points), digits = 1),
    standard_deviation = round(standard_deviation, digits = 1)
  )


index_this_year_prepared_wide <-
  index_this_year_prepared |>
  dplyr::filter(
    area_type == "COUNTY",
    period == "month",
    length_range == "alle",
    road_category == "Europa-, riks- og fylkesveg",
    day_type == "alle"
  ) |>
  dplyr::mutate(
    month_name_short =
      lubridate::month(month_object, label = TRUE, abbr = TRUE),
    index_p = round(index_p, digits = 1),
    area_name = factor(area_name, levels = unique(counties$county_name))
  ) |>
  dplyr::select(
    area_name,
    index_p,
    month_name_short
  ) |>
  tidyr::pivot_wider(
    names_from = month_name_short,
    values_from = index_p
  ) |>
  dplyr::arrange(area_name)

index_this_year_prepared_wide_country <-
  index_this_year_prepared |>
  dplyr::filter(
    area_type == "COUNTRY",
    period == "month",
    length_range == "alle",
    road_category == "Europa-, riks- og fylkesveg"#,
    #day_type == "alle"
  ) |>
  dplyr::mutate(
    month_name_short =
      lubridate::month(month_object, label = TRUE, abbr = TRUE),
    index_p = round(index_p, digits = 1),
    area_name = "Noreg"
  ) |>
  dplyr::select(
    area_name,
    day_type,
    index_p,
    month_name_short
  ) |>
  tidyr::pivot_wider(
    names_from = month_name_short,
    values_from = index_p
  )


# Write RDS ----
readr::write_rds(
  index_this_year_prepared,
  file = "road_traffic_index_files/index_this_year_prepared.rds"
)

readr::write_rds(
  index_this_year_prepared_wide,
  file = "road_traffic_index_files/index_this_year_prepared_wide.rds"
)

readr::write_rds(
  index_this_year_prepared_wide_country,
  file = "road_traffic_index_files/index_this_year_prepared_wide_country.rds"
)
