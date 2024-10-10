# Setup ----
{
source("rmd_setup.R")
source("get_from_trafficdata_api.R")
source("split_road_system_reference.R")
source("get_from_nvdb_api.R")
}

# Get TRP and crossings metainfo ----
latest_published_month <- 9

counties <-
  get_counties() |>
  dplyr::select(
    county_number,
    county_name,
    geo_number
  )

municipalities <-
  get_municipalities() |>
  dplyr::left_join(
    counties,
    by = "county_number"
  )

# Point metadata from Traffic Data API
points <-
  get_points() |>
  dplyr::distinct(
    trp_id,
    .keep_all = T
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    county_name,
    municipality_name,
    lat, lon,
    road_link_position
  ) |>
  dplyr::mutate(
    name = stringr::str_to_title(name, locale = "no")
  ) |>
  split_road_system_reference() |>
  dplyr::select(
    trp_id,
    name,
    #road_reference, road_category, road_number,
    #road_category_and_number,
    #section_number, subsection_number, meter,
    #intersection_part_number, intersection_meter,
    #county_name, municipality_name,
    lat, lon, road_link_position
  )

# All border crossings
border_crossings <-
  read.csv2(
    "border_index_data/border_crossings_complete.csv",
    encoding = "latin1"
  ) |>
  dplyr::mutate(
    road_category_and_number =
      paste0(road_category_shortname, "v ", road_number),
    aadt_short = round(adt * (1 - heavy_ratio / 100), digits = 0),
    aadt_long = adt - aadt_short
  ) |>
  dplyr::left_join(
    municipalities,
    by = "municipality_number"
  ) |>
  dplyr::arrange(
    desc(geo_number),
    desc(municipality_number)
  ) |>
  dplyr::select(
    road_category_and_number,
    street_name,
    geo_number,
    county_name,
    municipality_number,
    municipality_name,
    border_country, adt, aadt_short, aadt_long,
    year, trp_id
  ) |>
  dplyr::left_join(
    points,
    by = "trp_id"
  )

# TODO: find all 51 crossings in NVDB (remove csv dependency)

# Point AADT
# TODO: all from same source
border_trps <-
  border_crossings |>
  dplyr::filter(
    trp_id != ""
  ) |>
  dplyr::select(
    -adt,
    -aadt_short,
    -aadt_long,
    -year
  )

# Point AADT
adt <-
  get_aadt_by_length_for_trp_list(
    border_trps$trp_id
  )

adt_filtered <-
  adt |>
  dplyr::select(
    -sd_length_range
  ) |>
  dplyr::filter(
    length_range %in% c("[..,5.6)", "[5.6,..)")
  ) |>
  dplyr::mutate(
    length_range =
      if_else(
        length_range == "[..,5.6)",
        "short",
        "long"
      )
  ) |>
  tidyr::pivot_wider(
    names_from = length_range,
    names_prefix = "aadt_",
    values_from = aadt_length_range
  ) |>
  dplyr::mutate(
    length_quality = aadt_valid_length / aadt_total * 100
  ) |>
  #dplyr::filter(length_quality > 90) |>
  dplyr::filter(
    coverage > 50
  ) |>
  dplyr::group_by(trp_id) |>
  #dplyr::filter(year < 2020) |>
  dplyr::filter(year == max(year)) |>
  dplyr::select(
    trp_id,
    year,
    aadt_total,
    coverage,
    aadt_short,
    aadt_long,
    length_quality
  ) |>
  dplyr::rename(adt = aadt_total) |>
  dplyr::mutate(
    aadt_short =
      if_else(
        length_quality > 90,
        aadt_short,
        NA_real_
      ),
    aadt_long =
      if_else(
        length_quality > 90,
        aadt_long,
        NA_real_
      )
  ) |>
  dplyr::select(-length_quality)

border_trps_adt <-
  dplyr::left_join(
    border_trps,
    adt_filtered,
    by = "trp_id"
  )

# Must supply missing AADTs from NVDB based on road reference
# Non-trp crossings have manual AAFT values from 2018 in CSV
missing_aadt <-
  border_trps_adt |>
  dplyr::filter(adt == 0 | is.na(adt)) |>
  dplyr::mutate(
    adt = purrr::map(road_link_position, get_historic_aadt_by_roadlinkposition)) |>
  tidyr::unnest(
    cols = adt,
    names_sep = "_"
    ) |>
  dplyr::group_by(trp_id) |>
  dplyr::slice_max(adt_year) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    aadt_short = base::round(adt_aadt_total * (1 - adt_heavy_percentage / 100), 0),
    aadt_long =  base::round(adt_aadt_total * (adt_heavy_percentage / 100), 0)
  ) |>
  dplyr::select(
    -year,
    -adt_heavy_percentage,
    -adt_road_link_position,
    -adt_source
  ) |>
  dplyr::rename(
    adt = adt_aadt_total,
    year = adt_year
  )

with_aadt <-
  border_trps_adt |>
  dplyr::filter(adt > 0)

border_trps_adt_all <- dplyr::bind_rows(with_aadt, missing_aadt)

border_crossings_adt <-
  border_crossings |>
  dplyr::filter(trp_id == "") |>
  dplyr::bind_rows(border_trps_adt_all) |>
  dplyr::arrange(desc(geo_number), desc(municipality_number)) |>
  dplyr::select(-geo_number, -municipality_number)


# Get published index ----
{
index_2017 <- get_published_index_for_months(2952, 2017, 12)
index_2018 <- get_published_index_for_months(2952, 2018, 12)
index_2019 <- get_published_index_for_months(2952, 2019, 12)
index_2020 <- get_published_index_for_months(2952, 2020, 12)
index_2021 <- get_published_index_for_months(2952, 2021, 12)
index_2022 <- get_published_index_for_months(2952, 2022, 12)
index_2023 <- get_published_index_for_months(2952, 2023, 12)
index_2024 <- get_published_index_for_months(2952, 2024, latest_published_month)
}

{
pointindex_2017 <- get_published_pointindex_for_months_paginated(2952, 2017, 12)
pointindex_2018 <- get_published_pointindex_for_months_paginated(2952, 2018, 12)
pointindex_2019 <- get_published_pointindex_for_months_paginated(2952, 2019, 12)
pointindex_2020 <- get_published_pointindex_for_months_paginated(2952, 2020, 12)
pointindex_2021 <- get_published_pointindex_for_months_paginated(2952, 2021, 12)
pointindex_2022 <- get_published_pointindex_for_months_paginated(2952, 2022, 12)
pointindex_2023 <- get_published_pointindex_for_months_paginated(2952, 2023, 12)
pointindex_2024 <- get_published_pointindex_for_months_paginated(2952, 2024, latest_published_month)
}

pointindices <-
  dplyr::bind_rows(
    pointindex_2017[[2]],
    pointindex_2018[[2]],
    pointindex_2019[[2]],
    pointindex_2020[[2]],
    pointindex_2021[[2]],
    pointindex_2022[[2]],
    pointindex_2023[[2]],
    pointindex_2024[[2]]
  )

pointindices_all <-
  pointindices |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
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
  dplyr::mutate(length_range = "alle")

pointindices_light_n_heavy <-
  pointindices |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE
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

pointindices_by_length <-
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
  )

n_points <- pointindices_by_length |>
  dplyr::group_by(year, month, period, day_type, length_range) |>
  dplyr::summarise(no_points = n()) |>
  dplyr::mutate(road_category = "Europa-, riks- og fylkesveg") |>
  dplyr::ungroup()


# For an SD report
# index_2020_wide <- index_2020 |>
#   dplyr::select(area_name, year, month, road_category, length_range, index = index_p,
#                 confidence_width, period) |>
#   dplyr::mutate(length_range = dplyr::case_when(length_range == "[..,..)" ~ "total",
#                                                 length_range == "[..,5.6)" ~ "short",
#                                                 length_range == "[5.6,..)" ~ "long")) |>
#   tidyr::pivot_wider(names_from = length_range,
#                      values_from = c(index, confidence_width)) |>
#   dplyr::mutate(period = dplyr::case_when(period == "month" ~ "Måned",
#                                           period == "year_to_date" ~ "Hittil i år"),
#                 road_category = dplyr::case_when(
#                   road_category == "FYLKESVEG" ~ "Fylkesveg",
#                   road_category == "EUROPAVEG_RIKSVEG" ~ "Europa- og riksveg",
#                   road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG" ~ "Europa-, riks- og fylkesveg")) |>
#   dplyr::arrange(month, road_category, desc(period))
#
# write.csv2(index_2020_wide, file = "sd/riksgrenseindeks.csv",
#            row.names = F)

index_for_table <-
  dplyr::bind_rows(
    index_2017,
    index_2018,
    index_2019,
    index_2020,
    index_2021,
    index_2022,
    index_2023,
    index_2024
  ) |>
  dplyr::mutate(
    length_range =
      dplyr::case_when(
        length_range == "[..,..)" ~ "alle",
        length_range == "[..,5.6)" ~ "lette",
        length_range == "[5.6,..)" ~ "tunge"
      )
  ) |>
  dplyr::mutate(
    road_category =
      dplyr::case_when(
        road_category == "FYLKESVEG" ~ "Fylkesveg",
        road_category == "EUROPAVEG_RIKSVEG" ~ "Europa- og riksveg",
        road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG" ~ "Europa-, riks- og fylkesveg"
      )
  ) |>
  dplyr::mutate(
    month_object = lubridate::make_date(year = year, month = month),
    month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)
  ) |>
  dplyr::filter(
    road_category == "Europa-, riks- og fylkesveg",
    day_type == "ALL"
  ) |>
  dplyr::select(
    road_category,
    length_range,
    year,
    month,
    period,
    month_object,
    month_name,
    standard_deviation,
    index_p
  ) |>
  dplyr::left_join(
    n_points,
    by = c("year", "month", "period", "road_category", "length_range")
  ) |>
  dplyr::mutate(
    standard_error = round(standard_deviation / sqrt(no_points), digits = 1),
    standard_deviation = round(standard_deviation, digits = 1)
  )

#last_month_name <-
#  base::max(index_this_year$month_object) |>
#  lubridate::month(label = TRUE, abbr = FALSE)

index_all_years <-
  dplyr::bind_rows(
    index_2017,
    index_2018,
    index_2019,
    index_2020,
    index_2021,
    index_2022,
    index_2023,
    index_2024
  ) |>
  dplyr::mutate(
    length_range =
      dplyr::case_when(
        length_range == "[..,..)" ~ "alle",
        length_range == "[..,5.6)" ~ "lette",
        length_range == "[5.6,..)" ~ "tunge")) |>
  dplyr::mutate(
    road_category =
      dplyr::case_when(
        road_category == "FYLKESVEG" ~ "Fylkesveg",
        road_category == "EUROPAVEG_RIKSVEG" ~ "Europa- og riksveg",
        road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG" ~ "Europa-, riks- og fylkesveg"
      )
  ) |>
  dplyr::mutate(
    # setting all years to same is a trick to get the plot facet correct
    month_object = lubridate::make_date(year = 2000, month = month),
    month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)
  ) |>
  dplyr::filter(
    road_category == "Europa-, riks- og fylkesveg",
    day_type == "ALL"
  )


# Write RDS ----
readr::write_rds(
  border_crossings_adt,
  file = "border_index_data/border_crossings_adt.rds"
)

readr::write_rds(
  index_for_table,
  file = "border_index_data/index_for_table.rds"
)

readr::write_rds(
  index_all_years,
  file = "border_index_data/index_all_years.rds"
)

