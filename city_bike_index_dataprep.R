# Preparation of data for reporting
# Gathering info on all trp and indexes and writing them to RDS

# Setup ----
source("rmd_setup.R")
source("get_from_trafficdata_api.R")

# Functions
# source TAKLER IKKE Ø som brukes i kolonneoverskrift i csv-ene! Må åpne fila og kjøre alt derfra.
source("indexpoints_tidying_functions.R")

library(viridis)
options(warn=-1)
svv_background_color <- "#F5F5F5"


# Targets ----
# Make RDS files per city for use in report generation
# (visualisation used in report)
# 1. TRP meta info file
#   - road reference and municipality (table)
#   - lat lon (map)
# 2. TRP AADT file
#   - AADT for each year (table)
# 3. City index file
#   - with SE per year and accumulated (table)


# TRP info ----
trp <-
  get_points() %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    county_name,
    municipality_name,
    lat, lon, road_link_position
  ) %>%
  split_road_system_reference() %>%
  dplyr::mutate(
    name = stringr::str_to_title(name, locale = "nb")
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    road_category,
    road_number,
    road_category_and_number,
    section_number, subsection_number, meter,
    intersection_part_number, intersection_meter,
    county_name, municipality_name,
    lat, lon, road_link_position
  )


# City numbers ----
# Bergen 5952
# Førde 9952
# Grenland 4953
# Nedre Glomma 5953
# Nord-Jæren 6952
# Oslo 6953
# Tromsø 11952
# Vestfold 12952

# Choose
index_month <- 12 # the one to be published now
city_number <- 12952

reference_year <-
  dplyr::case_when(
    city_number %in% c(
      9952,
      4953,
      6953,
      12952
    ) ~ 2017,
    city_number %in% c(
      5952,
      5953
    ) ~ 2018,
    city_number %in% c(
      6952
    ) ~ 2019,
    city_number %in% c(
      11952
    ) ~ 2021
  )

previous_years <-
  base::seq(reference_year + 1, 2021, 1)

all_years <-
  base::seq(reference_year, 2022, 1)


# Get index numbers ----
city_index_previous_years <-
  purrr::map_dfr(
    previous_years,
    ~ get_published_index_for_months(
      city_number,
      .,
      12
    )
  )

city_index <-
  get_published_index_for_months(city_number, 2022, index_month) |>
  dplyr::bind_rows(
    city_index_previous_years
  ) |>
  dplyr::filter(
    length_range == "[..,..)",
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG"
  ) |>
  dplyr::select(
    area_name,
    year,
    month,
    period,
    index_p,
    index_i,
    calc_volume,
    base_volume,
    standard_deviation
  )

city_name <-
  city_index$area_name[1]

trp_index_previous_years <-
  purrr::map_dfr(
    previous_years,
    ~ get_published_bikepointindex_for_months(
      city_number,
      .,
      12
    )[[2]]
  )

trp_index <-
  get_published_bikepointindex_for_months(
    city_number,
    2022,
    index_month
  )[[2]] |>
  dplyr::bind_rows(
    trp_index_previous_years
  )

# n_trp_per_month <-
#   trp_index |>
#   dplyr::filter(
#     day_type == "ALL",
#     is_excluded == FALSE,
#     is_manually_excluded == FALSE,
#     period == "month"
#   ) |>
#   dplyr::group_by(
#     year,
#     month
#   ) |>
#   dplyr::summarise(
#     n_trp = n(),
#     .groups = "drop"
#   )

n_trp_per_year <-
  trp_index |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    period == "year_to_date"
  ) |>
  dplyr::group_by(
    year
  ) |>
  dplyr::slice_max(month) |>
  dplyr::summarise(
    n_trp = n(),
    .groups = "drop"
  )

city_trps <-
  get_published_bikepointindex_for_months(
    city_number,
    2022,
    index_month
  )[[1]] |>
  base::sort()


# AADT ----
adt <-
  get_aadt_for_trp_list(city_trps)

adt_latest <-
  adt |>
  dplyr::group_by(
    trp_id
  ) |>
  dplyr::slice_max(
    year
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    trp_id,
    adt
  )

adt_filtered <-
  adt |>
  dplyr::select(
    trp_id,
    year,
    adt
  ) |>
  dplyr::filter(
    year %in% all_years
  ) |>
  dplyr::arrange(
    year
  ) |>
  tidyr::pivot_wider(
    values_from = "adt",
    names_from = "year",
    names_prefix = "aadt_"
  ) |>
  dplyr::left_join(
    trp,
    by = "trp_id"
  ) |>
  dplyr::select(
    name,
    tidyselect::starts_with("aadt")
  ) |>
  dplyr::arrange(
    name
  )

readr::write_rds(
  adt_filtered,
  file =
    paste0(
      "tidy_data/aadt_",
      city_number,
      ".rds"
    )
)

trp |>
  dplyr::filter(
    trp_id %in% city_trps
  ) |>
  dplyr::left_join(
    adt_latest,
    by = "trp_id"
  ) |>
  readr::write_rds(
    file =
      paste0(
        "tidy_data/trp_",
        city_number,
        ".rds"
      )
  )


# All chain combinations ----
city_index_complete_years <-
  city_index |>
  dplyr::filter(
    period == "year_to_date",
    month == 12
  ) |>
  dplyr::arrange(
    year
  )

city_index_chain_combinations <-
  city_index_complete_years |>
  dplyr::select(
    year,
    index_i
  ) |>
  calculate_all_index_chain_combinations()


# TRP index chains ----
trp_index_complete_years <-
  trp_index |>
  dplyr::filter(
    period == "year_to_date",
    month == 12,
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    index_total_coverage >= 50
  ) |>
  dplyr::mutate(
    index_i = 1 + index_total_p / 100
  ) |>
  dplyr::select(
    trp_id,
    year,
    index_i
  ) |>
  dplyr::arrange(
    year
  )


calculate_chain_since_first_year <- function(df) {

  df |>
  calculate_all_index_chain_combinations() |>
    dplyr::select(
      1,
      chain = 3
    )

}


trp_chains <-
  trp_index_complete_years |>
  dplyr::group_by(trp_id) |>
  dplyr::group_modify(
    .f = ~ calculate_chain_since_first_year(.x),
    .keep = TRUE
  ) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = chain
  ) |>
  dplyr::select(
    trp_id,
    base::sort(tidyselect::peek_vars())
  )

trp_adt_index_chains <-
  trp |>
  dplyr::filter(
    trp_id %in% city_trps
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_category_and_number,
    municipality_name
  ) |>
  dplyr::left_join(
    adt_latest,
    by = "trp_id"
  ) |>
  dplyr::left_join(
    trp_chains,
    by = "trp_id"
  )




# City index ----
city_index_se <-
  city_index |>
  dplyr::group_by(
    year
  ) |>
  dplyr::filter(
    period == "year_to_date"
  ) %>%
  dplyr::slice_max(
    month
  ) |>
  dplyr::left_join(
    n_trp_per_year,
    by = "year"
  ) |>
  dplyr::mutate(
    year_base = year - 1,
    variance = standard_deviation^2,
    standard_error =
      base::round(standard_deviation / sqrt(n_trp), digits = 1)
  ) |>
  dplyr::select(
    year_base,
    year,
    month,
    index_p,
    index_i,
    standard_deviation,
    variance,
    n_trp,
    standard_error
  ) |>
  dplyr::arrange(year) |>
  dplyr::ungroup()

# How many index years to chain?
base::nrow(city_index_se)

years_1_2 <-
  calculate_two_year_index(city_index_se)

years_1_3 <-
  dplyr::bind_rows(
    years_1_2,
    dplyr::slice(city_index_se, 3)
  ) %>%
  calculate_two_year_index()

years_1_4 <-
  dplyr::bind_rows(
    years_1_3,
    dplyr::slice(city_index_se, 4)
  ) %>%
  calculate_two_year_index()

years_1_5 <-
  dplyr::bind_rows(
    years_1_4,
    dplyr::slice(city_index_se, 5)
  ) %>%
  calculate_two_year_index()

years_1_6 <-
  dplyr::bind_rows(
    years_1_5,
    dplyr::slice(city_index_se, 6)
  ) %>%
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last?
city_index_all <-
  city_index_se %>%
  dplyr::bind_rows(
    years_1_2,
    years_1_3,
    years_1_4,
    years_1_5,
    #years_1_6,
  ) %>%
  dplyr::mutate(
    year_from_to = paste0(year_base, "-", year),
    area_name = city_name,
    month_object = lubridate::make_date(year = year, month = month),
    month_name_short = lubridate::month(month_object, label = TRUE),
    period = paste0("jan-", month_name_short)
  )

readr::write_rds(
  city_index_all,
  file =
    paste0(
      "tidy_data/index_",
      city_number,
      ".rds"
    )
)
