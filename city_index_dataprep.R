# Preparation of data for reporting
# Gathering info on all points and indexes and writing them to csv

# Setup ----
# Packages are to be loaded through sourcing rmd_setup.R in the Rmd report file.
source("rmd_setup.R")
source("get_from_trafficdata_api.R")

# If necessary, get all TRPs from TRP API
# TRPs without commissions are not i TD-API!
#source("get_from_trp_api.R")

# NVDB API calls to get tolling stations or supply missing AADTs
#source("get_from_nvdb_api.R")

# Functions
# source TAKLER IKKE Ø som brukes i kolonneoverskrift i csv-ene! Må åpne fila og kjøre alt derfra.
source("indexpoints_tidying_functions.R")

library(viridis)
options(warn=-1)
svv_background_color <- "#F5F5F5"



# Points ----

## Connection to old data ----
# Points used in each city:
# (This is only used to match old index data from before 2020)
cities_points <-
  read.csv2("data_points_raw/cities_points.csv")

trp_id_msnr <-
  cities_points %>%
  dplyr::select(trp_id, msnr = legacyNortrafMpn) %>%
  dplyr::distinct()

# Shouldn't be necessary:
#cities_points_unestablished <-
#  read_csv2("data_points_raw/points_unestablished.csv")

## All points from Traffic Data API ----
points <- get_points() %>%
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
    name = stringr::str_to_title(name, locale = "no")
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

# All points from TRP API (if needed):
#points_trp <- get_points_from_trpapi_httr() %>%
#  split_road_system_reference() #%>%
# dplyr::select(trp_id, name, road_reference, road_category, road_number,
#               road_category_and_number,
#               section_number, subsection_number, meter,
#               intersection_part_number, intersection_meter,
#               county_name, municipality_name, lat, lon, road_link_position)



# City numbers ----
# Bergen 8952
# Buskerudbyen 1952
# Grenland 955
# Kristiansand og omegn 957 kommune 956
# Nedre Glomma 953
# Nord-Jæren 952
# Oslo 959
# Trondheim 960
# Tromsø 961

## Trondheim has its own script, all inclusive ----
#source("city_index_dataprep_trondheim_toll_stations")
# Trondheim stop

# Choose
index_month <- 8 # the one to be published now
city_number <- 1952

reference_year <-
  dplyr::case_when(
    city_number %in% c(
      953,
      955,
      956,
      957,
      961,
      1952
    ) ~ 2016,
    city_number %in% c(
      952
    ) ~ 2017,
    city_number %in% c(
      959,
      8952
    ) ~ 2018,
    city_number %in% c(
      960
    ) ~ 2019
  )

# Pointindices ----
# TODO: TRPs might differ from year to year!

# Fetch city indexes
# Note: not all cities use 2017
# Note: just needed for city_name for Trondheim
city_index_2017 <- get_published_index_for_months(city_number, 2017, 12)
city_index_2018 <- get_published_index_for_months(city_number, 2018, 12)
city_index_2019 <- get_published_index_for_months(city_number, 2019, 12)
city_index_2020 <- get_published_index_for_months(city_number, 2020, 12)
city_index_2021 <- get_published_index_for_months(city_number, 2021, 12)
city_index_2022 <- get_published_index_for_months(city_number, 2022, index_month)

## Old results on csv ----
# Still need to specify csv-files for years before 2020 to get the pointindex as they are not in API
# Note: not all cities use 2017
pointindex_17 <-
  read_pointindex_CSV(
    paste0("data_index_raw/pointindex_", city_number, "_", 2017, ".csv")
  ) %>%
  dplyr::rename(index_17 = index)

pointindex_17_monthly <-
  read_old_pointindex_csv_monthly(
    paste0("data_index_raw/pointindex_", city_number, "_", 2017, ".csv"),
    2017
  ) |>
  dplyr::left_join(
    trp_id_msnr,
    by = "msnr"
  ) |>
  dplyr::select(-msnr)

pointindex_18 <-
  read_pointindex_CSV(
    paste0("data_index_raw/pointindex_", city_number, "_", 2018, ".csv")
  ) %>%
  dplyr::rename(index_18 = index)

pointindex_18_monthly <-
  read_old_pointindex_csv_monthly(
    paste0("data_index_raw/pointindex_", city_number, "_", 2018, ".csv"),
    2018
  ) %>%
  dplyr::left_join(
    trp_id_msnr,
    by = "msnr"
  ) %>%
  dplyr::select(-msnr)

pointindex_19 <-
  read_pointindex_CSV(
    paste0("data_index_raw/pointindex_", city_number, "_", 2019, ".csv")
  ) %>%
  dplyr::rename(index_19 = index)

pointindex_19_monthly <-
  read_old_pointindex_csv_monthly(
    paste0("data_index_raw/pointindex_", city_number, "_", 2019, ".csv"),
    2019
  ) %>%
  dplyr::left_join(
    trp_id_msnr,
    by = "msnr"
  ) %>%
  dplyr::select(-msnr)


## New results from API ----
# Bergen
# Because Bergen had a new set of trps from 2019, it has just new results
pointindex_19_all <-
  get_published_pointindex_for_months(
    city_number,
    2019,
    12
  )

pointindex_19 <-
  pointindex_19_all[[2]] %>%
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "year_to_date",
    month == 12
  ) %>%
  dplyr::select(
    trp_id, #base_volume, calc_volume,
    index_19 = index_short
  )

# Bergen end

pointindex_20_all <-
  get_published_pointindex_for_months(city_number, 2020, 12)
pointindex_21_all <-
  get_published_pointindex_for_months(city_number, 2021, 12)
pointindex_22_all <-
  get_published_pointindex_for_months(city_number, 2022, index_month)

city_trps <-
  pointindex_22_all[[1]] |>
  base::sort()

city_name <- city_index_2022$area_name[1]

pointindex_20 <-
  pointindex_20_all[[2]] %>%
  dplyr::filter(day_type == "ALL",
                is_excluded == FALSE,
                is_manually_excluded == FALSE,
                length_excluded == FALSE,
                period == "year_to_date",
                month == 12) %>%
  dplyr::select(trp_id, index_20 = index_short)

pointindex_21 <-
  pointindex_21_all[[2]] %>%
  dplyr::filter(day_type == "ALL",
                is_excluded == FALSE,
                is_manually_excluded == FALSE,
                length_excluded == FALSE,
                period == "year_to_date",
                month == index_month) %>%
  dplyr::select(trp_id, index_21 = index_short)

pointindex_22 <-
  pointindex_22_all[[2]] %>%
  dplyr::filter(day_type == "ALL",
                is_excluded == FALSE,
                is_manually_excluded == FALSE,
                length_excluded == FALSE,
                period == "year_to_date",
                month == index_month) %>%
  dplyr::select(trp_id, index_22 = index_short)


n_17 <-
  pointindex_17 %>%
  dplyr::filter(!is.na(index_17)) %>%
  nrow()

n_18 <-
  pointindex_18 %>%
  dplyr::filter(!is.na(index_18)) %>%
  nrow()

n_19 <-
  pointindex_19 %>%
  dplyr::filter(!is.na(index_19)) %>%
  nrow()

n_20 <-
  pointindex_20  %>%
  dplyr::filter(!is.na(index_20)) %>%
  nrow()

n_21 <-
  pointindex_21  %>%
  dplyr::filter(!is.na(index_21)) %>%
  nrow()

n_22 <-
  pointindex_22  %>%
  dplyr::filter(!is.na(index_22)) %>%
  nrow()

# Number of points per month for SE in monthly city index
trp_index_monthly <-
  dplyr::bind_rows(
  # Pointindex from API here
    #pointindex_18_all[[2]],
    #pointindex_19_all[[2]],
    pointindex_20_all[[2]],
    pointindex_21_all[[2]],
    pointindex_22_all[[2]]
  ) %>%
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "month"
  ) %>%
  dplyr::select(
    trp_id,
    year,
    month,
    index = index_short
  ) %>%
  dplyr::bind_rows(
    # Pointindex from old csv files here:
    pointindex_17_monthly,
    pointindex_18_monthly,
    pointindex_19_monthly
  )


n_points_per_month <-
  trp_index_monthly %>%
  dplyr::group_by(
    year,
    month
  ) %>%
  dplyr::summarise(n_trp = n())

trp_index_monthly_wide <-
  trp_index_monthly |>
  tidyr::complete(
    trp_id,
    year,
    month
  ) |>
  dplyr::mutate(
    month_label = lubridate::make_date(
      year = 2000,
      month = month,
      day = 1
    ) |>
      lubridate::month(label = TRUE)
  ) |>
  dplyr::select(
    -month
  ) |>
  tidyr::pivot_wider(
    names_from = "month_label",
    values_from = "index"
  ) |>
  dplyr::left_join(
    points,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_category_and_number,
    year,
    jan:des
  ) |>
  dplyr::arrange(
    trp_id,
    year
  )


## AADT ----
adt <- get_aadt_by_length_for_trp_list(city_trps)

adt_filtered <-
  adt %>%
  dplyr::filter(
    length_range %in% c("[..,5.6)", "[5.6,..)")
    ) %>%
  dplyr::mutate(
    length_quality = round(aadt_valid_length / aadt_total * 100)
  ) %>%
  #dplyr::filter(
  #  length_quality > 90
  #) %>%
  dplyr::filter(
    coverage > 50
  ) %>%
  dplyr::mutate(
    length_range =
      dplyr::case_when(
        length_range == "[..,5.6)" ~ "lette",
        length_range == "[5.6,..)" ~ "tunge",
        TRUE ~ length_range
      )
  ) %>%
  dplyr::select(
    trp_id,
    year,
    length_range,
    aadt_length_range,
    coverage,
    aadt_total,
    sd_total,
    length_quality
  ) %>%
  tidyr::pivot_wider(
    names_from = "length_range",
    names_prefix = "aadt_",
    values_from = "aadt_length_range"
  ) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(
    trp_id,
    year,
    coverage,
    length_quality,
    aadt_total,
    aadt_lette,
    aadt_tunge
  )

### Bergen ----
adt_manual <- data.frame(
  trp_id = c("04939V804763", "58509V804762"),
  adt = c(100, 1700),
  year = c(2020, 2020)
)

### Nord-Jæren ----
adt_manual <- data.frame(
  #trp_id = c("68351V319882"),
  #adt = c(31500),
  #year = c(2017)
)

### Buskerudbyen ----
adt_manual <- data.frame(
  # trp_id = c("26634V181322", "06687V181318", "63545V180918"),
  # adt = c(2200, 26500, 6000),
  # year = c(2019, 2019, 2018)
)

### Oslo ----
# this_citys_trp_index_prel <- points %>%
#   dplyr::filter(trp_id %in% city_trps) %>%
#   split_road_system_reference() %>%
#   dplyr::select(trp_id, name, road_reference,
#                 road_category_and_number,
#                 county_name, municipality_name,
#                 lat, lon, road_link_position) %>%
#   dplyr::left_join(trp_id_msnr) %>%
#   left_join(adt_filtered) %>%
#   #left_join(pointindex_18) %>%
#   left_join(pointindex_19) %>%
#   left_join(pointindex_20) %>%
#   left_join(pointindex_21)
#
# missing_adt <- this_citys_trp_index_prel %>%
#   dplyr::filter(is.na(adt)) %>%
#   dplyr::mutate(adt = mapply(getAadtByRoadlinkposition, road_link_position))
#
# missing_adt_small_cars <- missing_adt %>%
#   dplyr::mutate(adt = round(0.9 * adt, digits = -2),
#                 year = 2019)
#
# this_citys_trp_index <- this_citys_trp_index_prel %>%
#   dplyr::filter(!is.na(adt)) %>%
#   dplyr::bind_rows(missing_adt_small_cars) %>%
#   split_road_system_reference()
# Oslo end, skip to refyear

### Grenland ----
adt_manual <- data.frame(
  trp_id = c("26489V521174"),
  adt = c(9600),
  year = c(2018)
)

### Tromsø ----
adt_manual <- data.frame()

### Kristiansand ----
adt_manual <- data.frame(
  trp_id = c("33412V121301"),
  aadt_lette = c(40000),
  year = c(2017)
)

### Nedre Glomma ----
adt_manual <- data.frame()

### All ----
adt_all <-
  bind_rows(
    adt_filtered,
    adt_manual
  )

## Final table ----
this_citys_trp_index <-
  points %>%
  dplyr::filter(trp_id %in% city_trps) %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    road_category_and_number,
    county_name, municipality_name,
    lat, lon, road_link_position
  ) %>%
  dplyr::left_join(trp_id_msnr) %>%
  dplyr::left_join(adt_all) %>%
  dplyr::left_join(pointindex_17) %>%
  dplyr::left_join(pointindex_18) %>%
  dplyr::left_join(pointindex_19) %>%
  dplyr::left_join(pointindex_20) %>%
  dplyr::left_join(pointindex_21) %>%
  dplyr::left_join(pointindex_22)

# Chained pointindex from reference year
trp_index_from_refyear <-
  this_citys_trp_index %>%
  dplyr::select(
    trp_id,
    tidyselect::starts_with("index")
  ) %>%
  dplyr::filter(
    dplyr::if_all(
      .cols = tidyselect::starts_with("index"),
      .fns = ~ !is.na(.x)
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::starts_with("index"),
      .fns = ~ index_converter(.))
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(index = prod(c_across(tidyselect::starts_with("index")))) %>%
  dplyr::mutate(index = round(100 * (index - 1), digits = 1)) %>%
  dplyr::select(trp_id, index)

this_citys_trp_index_refyear <-
  this_citys_trp_index %>%
  dplyr::left_join(trp_index_from_refyear)

# TODO: include coverage
# TODO: 3 year rolling index, but not now - only for the city

write.csv2(
  this_citys_trp_index_refyear,
  file = paste0("data_indexpoints_tidy/indekspunkt_", city_number, ".csv"),
  row.names = F
)


# City index ----
city_year_to_date_17 <-
  city_index_2017 %>%
  dplyr::filter(
    month == 12,
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    period == "year_to_date"
  )

city_year_to_date_18 <-
  city_index_2018 %>%
  dplyr::filter(
    month == 12,
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    period == "year_to_date"
  )

city_year_to_date_19 <-
  city_index_2019 %>%
  dplyr::filter(
    month == 12,
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    period == "year_to_date"
  )

city_year_to_date_20 <-
  city_index_2020 %>%
  dplyr::filter(
    month == 12,
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    period == "year_to_date"
  )

city_year_to_date_21 <-
  city_index_2021 %>%
  dplyr::filter(
    month == 12,
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    period == "year_to_date"
  )

city_year_to_date_22 <-
  city_index_2022 %>%
  dplyr::filter(
    month == index_month,
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    period == "year_to_date"
  )

city_index <-
  dplyr::bind_rows(
    city_year_to_date_17,
    city_year_to_date_18,
    city_year_to_date_19,
    city_year_to_date_20,
    city_year_to_date_21,
    city_year_to_date_22
  ) %>%
  dplyr::mutate(
    year_base = year - 1,
    index_i = index_converter(index_p),
    variance = standard_deviation^2,
    n_trp = c(
      n_17,
      n_18,
      n_19,
      n_20,
      n_21,
      n_22
    ),
   standard_error =
     base::round(standard_deviation / sqrt(n_trp), digits = 1)
  ) %>%
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
  ) %>%
  dplyr::arrange(year)


## Chained city index ----
# TODO: Functionize!
#calculate_each_index_from_reference_year <- function(city_index_df) {
  # In order to calculate SD and SE, do it iteratively for two years at a time
#  city_index_n_years <- nrow(city_index)
#  ref_year <- min(city_index$year_base)
#}

years_1_2 <- calculate_two_year_index(city_index)

years_1_3 <-
  bind_rows(years_1_2, slice(city_index, 3)) %>%
  calculate_two_year_index()

years_1_4 <-
  bind_rows(years_1_3, slice(city_index, 4)) %>%
   calculate_two_year_index()

years_1_5 <-
  bind_rows(years_1_4, slice(city_index, 5)) %>%
  calculate_two_year_index()

years_1_6 <-
  bind_rows(years_1_5, slice(city_index, 6)) %>%
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last?
city_index_all <-
  city_index %>%
  dplyr::bind_rows(years_1_2) %>%
  dplyr::bind_rows(years_1_3) %>%
  dplyr::bind_rows(years_1_4) %>%
  dplyr::bind_rows(years_1_5) %>%
  dplyr::bind_rows(years_1_6) %>%
  dplyr::mutate(
    year_from_to = paste0(year_base, "-", year),
    area_name = city_name
  )

write.csv2(
  city_index_all,
  file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".csv"),
  row.names = F
)



# City index monthly ----
city_monthly <-
  dplyr::bind_rows(
    monthly_city_index(city_index_2017),
    monthly_city_index(city_index_2018),
    monthly_city_index(city_index_2019),
    monthly_city_index(city_index_2020),
    monthly_city_index(city_index_2021),
    monthly_city_index(city_index_2022)
  ) %>%
  dplyr::select(
    area_name,
    year,
    month,
    period,
    month_object,
    month_name,
    index_p,
    standard_deviation,
    confidence_width,
    base_volume,
    calc_volume
  ) %>%
  dplyr::left_join(
    n_points_per_month,
    by = c("year", "month")
  ) %>%
  dplyr::mutate(
    standard_error = round(standard_deviation / sqrt(n_points), digits = 1)
  )

write.csv2(
  city_monthly,
  file =
    paste0(
      "data_indexpoints_tidy/byindeks_maanedlig_",
      city_number,
      ".csv"
    ),
  row.names = F
)


# City index three year rolling ----
# No use in calculating this before 37 months are available
# The first 36 month index is equal to the first three whole year index!
# TODO: 36 month rolling index with sd and ci

# all_possible_36_month_indexes <-
#   calculate_all_possible_36_month_indexes(city_monthly)
#
# readr::write_rds(
#   all_possible_36_month_indexes,
#   file =
#     paste0(
#       "data_indexpoints_tidy/byindeks_36_maaneder_",
#       city_number,
#       ".rds"
#     )
# )


# 36 month window using MDTs ----

## Get MDTs ----
present_year <-
  lubridate::today() |>
  lubridate::year()

years_from_reference_to_today <-
  base::seq(reference_year, present_year)

mdt <-
  purrr::map_dfr(
    years_from_reference_to_today,
    ~ get_mdt_by_length_for_trp_list(city_trps, .x)
)

# How many MDTs in reference year from NorTraf?
n_nortraf_mdt_reference_year <-
  mdt |>
  dplyr::filter(
    length_range == "[..,5.6)",
    year == reference_year
  ) |>
  dplyr::mutate(
    is_nortraf = dplyr::if_else(is.na(coverage), 1, 0)
  ) |>
  dplyr::group_by(
    trp_id
  ) |>
  dplyr::summarise(
    n_months_nortraf = sum(is_nortraf)
  )

n_valid_mdt_reference_year <-
  mdt_validated |>
  dplyr::filter(
    year == reference_year
  ) |>
  dplyr::mutate(
    valid_quality = coverage >= 50 & length_quality >= 99
  ) |>
  dplyr::filter(
    valid_quality == TRUE
  ) |>
  dplyr::group_by(
    trp_id
  ) |>
  dplyr::summarise(
    n_months_good_quality = n()
  )

this_citys_trp_nortraf_reference_year <-
  points |>
  dplyr::filter(trp_id %in% city_trps) |>
  split_road_system_reference() |>
  dplyr::select(
    trp_id,
    name,
    road_category_and_number
  ) |>
  dplyr::left_join(
    n_valid_mdt_reference_year,
    by = "trp_id"
  ) |>
  dplyr::left_join(
    n_nortraf_mdt_reference_year,
    by = "trp_id"
  ) |>
  dplyr::filter(
    n_months_good_quality >= 10,
    n_months_nortraf > 4
  )

write.csv2(
  this_citys_trp_nortraf_reference_year,
  file = "nortraf_buskerudbyen.csv",
  row.names = F,
  fileEncoding = "latin1"
)


mdt_filtered <-
  mdt |>
  dplyr::filter(
    length_range == "[..,5.6)"
  ) |>
  dplyr::mutate(
    mdt_valid_length = dplyr::case_when(
      is.na(sd_length_range) ~ mdt_total, # If NorTraf, assume high quality
      TRUE ~ mdt_valid_length
    ),
    length_quality = round(mdt_valid_length / mdt_total * 100),
    coverage = dplyr::case_when(
      is.na(sd_length_range) ~ 100, # If NorTraf, assume high quality
      TRUE ~ coverage
    )
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    mdt = mdt_length_range,
    coverage,
    length_quality
  ) |>
  dplyr::mutate(
    year_month = lubridate::as_date(
      paste0(
        year,
        "-",
        month,
        "-01"
      )
    )
  ) |>
  tibble::as_tibble()

readr::write_rds(
  mdt_filtered,
  file =
    paste0(
      "data_indexpoints_tidy/mdt_",
      city_number,
      ".rds"
    )
)

mdt_filtered <-
  readr::read_rds(
    paste0(
      "data_indexpoints_tidy/mdt_",
      city_number,
      ".rds"
    )
  )


## Check MDT validity
mdt_filtered |>
  dplyr::filter(
    trp_id %in% city_trps[10:12]
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    mdt,
    coverage,
    length_quality
  ) |>
  tidyr::complete(
    trp_id,
    year,
    month,
    fill = list(
      mdt = 0,
      coverage = 0,
      length_quality = 0
    )
  ) |>
  dplyr::mutate(
    month_object =
      lubridate::make_date(
        year = 2000,
        month = month,
        day = 1
      )
  ) |>
  dplyr::left_join(
    points,
    by = "trp_id"
  ) |>
  dplyr::mutate(
    road_category_and_number_and_point_name =
      paste0(
        road_category_and_number,
        " ",
        name
      )
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    mdt,
    coverage,
    length_quality,
    month_object,
    road_category_and_number_and_point_name
  ) |>
  dplyr::mutate(
    valid_quality = coverage >= 50 & length_quality >= 99
  ) |>
  create_mdt_barplot()

# Exclude trp-months
source("exclude_trp_mdts.R")


## TRP MDT table ----
# To show MDTs in a table in the report
mdt_each_year <-
  purrr::map_dfr(
    years_from_reference_to_today,
    ~ filter_mdt(mdt_filtered, .x)
  ) |>
  dplyr::select(
    -n_months
  ) |>
  tidyr::pivot_wider(
    names_from = year,
    names_prefix = "mdt_",
    values_from = mean_mdt
  )

city_trps_mdt <-
  points %>%
  dplyr::filter(trp_id %in% city_trps) %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    road_category_and_number
  ) %>%
  dplyr::left_join(
    mdt_each_year,
    by = "trp_id"
  ) |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::starts_with("mdt_"),
      ~ round(
        .x,
        digits = -1
        )
    )
  )

readr::write_rds(
  city_trps_mdt,
  file =
    paste0(
      "data_indexpoints_tidy/city_trps_mdt_",
      city_number,
      ".rds"
    )
)



## All possible 36 month window indices ----
first_possible_year_month <-
  lubridate::as_date(
    paste0(
      reference_year + 3,
      "-12-01"
    )
  )

last_year_month <-
  lubridate::as_date(
    paste0(
      present_year,
      "-",
      index_month,
      "-01"
    )
  )

year_months_possible <-
  base::seq.Date(
    from = first_possible_year_month,
    to = last_year_month,
    by = "month"
  )

all_36_month_indices <-
  purrr::map_dfr(
    year_months_possible,
    ~ calculate_rolling_indices_by_mdt(reference_year, .x, 36, mdt_validated)
  )


readr::write_rds(
  all_36_month_indices,
  file =
    paste0(
      "data_indexpoints_tidy/mdt_36_",
      city_number,
      ".rds"
    )
)


# E18 Buskerudbyen ----
trps_e18 <- c("08879V180819", "17291V181259")

point_index_e18 <-
  dplyr::bind_rows(
    get_pointindices_for_trp_list(trps_e18, 2017),
    get_pointindices_for_trp_list(trps_e18, 2018),
    get_pointindices_for_trp_list(trps_e18, 2019),
    get_pointindices_for_trp_list(trps_e18, 2020),
    get_pointindices_for_trp_list(trps_e18, 2021),
    get_pointindices_for_trp_list(trps_e18, 2022)
  ) %>%
  dplyr::filter(
    day_type == "ALL",
    period == "year_to_date"
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::filter(month == max(month))

trps_e18_index <-
  points %>%
  dplyr::filter(trp_id %in% trps_e18) %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    road_category_and_number,
    county_name, municipality_name,
    lat, lon, road_link_position
  ) %>%
  dplyr::left_join(point_index_e18)

write.csv2(
  trps_e18_index,
  file = "data_indexpoints_tidy/buskerudbyen_e18_punktindekser.csv",
  row.names = F
)
