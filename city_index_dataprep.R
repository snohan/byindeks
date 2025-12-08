# Preparation of data for reporting

# IN
# - TRP metadata (Traffic data API) made in city_index_check.Rmd
# - City index for all years (Traffic data API, local CSVs for older data)
# - TRP index for all years (Traffic data API, local CSVs for older data)
# - MDT (Traffic data API)

# OUT
# - RDS files:
#   - City index
#   - Chained yearly city index
#   - Rolling index based on MDT
# - Excel file with all data


# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
  source("indexpoints_tidying_functions.R")
  library(viridis)
  library(writexl)
  library(readxl)
  library(plotly)
  library(tictoc)
  options(warn = -1)
  svv_background_color <- "#F5F5F5"
}


# Old data connection ----
# (This is only used to match old index data from before 2020)
trp_id_msnr <-
  read.csv2("data_points_raw/cities_points.csv") |>
  dplyr::select(trp_id, msnr = legacyNortrafMpn) |>
  dplyr::distinct()


# Time ----
# City IDs
# 2016:
#   Buskerudbyen 1952
#   Grenland 955
# 2017:
#   Nord-Jæren 952
# 2018:
#   Bergen 8952
#   Oslo 959
# 2019:
#   Trondheim 960
# 2022:
#   Tromsø 2022 16952
# 2023:
#   Kristiansandsregionen 19953
#   Nedre Glomma 18952
# 2024:
#   Bodø 19954
#   Haugesund 19955
#   Ålesund 20952

{
  present_year <- 2025
  # month to be published now:
  index_month <- 11
  city_number <- 960
}

source("set_time_references.R")


# TRPs ----
this_citys_trps_all_adt_final <-
  readr::read_rds(
    file = paste0(
      "index_trp_metadata/trp_",
      city_number,
      ".rds"
    )
  )

trp_names <-
  this_citys_trps_all_adt_final |>
  dplyr::select(
    trp_id,
    name,
    municipality_name
  ) |>
  dplyr::arrange(name)

if(city_number == 959) {
  sub_areas <-
    this_citys_trps_all_adt_final |>
    dplyr::select(
      trp_id,
      sub_area = county_name
    ) |>
    # Some old TRPs no longer part of definition
    dplyr::bind_rows(
      tibble::tribble(
        ~trp_id, ~sub_area,
        "23568V444232", "Akershus",
        "30552V444220", "Akershus",
        "88537V444232", "Akershus",
        "69931V443604", "Akershus"
      )
    )
}

# TODO: TRPs might differ from year to year!
city_trps <-
  get_published_pointindex_for_months(city_number, max(index_years), 1)[[1]] |>
  base::sort()


# Rolling index ----
# Do this first in order to wait for yearly index to be published in API
## Get MDTs ----
{
  tictoc::tic()
  mdt <-
    purrr::map_dfr(
      years_from_reference_to_today,
      ~ get_mdt_by_length_for_trp_list(city_trps, .x)
    )
  tictoc::toc()
}

if(city_number == 960) {
  toll_mdt_light <-
    readr::read_rds(
      file = "data_indexpoints_tidy/trd_toll_mdt.rds",
    ) |>
    dplyr::filter(class == "lette") |>
    dplyr::rename(
      year_month = month
    ) |>
    dplyr::mutate(
      year = lubridate::year(year_month),
      month = lubridate::month(year_month),
      length_quality = 100
    ) |>
    dplyr::select(
      -class,
      -n_days,
      -traffic
    )
}

mdt_filtered <-
  mdt |>
  dplyr::filter(length_range == "[..,5.6)") |>
  dplyr::mutate(
    mdt_valid_length = dplyr::case_when(
      is.na(total_coverage) ~ mdt_total, # If NorTraf, assume high quality
      TRUE ~ mdt_valid_length
    ),
    length_quality = mdt_valid_length / mdt_total * 100,
    coverage = dplyr::case_when(
      is.na(total_coverage) ~ 100, # If NorTraf, assume high quality
      TRUE ~ total_coverage * length_quality / 100
    )
  ) |>
  dplyr::left_join(
    trp_names,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    year, month,
    mdt = mdt_length_range,
    coverage,
    length_quality,
    #sub_area = municipality_name
  ) |>
  dplyr::mutate(
    year_month = lubridate::as_date(paste0(year, "-", month, "-01"))
  ) |>
  tibble::as_tibble()

if(city_number == 960){
  mdt_filtered <-
    mdt_filtered |>
    dplyr::bind_rows(toll_mdt_light)
}

mdt_filtered |>
  readr::write_rds(
    file =
      paste0(
        "data_indexpoints_tidy/mdt_",
        city_number,
        ".rds"
      )
  )

# Read back in
#mdt_filtered <- readr::read_rds(paste0("data_indexpoints_tidy/mdt_", city_number, ".rds"))


## Check validity ----
# Exclude trp-months
source("exclude_trp_mdts_list.R")

trp_mdt_ok_refyear <-
  mdt_validated |>
  dplyr::filter(
    trp_id %in% city_trps # avoid toll stations appearing here as they've already been checked
  ) |>
  filter_mdt(reference_year) |>
  purrr::pluck(1)

length(trp_mdt_ok_refyear)

trp_not_ok <-
  trp_names |>
  dplyr::filter(
    !(trp_id %in% trp_mdt_ok_refyear)
  )

# TODO: define sections
# TODO: look at sectional TRPs concurrently
# TODO: in map, draw curve connecting sectional TRPs

# TODO: correlate exclusions of TRP index and MDT
# TODO: show TRP contributions to rolling indices
# TODO: Shiny app for checking MDT

plot_mdt(13)
source("exclude_trp_mdts_list.R")

#source("mdt_check.R")
#plot_mdt_comparisons |> plotly::ggplotly()


## Rolling index ----
all_12_month_indices <- calculate_rolling_indices(12)
all_24_month_indices <- calculate_rolling_indices(24)
all_36_month_indices <- calculate_rolling_indices(36)

compare_to_report <-
  all_36_month_indices |>
  dplyr::slice_max(month_n, by = year) |>
  dplyr::select(
    index_period,
    n_trp,
    index_p,
    ci_lower, ci_upper
  ) |>
  dplyr::mutate(index_p = round(index_p, 1))

all_rolling_indices_list <-
  list(
    all_12_month_indices,
    all_24_month_indices,
    all_36_month_indices
  )

all_rolling_indices <-
  dplyr::bind_rows(
    all_rolling_indices_list
  )

all_rolling_indices_list |>
  readr::write_rds(
    file =
      paste0(
        "data_indexpoints_tidy/rolling_indices_",
        city_number,
        ".rds"
      )
  )

# city_index_rolling_trp.R

if(city_number == 959) {
  source("city_index_rolling_sub_area.R")
}


# Yearly index ----
# TRD: Trondheim has its own script for yearly index: city_index_dataprep_trd.R

# TODO: one for whole years, and one for so-far-this-year


## Yearly TRP index ----

# TODO: generalize calculation of yearly index so that versions of so-far and whole-year can be done with same code
# NB! Will not work for indexes using data from before 2019 (except Bergen)

## So-far TRUE ----
# Makes no sense if the index_month is 1 or 12, but just include it in the workflow anyway (hopefully the results will be the same as for whole years)
so_far <- TRUE
source("city_index_trp_yearly.R")

# Sub area
if(city_number == 959) {
  source("city_index_yearly_sub_area.R")
}

# Whole area
source("city_index_yearly.R")


## So-far FALSE ----
so_far <- FALSE
source("city_index_trp_yearly.R")

# Sub area
if(city_number == 959) {
  source("city_index_yearly_sub_area.R")
}

# Whole area
source("city_index_yearly.R")

# TRP index by month for Excel
source("city_index_trp_index_by_month.R")


# E18 Buskerudbyen ----
if(city_number == 1952) {
  source("e18_buskerudbyen.R")
}


# Combine direct and rolling ----
## Nord-Jæren test ----
# chain_start_year_from_to <- "2017-2019"

# city_index_njaeren_2017_2019_official <-
#   city_info |>
#   dplyr::filter(year_from_to == chain_start_year_from_to)

# chain_link_index_i <- city_index_njaeren_2017_2019_official$index_i
# chain_link_se_p <- city_index_njaeren_2017_2019_official$standard_error

# all_36_month_indices_combined <-
#   all_36_month_indices |>
#   dplyr::select(
#     index_period,
#     month_object,
#     month_n,
#     year,
#     window,
#     n_trp,
#     index_i,
#     standard_error_p
#   ) |>
#   dplyr::mutate(
#     index_period =
#       paste0(
#         stringr::str_sub(chain_start_year_from_to, 1, 4),
#         stringr::str_sub(index_period, 5, -1)
#       ),
#     chained_index_i = index_i * chain_link_index_i,
#     index_p = (chained_index_i - 1) * 100,
#     standard_error =
#       100 * sqrt(
#         index_i^2 * 1e-4 * chain_link_se_p^2 +
#           chain_link_index_i^2 * 1e-4 * standard_error_p^2 +
#           1e-4 * chain_link_se_p^2 * 1e-4 * standard_error_p^2
#       ),
#     ci_lower = round(index_p - 1.96 * standard_error, 1),
#     ci_upper = round(index_p + 1.96 * standard_error, 1)
#   ) |>
#   dplyr::select(
#     index_period,
#     month_object,
#     month_n,
#     year,
#     window,
#     n_trp,
#     index_i = chained_index_i,
#     index_p,
#     standard_error,
#     ci_lower,
#     ci_upper
#   )

# readr::write_rds(
#   all_36_month_indices_combined,
#   file =
#     paste0(
#       "data_indexpoints_tidy/combined_indices_",
#       city_number,
#       ".rds"
#     )
# )


## Tromsø ----
chain_start_year_from_to <- "2019-2022"

city_index_tromso_2019_2022 <-
  readr::read_rds(
    "data_indexpoints_tidy/city_index_tromso_2019_2022.rds"
  ) |>
  dplyr::mutate(
    index_i = index_converter(index_p),
    period_build = "direct",
    months = "jan-des",
    year_base = 2019,
    year = 2022,
    month = 12
  )

chain_link_se_p <- city_index_tromso_2019_2022$standard_error


### Yearly index
city_index_chained <-
  city_index_yearly_all |>
  dplyr::filter(
    month == 12,
    year_base == 2022
  ) |>
  dplyr::select(
    index_period = year_from_to,
    year_base,
    year,
    month,
    n_trp,
    index_i,
    standard_error_p = standard_error
  ) |>
  dplyr::mutate(
    year_base = stringr::str_sub(chain_start_year_from_to, 1, 4) |> as.numeric(),
    year_from_to =
      paste0(
        stringr::str_sub(chain_start_year_from_to, 1, 4),
        stringr::str_sub(index_period, 5, -1)
      ),
    chained_index_i = index_i * city_index_tromso_2019_2022$index_i,
    index_p = (chained_index_i - 1) * 100,
    standard_error =
      100 * sqrt(
        index_i^2 * 1e-4 * chain_link_se_p^2 +
          city_index_tromso_2019_2022$index_i^2 * 1e-4 * standard_error_p^2 +
          1e-4 * chain_link_se_p^2 * 1e-4 * standard_error_p^2
      ),
    ci_lower = round(index_p - 1.96 * standard_error, 1),
    ci_upper = round(index_p + 1.96 * standard_error, 1),
    index_type = "chained",
    period = "jan-des"
  ) |>
  dplyr::select(
    year_base,
    year,
    month,
    year_from_to,
    index_type,
    period,
    index_i = chained_index_i,
    index_p,
    standard_error,
    ci_lower,
    ci_upper
  )

city_index_final <-
  dplyr::bind_rows(
    city_index_tromso_2019_2022 |>
      dplyr::select(
        year_base,
        year,
        month,
        year_from_to = period,
        index_type = period_build,
        period = months,
        n_trp,
        index_i,
        index_p,
        standard_error,
        ci_lower,
        ci_upper
      ),
    city_index_yearly_all |>
      dplyr::select(
        year_base,
        year,
        month,
        year_from_to,
        index_type,
        period,
        n_trp,
        index_i,
        index_p,
        standard_error,
        ci_lower,
        ci_upper
      ),
    city_index_chained
  ) |>
  dplyr::mutate(
    area_name = city_name
  )

readr::write_rds(
  city_index_final,
  file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".rds")
)


### Rolling index
all_rolling_indexes <-
  readr::read_rds(
    file =
      paste0(
        "data_indexpoints_tidy/rolling_indices_",
        city_number,
        ".rds"
      )
  ) |>
  purrr::list_rbind()

all_rolling_indexes_chained <-
  all_rolling_indexes |>
  dplyr::mutate(
    index_period =
      paste0(
        stringr::str_sub(chain_start_year_from_to, 1, 4),
        stringr::str_sub(index_period, 5, -1)
      ),
    chained_index_i = index_i * city_index_tromso_2019_2022$index_i,
    index_p = (chained_index_i - 1) * 100,
    standard_error_p =
      100 * sqrt(
        index_i^2 * 1e-4 * chain_link_se_p^2 +
          city_index_tromso_2019_2022$index_i^2 * 1e-4 * standard_error_p^2 +
          1e-4 * chain_link_se_p^2 * 1e-4 * standard_error_p^2
      ),
    ci_lower = round(index_p - 1.96 * standard_error_p, 1),
    ci_upper = round(index_p + 1.96 * standard_error_p, 1)#,
    #index_type = "chained",
    #period = "jan-des"
  ) |>
  dplyr::rename(
    index_i_org = index_i,
    index_i = chained_index_i
  ) |>
  dplyr::select(
    -index_i_org
  ) |>
  dplyr::relocate(
    index_i
  )

all_rolling_indexes_chained |>
  readr::write_rds(
    file =
      paste0(
        "data_indexpoints_tidy/rolling_indices_",
        city_number,
        ".rds"
      )
  )


# YDT ----
# For Excel
# Not for TRD!
{
ydt <- get_aadt_by_length_for_trp_list(this_citys_trps_all_adt_final$trp_id, "WEEKDAY")

ydt_reference_year <-
  if(city_number == 16952) {
    2019
  }else{
    reference_year
  }

ydt_ref_year <-
  ydt |>
  dplyr::filter(
    length_range %in% c("[..,5.6)", "[5.6,..)"),
    year == ydt_reference_year
  ) |>
  dplyr::select(trp_id, length_range, ydt_ref = aadt_length_range) |>
  dplyr::mutate(
    ydt_ref = round(ydt_ref, -1),
    length_range =
      dplyr::case_when(
        length_range == "[..,5.6)" ~ "light",
        length_range == "[5.6,..)" ~ "heavy"
      )
  ) |>
  tidyr::pivot_wider(
    names_from = length_range,
    names_prefix = "ydt_ref_",
    values_from = ydt_ref
  )

ydt_filtered <-
  ydt |>
  dplyr::filter(
    length_range %in% c("[..,5.6)", "[5.6,..)"),
  ) |>
  #dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  #dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) |>
  dplyr::group_by(trp_id) |>
  dplyr::filter(year == max(year)) |>
  # Assuming this to be the same year as for AADT
  dplyr::select(trp_id, length_range, ydt = aadt_length_range) |>
  dplyr::mutate(
    ydt = round(ydt, -1),
    length_range =
      dplyr::case_when(
        length_range == "[..,5.6)" ~ "light",
        length_range == "[5.6,..)" ~ "heavy"
      )
  ) |>
  tidyr::pivot_wider(
    names_from = length_range,
    names_prefix = "ydt_",
    values_from = ydt
  )

trp_info_adt <-
  this_citys_trps_all_adt_final |>
  dplyr::left_join(
    ydt_ref_year,
    by = join_by(trp_id)
  ) |>
  dplyr::left_join(
    ydt_filtered,
    by = join_by(trp_id)
  )

}


# Write Excel ----
source("city_index_to_excel.R")
