# Calculate city index using new method

# Introduction ----
# New concepts:
# - Using calendar adjusted MDT
# - Traffic work weights (from traffic links)
# - Chaining when necessary (need to suitably subdivide index period, possibly one road net version per subperiod)
# - Estimate confidence interval (compare and decide which method to use)
# - Measures of representativity

# To be included later:
# - Vehicle classification by type, not length

# Resolution in time:
# - Month by month
# - So far this year? Unnecessary?
# - Last 12 months
# - Last 24 months
# - Last 36 months

# Resolution in day type
# - working days
# - non-working days
# - all days

# Resolution in vehicle type:
# - light (short)
# - heavy (long)
# - all

# How to compare:
# - hard to make direct comparisons and attribute differences to specific parts of new method
# - new versus old index results in different time resolutions, separated from new chaining strategies
# - the impact of new chaining strategies leading to better representativity

# What are isolated improvements?
# - measures of representativity, especially traffic work
# - smaller (?) confidence interval, mostly (?) due to finiteness of population
# - traffic work weights


# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  svv_background_color <- "#F5F5F5"

  library(tidyverse)
  #library(boot)

  source("split_road_system_reference.R")
  source("indexpoints_tidying_functions.R")
  source("index_report_functions.R")
  source("get_from_trafficdata_api.R")
  source("traffic_link_functions.R")

  link_id_weights_2024 <- readr::read_rds("traffic_link_pop/link_id_weights_2024.rds")
  link_trp_id <- readr::read_rds("traffic_link_pop/link_trp_id.rds")
  points <- readr::read_rds("trps_for_city_index.rds")
}


# Bergen ----
{
  city_number <- "8952"
  present_year <- 2024
  index_month <- 12
  source("set_time_references.R")
}

## TRPs ----
this_citys_trps_all_adt_final <-
  readr::read_rds(
    file = paste0(
      "index_trp_metadata/trp_",
      city_number,
      ".rds"
    )
  ) |>
  dplyr::filter(
    stringr::str_sub(road_category_and_number, 1, 1) != "K"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    municipality_name,
    #lat, lon,
    adt, year_aadt, adt_ref
  )


## Link population ----
# Made in script city_link_population.R
links_bergen <-
  readr::read_rds(
    "traffic_link_pop/links_bergen.rds"
  )
population_size <- nrow(links_bergen)

function_class_tw <-
  links_bergen |>
  sf::st_drop_geometry() |>
  dplyr::select(
    tw_km = tw,
    function_class
  ) |>
  dplyr::summarise(
    tw_fcl_population_kkm = base::sum(tw_km) / 1000,
    n_links = n(),
    .by = function_class
  ) |>
  dplyr::arrange(function_class)

trp_weights <-
  links_bergen |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    !is.na(point_id)
  ) |>
  dplyr::select(
    trp_id = point_id,
    length_m,
    function_class
  ) |>
  dplyr::mutate(
    length_m = base::round(length_m)
  ) |>
  dplyr::left_join(
    function_class_tw,
    by = "function_class"
  )


# missing <-
#   this_citys_trps_all_adt_final |>
#   dplyr::filter(
#     !(trp_id %in% trp_weights$trp_id)
#   )
# Some are outside urban area, some are missing from links


## MDT ----
mdt_filtered <-
  readr::read_rds(
    paste0(
      "data_indexpoints_tidy/cmdt_",
      city_number,
      ".rds"
    )
  ) |>
  dplyr::filter(
    length_class == "korte"
  )


# To get the mdt_validated df
source("exclude_cmdt.R")


## Index calculation ----
# In production we would calculate monthly index, so-far-this-year index. But here, we only do rolling index.


# Steps in adding uncertainty estimation
# 1
# TODO: add standard error in CMDT, based on missing days
# For now, assume no uncerainty here, and it probably is much smaller than the contribution from spatial TRP sampling.

# 2
# TODO: adding up uncertainty in CMDT in this weighted yearly mean CMDT
# For now, assume no uncertainty here, and it probably is much smaller than the contribution from spatial TRP sampling.
brg_index_month <-
  mdt_validated |>
  calculate_area_index_month(population_size)

area_index_one_year_brg <- calculate_rolling_area_index_one_year(brg_index_month)

area_index_three_years_brg <- calculate_rolling_index_multiple_years(area_index_one_year_brg, 3)

# Sidetrack: for showing some data in presentation
viz_mdt <- mdt_validated |> select(trp_id, year, month, mdt, length_m, function_class)
viz_month <- brg_index_month |> select(x_label, index_p, n_trp)
viz_one_y <- area_index_one_year_brg |> select(x_label, index_p, ci_lower, ci_upper) |> mutate(across(where(is.double), ~ round(.x, 1)))
viz_three_y <- area_index_three_years_brg |> mutate(across(where(is.double), ~ round(.x, 1)))

# Back on track
readr::write_rds(
  brg_index_month,
  "representativity/cmdt_index_month_brg.rds"
)

list(
  area_index_one_year_brg |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(
      window_years = "one"
    ),
  area_index_three_years_brg |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(
      window_years = "three"
    )
) |>
  readr::write_rds(
    "representativity/rolling_cmdt_index_brg.rds"
  )





# trp_window_index <- rolling_index_trp(mdt_validated)
#
# trp_window_index_wide <-
#   trp_window_index |>
#   dplyr::mutate(
#     index_p = round(index_p, 1)
#   ) |>
#   tidyr::pivot_wider(
#     id_cols = trp_id,
#     names_from = universal_year_period_id_end,
#     names_prefix = "u_",
#     values_from = index_p
#   )
#
# # 3
# area_index_one_year <- rolling_index_area(trp_window_index)
#
# # 4
# #area_index_two_years <- rolling_index_multiple_years(area_index_one_year, 2)
# area_index_three_years <- rolling_index_multiple_years(area_index_one_year, 3)
#
# list(
#   area_index_one_year |>
#     dplyr::select(
#       universal_year_period_id,
#       x_label,
#       index_p,
#       ci_lower,
#       ci_upper,
#       n_trp
#     ) |>
#     dplyr::mutate(
#       window_years = "one"
#     ),
#   area_index_three_years |>
#     dplyr::select(
#       universal_year_period_id,
#       x_label,
#       index_p,
#       ci_lower,
#       ci_upper
#     ) |>
#     dplyr::mutate(
#       window_years = "three"
#     )
# ) |>
# readr::write_rds(
#   "representativity/rolling_cmdt_index_bergen.rds"
# )


# The offical results
# all_rolling_indices_official <-
#   readr::read_rds(
#     file =
#       paste0(
#         "data_indexpoints_tidy/rolling_indices_",
#         city_number,
#         ".rds"
#       )
#   ) |>
#   dplyr::bind_rows()

# list(
#   all_rolling_indices_old,
#   all_rolling_indices_new
# ) |>
# readr::write_rds(
#   "representativity/new_index_examples_bergen.rds"
# )

# compare_indexes <-
#   dplyr::inner_join(
#     prepare_rolling_indexes_for_comparison(all_rolling_indices_official),
#     prepare_rolling_indexes_for_comparison(all_rolling_indices_old),
#     by = dplyr::join_by(index_period, window),
#     suffix = c("_official", "_old")
#   ) |>
#   dplyr::relocate(
#     index_period,
#     window
#   )


# Trondheim ----
city_number <- "960"
present_year <- 2025
index_month <- 4
source("set_time_references.R")


## TRPs ----
this_citys_trps_all_adt_final <-
  readr::read_rds(
    file = paste0(
      "index_trp_metadata/trp_",
      city_number,
      ".rds"
    )
  ) |>
  dplyr::filter(
    stringr::str_sub(road_category_and_number, 1, 1) != "K"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    municipality_name,
    #lat, lon,
    adt, year_aadt, adt_ref
  )


## Link population ----
# Made in script city_link_population.R
links_trondheim <-
  readr::read_rds(
    "traffic_link_pop/links_trondheim.rds"
  )

population_size <- nrow(links_trondheim)

trp_weights <-
  links_trondheim |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    !is.na(point_id)
  ) |>
  dplyr::select(
    trp_id = point_id,
    tw,
    length_m
  ) |>
  dplyr::mutate(
    tw = base::round(tw / 1000),
    length_m = base::round(length_m)
  )

missing <-
  this_citys_trps_all_adt_final |>
  dplyr::filter(
    !(trp_id %in% trp_weights$trp_id)
  )
# Some are outside urban area, some are missing from links


## MDT ----
mdt_filtered <-
  readr::read_rds(
    paste0(
      "data_indexpoints_tidy/mdt_",
      city_number,
      ".rds"
    )
  )

# To get the mdt_validated
{
  source("exclude_trp_mdts_list.R")

  mdt_validated <-
    mdt_validated |>
    dplyr::inner_join(
      # "inner" works as a filter here!
      trp_weights,
      by = dplyr::join_by(trp_id)
    )
}

trp_mdt_ok_refyear <-
  mdt_validated |>
  dplyr::filter(
    trp_id %in% links_trondheim$point_id
  ) |>
  filter_mdt(reference_year) |>
  purrr::pluck(1)

mdt_yearly <-
  mdt_validated |>
  dplyr::filter(
    trp_id %in% trp_mdt_ok_refyear,
    coverage >= 50,
    length_quality >= 98.5
  ) |>
  dplyr::group_by(
    trp_id,
    year
  ) |>
  dplyr::summarise(
    n_months = n(),
    mean_mdt = base::mean(mdt) |> base::floor(),
    .groups = "drop"
  ) |>
  dplyr::filter(
    n_months >= 9
  ) |>
  dplyr::inner_join(
    trp_weights,
    by = dplyr::join_by(trp_id)
  )

n_trp_per_year <-
  mdt_yearly |>
  dplyr::summarise(
    n_trp = n(),
    .by = year
  ) |>
  dplyr::arrange(
    year
  )

## Index calculation ----
all_rolling_indices_old <- calculate_all_rolling_indices_old()

{
  tictoc::tic()
  all_rolling_indices_new <- calculate_all_rolling_indices_tw(population_size)
  tictoc::toc()
}

list(
  all_rolling_indices_old,
  all_rolling_indices_new
) |>
  readr::write_rds(
    "representativity/new_index_examples_trondheim.rds"
  )


# Nord-Jæren ----
{
  city_number <- "952"
  present_year <- 2024
  index_month <- 12
  source("set_time_references.R")
}


## TRPs ----
this_citys_trps_all_adt_final <-
  readr::read_rds(
    file = paste0(
      "index_trp_metadata/trp_",
      city_number,
      ".rds"
    )
  ) |>
  dplyr::filter(
    stringr::str_sub(road_category_and_number, 1, 1) != "K"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    municipality_name,
    #lat, lon,
    adt, year_aadt, adt_ref
  )


## Link population ----
# Made in script city_link_population.R
links_nj <- readr::read_rds("traffic_link_pop/links_nj.rds")
population_size <- nrow(links_nj)


## Original TRPs ----
# trp_weights <-
#   links_nj |>
#   sf::st_drop_geometry() |>
#   dplyr::filter(
#     !is.na(point_id)
#   ) |>
#   dplyr::select(
#     trp_id = point_id,
#     tw,
#     length_m
#   ) |>
#   dplyr::mutate(
#     tw = base::round(tw / 1000),
#     length_m = base::round(length_m)
#   )

# missing <-
#   this_citys_trps_all_adt_final |>
#   dplyr::filter(
#     !(trp_id %in% trp_weights$trp_id)
#   )
# Rege is outside urban area


### MDT ----
# mdt_filtered_o <-
#   readr::read_rds(
#     paste0(
#       "data_indexpoints_tidy/mdt_",
#       city_number,
#       ".rds"
#     )
#   )

# To get the mdt_validated
# source("exclude_trp_mdts_list.R")
#
# mdt_validated <-
#   mdt_validated |>
#   dplyr::inner_join(
#     # "inner" works as a filter here!
#     trp_weights,
#     by = dplyr::join_by(trp_id)
#   ) |>
#   dplyr::summarise(
#     n_trp = n(),
#     .by = "year_month"
#   )


# trp_mdt_ok_refyear <-
#   mdt_validated |>
#   dplyr::filter(
#     trp_id %in% links_nj$point_id
#   ) |>
#   filter_mdt(reference_year) |>
#   purrr::pluck(1)


### Index calculation ----
#all_rolling_indices_old <- calculate_all_rolling_indices_old()

# {
#   tictoc::tic()
#   all_rolling_indices_new <- calculate_all_rolling_indices_tw(population_size)
#   tictoc::toc()
# }
#
# list(
#   all_rolling_indices_old,
#   all_rolling_indices_new
# ) |>
#   readr::write_rds(
#     "representativity/new_index_examples_nj.rds"
#   )


## More TRPS ----
# Using as many TRPs as possible
# 1. Existing TRPs
# 2. All MDTs
# 3. Filter MDTs by year
# 4. Decide chain intervals

# trps_existing <-
#   link_trp_id |>
#   dplyr::filter(
#     link_id %in% links_nj$link_id
#   ) |>
#   dplyr::left_join(
#     points,
#     by = dplyr::join_by(trp_id)
#   )
#
# latest_adt <-
#   get_aadt_for_trp_list(trps_existing$trp_id) |>
#   dplyr::slice_max(
#     order_by = year,
#     by = trp_id
#   )

# Look at map
# trps_existing_2 <-
#   trps_existing |>
#   dplyr::left_join(
#     latest_adt,
#     by = dplyr::join_by(trp_id)
#   ) |>
#   dplyr::mutate(
#     label_text = paste0(trp_id, "<br/>", name) |> purrr::map(~ htmltools::HTML(.x))
#   ) |>
#   dplyr::select(
#     trp_id,
#     name,
#     road_reference,
#     lat, lon,
#     adt,
#     label_text
#   )
#
# trps_existing_2 |> create_point_adt_map_with_labels()

### MDT ----
# {
#   tictoc::tic()
#   mdt <-
#     purrr::map_dfr(
#       years_from_reference_to_today,
#       ~ get_mdt_by_length_for_trp_list(trps_existing$trp_id, .x)
#     )
#   tictoc::toc()
# }
#
# trp_weights <-
#   links_nj |>
#   sf::st_drop_geometry() |>
#   dplyr::filter(
#     !is.na(point_id)
#   ) |>
#   dplyr::select(
#     trp_id = point_id,
#     tw,
#     length_m
#   ) |>
#   dplyr::mutate(
#     tw = base::round(tw / 1000),
#     length_m = base::round(length_m)
#   )

# mdt_filtered <-
#   mdt |>
#   dplyr::filter(
#     length_range == "[..,5.6)"
#   ) |>
#   dplyr::mutate(
#     mdt_valid_length = dplyr::case_when(
#       is.na(total_coverage) ~ mdt_total, # If NorTraf, assume high quality
#       TRUE ~ mdt_valid_length
#     ),
#     length_quality = mdt_valid_length / mdt_total * 100,
#     coverage = dplyr::case_when(
#       is.na(total_coverage) ~ 100, # If NorTraf, assume high quality
#       TRUE ~ total_coverage * length_quality / 100
#     )
#   ) |>
#   dplyr::select(
#     trp_id,
#     year,
#     month,
#     mdt = mdt_length_range,
#     coverage,
#     length_quality
#   ) |>
#   dplyr::mutate(
#     year_month = lubridate::as_date(
#       paste0(
#         year,
#         "-",
#         month,
#         "-01"
#       )
#     )
#   ) |>
#   dplyr::left_join(
#     link_id_weights_2024,
#     by = dplyr::join_by(trp_id)
#   ) |>
#   tibble::as_tibble()


### Direct index ----
# May use the same exclusions as the official index
# TODO: Possibly with a few additions
#source("exclude_trp_mdts_list.R")

# mdt_validated <-
#   mdt_validated |>
#   dplyr::filter(
#     !(year_month >= "2020-01-01" &
#       trp_id %in%  c(
#         "17949V320695", # Bybrua sør
#         "10795V320297", # Randabergveien
#         "58562V320296", # Tanke Svilandsgate
#         "08952V320223", # Bjergsted
#         "68351V319882", # Kannik
#         "57279V320244", # Storhaugtunnelen
#         "54577V319746", # Hillevågstunnelen
#         "55507V319881", # Madlaveien Mosvatnet
#         "71535V319524", # Lassa
#         "83652V319725", # Strandgata nord
#         "92102V319885", # Bergelandstunnelen
#         "50749V319525", # Byhaugtunnelen sør
#         "86207V319742", # Lagårdsveien
#         "32842V319521", # Mosheim
#         "10028V320295"  # Løkkeveien
#       )
#     )
#   )

# Direct index with more TRPs
#all_rolling_indices_old <- calculate_all_rolling_indices_old()

# {
#   tictoc::tic()
#   all_rolling_indices_new <- calculate_all_rolling_indices_tw(population_size)
#   tictoc::toc()
# }
#
# list(
#   all_rolling_indices_old,
#   all_rolling_indices_new
# ) |>
#   readr::write_rds(
#     "representativity/new_index_examples_2_nj.rds"
#   )


### Chained ----
# Must have tailored exclusions to accomodate a maximum utilisation of TRPs in each chain period
# mdt_2017_2019 <-
#   #mdt_filtered |>
#   mdt_validated |>
#   dplyr::filter(
#     !(trp_id %in% c(
#       "73355V319671", # Austråttunnelen, er komplementær med Hana ved Rovik som følge av ny bom?
#       "83652V319725", # Strandgata nord, mye som har foregått her...
#       "43296V319721"  # Åsedalen, ny kobling til E39 oktober 2018.
#       #"59675V319722"  # Brualand, avvikende verdi, neppe riktig, men finner ingen åpenbar grunn.
#     )),
#     !(trp_id == "89457V2303027" & year_month == "2017-01-01"),
#     !(trp_id == "71798V319583" & year_month %in% c("2017-01-01", "2017-02-01", "2017-03-01"))
#   )
#
# index_2017_2019 <-
#   calculate_rolling_indices_tw(
#     reference_year,
#     "2019-12-01",
#     12,
#     mdt_2017_2019,
#     population_size,
#     "by_area"
#   )

# trp_index_2017_2019 <-
#   calculate_rolling_indices_tw(
#     reference_year,
#     "2019-12-01",
#     12,
#     mdt_2017_2019,
#     population_size,
#     "by_trp"
#   ) |>
#   dplyr::left_join(
#     points,
#     by = dplyr::join_by(trp_id)
#   ) |>
#   dplyr::select(
#     trp_id,
#     name,
#     road_category_and_number,
#     index_period,
#     length_km,
#     tidyselect::starts_with("mean_mdt"),
#     w_tw, w_tv,
#     trp_index_p
#   ) |>
#   dplyr::arrange(
#     trp_index_p
#   )

# mdt_2019_2023 <-
#   mdt_filtered |>
#   dplyr::filter(
#     !(trp_id %in% c(
#       # Åpning av Eiganestunnelen og Ryfylketunnelen:
#       "17949V320695", # Bybrua sør
#       "10795V320297", # Randabergveien
#       "58562V320296", # Tanke Svilandsgate
#       "08952V320223", # Bjergsted
#       "68351V319882", # Kannik
#       "57279V320244", # Storhaugtunnelen
#       "54577V319746", # Hillevågstunnelen
#       "55507V319881", # Madlaveien Mosvatnet
#       "71535V319524", # Lassa
#       "83652V319725", # Strandgata nord
#       "92102V319885", # Bergelandstunnelen
#       "50749V319525", # Byhaugtunnelen sør
#       "86207V319742", # Lagårdsveien
#       "32842V319521", # Mosheim
#       "10028V320295", # Løkkeveien
#       #
#       "59675V319722"  # Brualand: Avvikende verdi, ukjent årsak
#     ))
#   )

# index_2019_2023 <-
#   calculate_rolling_indices_tw(
#     2019,
#     "2023-12-01",
#     12,
#     mdt_2019_2023,
#     population_size,
#     "by_area"
#   )
#
# trp_index_2019_2023 <-
#   calculate_rolling_indices_tw(
#     2019,
#     "2023-12-01",
#     12,
#     mdt_2019_2023,
#     population_size,
#     "by_trp"
#   ) |>
#   dplyr::left_join(
#     points,
#     by = dplyr::join_by(trp_id)
#   ) |>
#   dplyr::select(
#     trp_id,
#     name,
#     road_category_and_number,
#     index_period,
#     length_km,
#     tidyselect::starts_with("mean_mdt"),
#     w_tw, w_tv,
#     trp_index_p
#   ) |>
#   dplyr::arrange(
#     trp_index_p
#   )

# mdt_2023_2024 <-
#   mdt_filtered |>
#   dplyr::filter(
#     !(trp_id %in% c(
#       "88125V320152", # Austrått
#       "89794V320138"  # Hoveveien, negativ korrelasjon mellom denne og Austrått, vegarbeid i nærheten?
#     )),
#     # Vegarbeid i Tanke Svilandsgate
#     !(trp_id == "58562V320296" &
#         year_month %in% base::seq(lubridate::ymd("2024-07-01"), lubridate::ymd("2025-06-01"), by = "month"))
#   )
#
# index_2023_2024 <-
#   calculate_rolling_indices_tw(
#     2023,
#     "2024-12-01",
#     12,
#     mdt_2023_2024,
#     population_size,
#     "by_area"
#   )

# trp_index_2023_2024 <-
#   calculate_rolling_indices_tw(
#     2023,
#     "2024-12-01",
#     12,
#     mdt_2023_2024,
#     population_size,
#     "by_trp"
#   ) |>
#   dplyr::left_join(
#     points,
#     by = dplyr::join_by(trp_id)
#   ) |>
#   dplyr::select(
#     trp_id,
#     name,
#     road_category_and_number,
#     index_period,
#     length_km,
#     tidyselect::starts_with("mean_mdt"),
#     w_tw, w_tv,
#     trp_index_p
#   ) |>
#   dplyr::arrange(
#     trp_index_p
#   )
#
# index_2017_2024_chained <-
#   dplyr::bind_rows(
#     index_2017_2019,
#     index_2019_2023,
#     index_2023_2024
#   )
#
# readr::write_rds(
#   index_2017_2024_chained,
#   "representativity/new_index_chain_nj.rds"
# )


# chained <- index_2017_2019$index_i * index_2019_2023$index_i * index_2023_2024$index_i
#
# chain_1 <-
#   index_2017_2024_chained |>
#   dplyr::rename(
#     standard_error = se_model_p,
#     month = month_n
#   ) |>
#   dplyr::mutate(
#     year_base = stringr::str_sub(index_period, 1, 4)
#   ) |>
#   calculate_two_year_index()
#
# chain_2 <-
#   dplyr::bind_rows(
#     chain_1,
#     slice(index_2017_2024_chained, 3) |>
#       dplyr::rename(
#         standard_error = se_model_p,
#         month = month_n
#       ) |>
#       dplyr::mutate(
#         year_base = stringr::str_sub(index_period, 1, 4)
#       )
#     ) |>
#   calculate_two_year_index()
#
# index_chained <-
#   chain_2 |>
#   dplyr::mutate(
#     year_from_to = paste0(year_base, "-", year),
#     #area_name = city_name,
#     month_name_short = lubridate::month(month, label = TRUE),
#     period = paste0("jan-", month_name_short),
#     index_p = round(index_p, 1),
#     ci_lower = round(index_p - 1.96 * standard_error, 1),
#     ci_upper = round(index_p + 1.96 * standard_error, 1),
#     version = "new_chained"
#   ) |>
#   dplyr::select(
#     -year_base
#   )

# Compare this to original chained index and 12 month index
# index_chained_original <-
#   readr::read_rds(
#     file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".rds")
#   ) |>
#   dplyr::filter(
#     index_type == "chained",
#     year == 2024
#   ) |>
#   dplyr::mutate(
#     version = "original_chained"
#   )
#
# index_12_month_original <-
#   readr::read_rds(
#     file =
#       paste0(
#         "data_indexpoints_tidy/rolling_indices_",
#         city_number,
#         ".rds"
#       )
#   ) |>
#   dplyr::bind_rows() |>
#   dplyr::filter(
#     window == "12_months",
#     month_object == "2024-12-01"
#   ) |>
#   dplyr::mutate(
#     version = "official_12_month"
#   )
#
# index_comparison <-
#   dplyr::bind_rows(
#     index_chained,
#     index_chained_original,
#     index_12_month_original
#   ) |>
#   dplyr::select(
#     version,
#     year,
#     n_trp,
#     index_p,
#     ci_lower, ci_upper
#   ) |>
#   dplyr::mutate(
#     ci_width = ci_upper - ci_lower
#   )
#
# readr::write_rds(
#   index_comparison,
#   "representativity/new_index_comparison_nj.rds"
# )


## CMDT and improved ----
function_class_tw <-
  links_nj |>
  sf::st_drop_geometry() |>
  dplyr::select(
    tw_km = tw,
    function_class
  ) |>
  dplyr::summarise(
    tw_fcl_population_kkm = base::sum(tw_km) / 1000,
    n_links = n(),
    .by = function_class
  ) |>
  dplyr::arrange(function_class)

trp_weights <-
  links_nj |>
  sf::st_drop_geometry() |>
  dplyr::left_join(
    link_trp_id,
    by = "link_id"
  ) |>
  dplyr::filter(
    !is.na(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    length_m,
    function_class
  ) |>
  dplyr::mutate(
    length_m = base::round(length_m)
  ) |>
  dplyr::left_join(
    function_class_tw,
    by = "function_class"
  )

### CMDT ----
mdt_filtered <-
  readr::read_rds(
    paste0(
      "data_indexpoints_tidy/cmdt_",
      city_number,
      ".rds"
    )
  ) |>
  dplyr::filter(
    length_class == "korte"
  )

# To get the mdt_validated df
source("exclude_cmdt.R")
length(unique(mdt_validated$trp_id))


### Original TRPs ----
nj_index_month <-
  mdt_validated |>
  dplyr::filter(
    trp_id %in% this_citys_trps_all_adt_final$trp_id
  ) |>
  calculate_area_index_month(population_size)

area_index_one_year_nj <- calculate_rolling_area_index_one_year(nj_index_month)

area_index_three_years_nj <- calculate_rolling_index_multiple_years(area_index_one_year_nj, 3)

readr::write_rds(
  nj_index_month,
  "representativity/cmdt_index_month_nj.rds"
)

list(
  area_index_one_year_nj |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(
      window_years = "one"
    ),
  area_index_three_years_nj |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(
      window_years = "three"
    )
  ) |>
  readr::write_rds(
    "representativity/rolling_cmdt_index_nj.rds"
  )


### More TRPs ----
nj_index_month_more <-
  mdt_validated |>
  dplyr::filter(
    !(trp_id %in% c(
      "73355V319671", # Austråttunnelen, er komplementær med Hana ved Rovik som følge av ny bom?
      "83652V319725", # Strandgata nord, mye som har foregått her...
      "43296V319721"  # Åsedalen, ny kobling til E39 oktober 2018.
      #"59675V319722"  # Brualand, avvikende verdi, neppe riktig, men finner ingen åpenbar grunn.
    )),
    !(trp_id == "89457V2303027" & universal_year_period_id == 15),
    !(trp_id == "71798V319583" & universal_year_period_id %in% c(15, 16, 17))
  ) |>
  calculate_area_index_month(population_size)

area_index_one_year_nj_more <- calculate_rolling_area_index_one_year(nj_index_month_more)

area_index_three_years_nj_more <- calculate_rolling_index_multiple_years(area_index_one_year_nj_more, 3)

readr::write_rds(
  nj_index_month_more,
  "representativity/cmdt_index_month_nj_more.rds"
)

list(
  area_index_one_year_nj_more |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(
      window_years = "one"
    ),
  area_index_three_years_nj_more |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(
      window_years = "three"
    )
) |>
  readr::write_rds(
    "representativity/rolling_cmdt_index_nj_more.rds"
  )


### Chained ----
# Chain link 1: 2017-2019
cmdt_chain_1 <-
  area_index_one_year_nj_more |>
  dplyr::filter(
    x_label == "des 19"
  )

# Chain link 2: 2019-2023
nj_index_month_more_2 <-
  mdt_validated |>
  dplyr::filter(
    !(trp_id %in% c(
      # Åpning av Eiganestunnelen og Ryfylketunnelen:
      "17949V320695", # Bybrua sør
      "10795V320297", # Randabergveien
      "58562V320296", # Tanke Svilandsgate
      "08952V320223", # Bjergsted
      "68351V319882", # Kannik
      "57279V320244", # Storhaugtunnelen
      "54577V319746", # Hillevågstunnelen
      "55507V319881", # Madlaveien Mosvatnet
      "71535V319524", # Lassa
      "83652V319725", # Strandgata nord
      "92102V319885", # Bergelandstunnelen
      "50749V319525", # Byhaugtunnelen sør
      "86207V319742", # Lagårdsveien
      "32842V319521", # Mosheim
      "10028V320295", # Løkkeveien
      #
      "59675V319722"  # Brualand: Avvikende verdi, ukjent årsak
    ))
  ) |>
  dplyr::filter(
    year >= 2019
  ) |>
  calculate_area_index_month(population_size)

area_index_one_year_nj_more_2 <- calculate_rolling_area_index_one_year(nj_index_month_more_2)

cmdt_chain_2 <-
  area_index_one_year_nj_more_2 |>
  dplyr::filter(
    x_label == "des 23"
  )

# Chain link 3: 2023-2024
nj_index_month_more_3 <-
  mdt_validated |>
  dplyr::filter(
    !(trp_id %in% c(
      "88125V320152", # Austrått
      "89794V320138", # Hoveveien, negativ korrelasjon mellom denne og Austrått, vegarbeid i nærheten?
      "58562V320296"  # Tanke Svilandsgate
    ))
  ) |>
  dplyr::filter(
    year >= 2023
  ) |>
  calculate_area_index_month(population_size)

area_index_one_year_nj_more_3 <- calculate_rolling_area_index_one_year(nj_index_month_more_3)

# Gather
nj_index_month_more_chained <-
  dplyr::bind_rows(
    nj_index_month_more |>
      dplyr::filter(
        universal_year_period_id <= 56
      ),
    nj_index_month_more_2 |>
      dplyr::filter(
        universal_year_period_id <= 112
      ),
    nj_index_month_more_3 |>
      dplyr::filter(
        universal_year_period_id <= 126
      )
  )

readr::write_rds(
  nj_index_month_more_chained,
  "representativity/cmdt_index_month_nj_more_chained.rds"
)

index_chained <-
  dplyr::bind_rows(
    cmdt_chain_1,
    cmdt_chain_2,
    area_index_one_year_nj_more_3
  ) |>
  dplyr::mutate(
    universal_year_period_id = as.character(universal_year_period_id)
  ) |>
  dplyr::select(
    universal_year_period_id,
    index_i, var_i
  )

index_chain_1_2 <- calculate_chained_cmdt_index(dplyr::slice(index_chained, 1), dplyr::slice(index_chained, 2))

index_chain_1_2_3 <-
  calculate_chained_cmdt_index(index_chain_1_2, dplyr::slice(index_chained, 3)) |>
  dplyr::mutate(
    index_p = 100 * (index_i - 1),
    sd_p = 100 * base::sqrt(var_i),
    em_p = -stats::qnorm(0.025) * sd_p,
    ci_lower = index_p - em_p,
    ci_upper = index_p + em_p
  )


# Oslo ----
{
  city_number <- "959"
  present_year <- 2024
  index_month <- 12
  source("set_time_references.R")
}

## TRPs ----
this_citys_trps_all_adt_final <-
  readr::read_rds(
    file = paste0(
      "index_trp_metadata/trp_",
      city_number,
      ".rds"
    )
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    municipality_name,
    #lat, lon,
    adt, year_aadt, adt_ref
  )


## Link population ----
# Made in script city_link_population.R
links_oslo <-
  readr::read_rds(
    "traffic_link_pop/links_oslo.rds"
  )
population_size <- nrow(links_oslo)

function_class_tw <-
  links_oslo |>
  sf::st_drop_geometry() |>
  dplyr::select(
    tw_km = tw,
    function_class
  ) |>
  dplyr::summarise(
    tw_fcl_population_kkm = base::sum(tw_km) / 1000,
    n_links = n(),
    .by = function_class
  ) |>
  dplyr::arrange(function_class)

trp_weights <-
  links_oslo |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    !is.na(point_id)
  ) |>
  dplyr::select(
    trp_id = point_id,
    length_m,
    function_class
  ) |>
  dplyr::mutate(
    length_m = base::round(length_m)
  ) |>
  dplyr::left_join(
    function_class_tw,
    by = "function_class"
  )

# missing <-
#   this_citys_trps_all_adt_final |>
#   dplyr::filter(
#     !(trp_id %in% trp_weights$trp_id)
#   )
# Årnes Runni is outside urban area, Fjellsrud syd is on wrong road (no data since 2017)


## MDT ----
mdt_filtered <-
  readr::read_rds(
    paste0(
      "data_indexpoints_tidy/cmdt_",
      city_number,
      ".rds"
    )
  ) |>
  dplyr::filter(
    length_class == "korte"
  )

# To get the mdt_validated df
source("exclude_cmdt.R")

# mdt_validated |>
#   dplyr::filter(trp_id %in% city_trps[1:2]) |>
#   dplyr::select(
#     trp_id,
#     year,
#     month,
#     mdt
#   ) |>
#   tidyr::complete(
#     trp_id,
#     year,
#     month,
#     fill = list(mdt = NA_real_)
#   ) |>
#   dplyr::left_join(
#     points,
#     by = "trp_id"
#   ) |>
#   dplyr::mutate(
#     road_category_and_number_and_point_name = paste0(road_category_and_number, " ", name),
#     year = forcats::as_factor(year)
#   ) |>
#   dplyr::select(
#     trp_id,
#     year,
#     month,
#     mdt,
#     road_category_and_number_and_point_name
#   ) |>
#   barplot_cmdt()


## Index calculation ----
oslo_index_month <-
  mdt_validated |>
  calculate_area_index_month(population_size)

area_index_one_year_oslo <- calculate_rolling_area_index_one_year(oslo_index_month)

area_index_three_years_oslo <- calculate_rolling_index_multiple_years(area_index_one_year_oslo, 3)

# Back on track
readr::write_rds(
  oslo_index_month,
  "representativity/cmdt_index_month_oslo.rds"
)

list(
  area_index_one_year_oslo |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(
      window_years = "one"
    ),
  area_index_three_years_oslo |>
    dplyr::select(
      universal_year_period_id,
      x_label,
      index_p,
      ci_lower,
      ci_upper
    ) |>
    dplyr::mutate(
      window_years = "three"
    )
) |>
  readr::write_rds(
    "representativity/rolling_cmdt_index_oslo.rds"
  )
