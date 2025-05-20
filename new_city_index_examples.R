# Calculate city index using new method

# New concepts:
# - Using only MDT (normal situation, seasonally adjusted)
# - Traffic work weights (need traffic links)
# - Chaining when necessary (need to suitably subdivide index period, possibly one road net version per subperiod)
# - Estimate confidence interval (compare and decide which method to use)
# - Measures of representativity

# To be included later:
# - Seasonally adjusted MDT
# - Vehicle classification by type, not just length

# Resolution in time:
# - Month by month
# - So far this year
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
# - new versus old index results in different time resolutions, separated from new chaining strategies
# - the impact of new chaining strategies leading to better representativity


# What are isolated improvements?
# - measures of representativity, especially traffic work
# - smaller (?) confidence interval, mostly (?) due to finiteness of population
# - traffic work weights

# Suitable cities as examples:
# - Bergen (chaining might be useful)
# - Buskerudbyen (has had representativity issues, no chaining?)
# - Trondheim (representativity issues, no chaining?)
# - Nord-Jæren (with new chaining strategy)


# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  svv_background_color <- "#F5F5F5"

  library(tidyverse)
  library(boot)

  source("split_road_system_reference.R")
  source("indexpoints_tidying_functions.R")
  source("index_report_functions.R")
  source("get_from_trafficdata_api.R")
  source("traffic_link_functions.R")

  link_id_weights_2024 <- readr::read_rds("traffic_link_pop/link_id_weights_2024.rds")
}

# Bergen ----
city_number <- "8952"
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
links_bergen <-
  readr::read_rds(
    "traffic_link_pop/links_bergen.rds"
  )

population_size <- nrow(links_bergen)

trp_weights <-
  links_bergen |>
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
# TODO: heatmap per TRP per month, one for each year
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
    trp_id %in% links_bergen$point_id
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

readr::write_csv2(
  mdt_yearly,
  "spesialuttak/mdt_bergen.csv"
)


## Index calculation ----
# Since some TRPs are lost because they are outside urban area or missing from raw links,
# a comparison between new and old methods must be based on same MDT data set, also.

# Mostly interested in intervals of at least 12 months, thus limiting the examples.

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
  "representativity/new_index_examples_bergen.rds"
)

# For Jonas
#all_rolling_indices_new |> readr::write_csv2("spesialuttak/alle_glidende_indekser_test_bergen.csv")

# The offical results
all_rolling_indices_official <-
  readr::read_rds(
    file =
      paste0(
        "data_indexpoints_tidy/rolling_indices_",
        city_number,
        ".rds"
      )
  ) |>
  dplyr::bind_rows()

compare_indexes <-
  dplyr::inner_join(
    prepare_rolling_indexes_for_comparison(all_rolling_indices_official),
    prepare_rolling_indexes_for_comparison(all_rolling_indices_old),
    by = dplyr::join_by(index_period, window),
    suffix = c("_official", "_old")
  ) |>
  dplyr::relocate(
    index_period,
    window
  )


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
city_number <- "952"
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
links_nj <-
  readr::read_rds(
    "traffic_link_pop/links_nj.rds"
  )

population_size <- nrow(links_nj)

trp_weights <-
  links_nj |>
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
# Rege is outside urban area


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
    trp_id %in% links_nj$point_id
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
    "representativity/new_index_examples_nj.rds"
  )


## More TRPS, direct ----
# Using as many TRPs as possible
# 1. Existing TRPs
# 2. All MDTs
# 3. Filter MDTs by year
# 4. Decide chain intervals

trps_existing <-
  link_trp_id |>
  dplyr::filter(
    link_id %in% links_nj$link_id
  ) |>
  dplyr::left_join(
    points,
    by = dplyr::join_by(trp_id)
  )

latest_adt <-
  get_aadt_for_trp_list(trps_existing$trp_id) |>
  dplyr::slice_max(
    order_by = year,
    by = trp_id
  )

# Look at map
trps_existing_2 <-
  trps_existing |>
  dplyr::left_join(
    latest_adt,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::mutate(
    #label_text = paste0(trp_id, " ", name) |> base::lapply(htmltools::HTML),
    label_text = paste0(trp_id, "<br/>", name) |> purrr::map(~ htmltools::HTML(.x))
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    lat, lon,
    adt,
    label_text
  )

trps_existing_2 |>
  create_point_adt_map_with_labels()

### MDT ----
{
  tictoc::tic()
  mdt <-
    purrr::map_dfr(
      years_from_reference_to_today,
      ~ get_mdt_by_length_for_trp_list(trps_existing$trp_id, .x)
    )
  tictoc::toc()
  }

trp_weights <-
  links_nj |>
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

mdt_filtered <-
  mdt |>
  dplyr::filter(
    length_range == "[..,5.6)"
  ) |>
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
  dplyr::left_join(
    link_id_weights_2024,
    by = dplyr::join_by(trp_id)
  ) |>
  tibble::as_tibble()


### Direct index ----
# May use the same exclusions as the official index
# TODO: Possibly with a few additions
source("exclude_trp_mdts_list.R")

mdt_validated <-
  mdt_validated |>
  dplyr::filter(
    !(year_month >= "2020-01-01" &
      trp_id %in%  c(
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
        "10028V320295"  # Løkkeveien
      )
    )
  )

# Direct index with more TRPs
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
    "representativity/new_index_examples_2_nj.rds"
  )


### Chaining ----
# Must have tailored exclusions to accomodate a maximum utilisation of TRPs in each chain period

# Chains
index_2017_2019 <-
  calculate_rolling_indices_tw(
    reference_year,
    "2019-12-01",
    12,
    mdt_filtered,
    population_size,
    "by_area"
  )

trp_index_2017_2019 <-
  calculate_rolling_indices_tw(
    reference_year,
    "2019-12-01",
    12,
    mdt_filtered,
    population_size,
    "by_trp"
  ) |>
  dplyr::left_join(
    points,
    by = dplyr::join_by(trp_id)
  )

index_2019_2023 <-
  calculate_rolling_indices_tw(
    2019,
    "2023-12-01",
    12,
    mdt_filtered,
    population_size,
    "by_area"
  )

trp_index_2019_2023 <-
  calculate_rolling_indices_tw(
    2019,
    "2023-12-01",
    12,
    mdt_filtered,
    population_size,
    "by_trp"
  ) |>
  dplyr::left_join(
    points,
    by = dplyr::join_by(trp_id)
  )

index_2023_2024 <-
  calculate_rolling_indices_tw(
    2023,
    "2024-12-01",
    12,
    mdt_filtered,
    population_size,
    "by_area"
  )

trp_index_2023_2024 <-
  calculate_rolling_indices_tw(
    2023,
    "2024-12-01",
    12,
    mdt_filtered,
    population_size,
    "by_trp"
  ) |>
  dplyr::left_join(
    points,
    by = dplyr::join_by(trp_id)
  )

chained <- index_2017_2019$index_i * index_2019_2023$index_i * index_2023_2024$index_i
