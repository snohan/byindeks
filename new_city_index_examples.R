# Calculate city index using new method

# Background ----
# New concepts in improved method:
# - Calendar adjusted MDT
# - Traffic work weights (from traffic links)
# - Chaining when necessary (need to suitably subdivide index period, possibly one road net version per subperiod)
# - Measures of representativity

# To be included later:
# - Vehicle classification by type, not length

# Resolution in time:
# - Month
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

# How to compare methods:
# - hard to make direct comparisons and attribute differences to specific parts of new method
# - new versus old index results in different time resolutions, separated from new chaining strategies
# - the impact of new chaining strategies leading to better representativity

# What are isolated improvements?
# - more measures of representativity
# - smaller confidence interval, mostly due to weighting scheme
# - traffic work weights by link and strata


# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  svv_background_color <- "#F5F5F5"

  library(tidyverse)
  #library(boot)

  source("get_from_trafficdata_api.R")
  source("split_road_system_reference.R")
  source("indexpoints_tidying_functions.R")
  source("index_report_functions.R")
  source("traffic_link_functions.R")

  link_id_weights_2024 <- readr::read_rds("traffic_link_pop/link_id_weights_2024.rds")
  link_trp_id <- readr::read_rds("traffic_link_pop/link_trp_id.rds")
  points <- readr::read_rds("trps_for_city_index.rds")
}


# Bergen ----
{
  city_number <- "8952"
  links_in_area <- readr::read_rds("traffic_link_pop/links_bergen.rds")
}

source("new_city_index_examples_prepare.R")
source("new_city_index_examples_calculate.R")

# Check:
# missing <-
#   this_citys_trps_all_adt_final |>
#   dplyr::filter(
#     !(trp_id %in% trp_weights$trp_id)
#   )
# Some TRPs are outside urban area, some are missing from links.

# Sidetrack: for showing some data in presentation
# viz_mdt <- mdt_validated |> select(trp_id, year, month, mdt, length_m, function_class)
# viz_month <- brg_index_month |> select(x_label, index_p, n_trp)
# viz_one_y <- area_index_one_year_brg |> select(x_label, index_p, ci_lower, ci_upper) |> mutate(across(where(is.double), ~ round(.x, 1)))
# viz_three_y <- area_index_three_years_brg |> mutate(across(where(is.double), ~ round(.x, 1)))


# Oslo ----
{
  city_number <- "959"
  links_in_area <- readr::read_rds("traffic_link_pop/links_oslo.rds")
}

source("new_city_index_examples_prepare.R")
source("new_city_index_examples_calculate.R")

# missing <-
#   this_citys_trps_all_adt_final |>
#   dplyr::filter(
#     !(trp_id %in% trp_weights$trp_id)
#   )
# Årnes Runni is outside urban area, Fjellsrud syd is on wrong road (no data since 2017)


# Trondheim ----
{
  city_number <- "960"
  links_in_area <- readr::read_rds("traffic_link_pop/links_trondheim.rds")
}

source("new_city_index_examples_prepare.R")
source("new_city_index_examples_calculate.R")

# missing <-
#   this_citys_trps_all_adt_final |>
#   dplyr::filter(
#     !(trp_id %in% trp_weights$trp_id)
#   )
# Some are outside urban area (1), some are missing from links (6 on K-roads)


# Nord-Jæren ----
{
  city_number <- "952"
  links_in_area <- readr::read_rds("traffic_link_pop/links_nj.rds")
}

source("new_city_index_examples_prepare.R")

# missing <-
#   this_citys_trps_all_adt_final |>
#   dplyr::filter(
#     !(trp_id %in% trp_weights$trp_id)
#   )
# Rege is outside urban area

# trps_existing <-
#   link_trp_id |>
#   dplyr::filter(
#     link_id %in% links_in_area$link_id
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


## Original TRPs ----
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


## More TRPs ----
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


## Chained ----
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
        universal_year_period_id %in% c(43:56)
      ),
    nj_index_month_more_2 |>
      dplyr::filter(
        universal_year_period_id %in% c(99:112)
      ),
    nj_index_month_more_3 |>
      dplyr::filter(
        universal_year_period_id %in% c(113:126)
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
