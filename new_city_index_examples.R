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
# - So far this year
# - Last 12 months (possibly also multiples of 12, but not prioritized)

# Resolution in day type
# - working days
# - non-working days
# - all days

# Resolution in vehicle type:
# - light (short)
# - heavy (long)
# - all

# How to compare methods:
# - hard to make direct comparisons and attribute differences to specific parts of new method, since many things is done differently
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
  link_toll_id <- readr::read_rds("traffic_link_pop/link_toll_id.rds") |> 
    # Add links that are missing their toll ids
    dplyr::bind_rows(
      tibble::tibble(
        link_id = c("0.0@2798059-1.0@3500119"),
        toll_id = c("906727254")
      )
    )
  points <- readr::read_rds("trps_for_city_index.rds")
}


# Bergen ----
city_number <- "8952"
links_in_area <- readr::read_rds("traffic_link_pop/links_bergen.rds")
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
city_number <- "959"
links_in_area <- readr::read_rds("traffic_link_pop/links_oslo.rds")
source("new_city_index_examples_prepare.R")
source("new_city_index_examples_calculate.R")

# missing <-
#   this_citys_trps_all_adt_final |>
#   dplyr::filter(
#     !(trp_id %in% trp_weights$trp_id)
#   )
# Årnes Runni is outside urban area, Fjellsrud syd is on wrong road (no data since 2017)


# Trondheim ----
city_number <- "960"
links_in_area <- readr::read_rds("traffic_link_pop/links_trondheim.rds")
source("new_city_index_examples_prepare.R")
source("new_city_index_examples_calculate.R")

# missing <-
#   this_citys_trps_all_adt_final |>
#   dplyr::filter(
#     !(trp_id %in% trp_weights$trp_id)
#   )
# Some are outside urban area (1), some are missing from links (6 on K-roads)


# Nord-Jæren ----
city_number <- "952"
links_in_area <- readr::read_rds("traffic_link_pop/links_nj.rds")

# !!! 
# Test without traffic work weights per link
# links_in_area <- links_in_area |> dplyr::mutate(length_m = 1)
# !!!

source("bomdata_nj_stations.R")
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

area_index_one_year_nj <- calculate_rolling_area_index_one_year(nj_index_month[[1]])
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

area_index_one_year_nj_more <- calculate_rolling_area_index_one_year(nj_index_month_more[[1]])
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


## Chained, v. 2025 ----
# Goal: have an index chain solely through the link years

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
  dplyr::filter(year >= 2019) |>
  calculate_area_index_month(population_size)

area_index_one_year_nj_more_2 <- calculate_rolling_area_index_one_year(nj_index_month_more_2[[1]])

cmdt_chain_2 <-
  area_index_one_year_nj_more_2 |>
  dplyr::filter(
    x_label == "des 23"
  )

# Chain link 3: 2023-
nj_mdt_more_3 <-
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
  ) 

# For the walk through example
readr::write_rds(
  nj_mdt_more_3,
  "representativity/cmdt_index_nj_more_chained_p3_mdt.rds"
)

nj_index_month_more_3 <-
  nj_mdt_more_3 |>
  calculate_area_index_month(population_size)

area_index_one_year_nj_more_3 <- calculate_rolling_area_index_one_year(nj_index_month_more_3[[1]])

# Gather
nj_index_month_more_chained <-
  dplyr::bind_rows(
    nj_index_month_more[[1]] |>
      dplyr::filter(
        universal_year_period_id %in% c(43:56)
      ),
    nj_index_month_more_2[[1]] |>
      dplyr::filter(
        universal_year_period_id %in% c(99:112)
      ),
    nj_index_month_more_3[[1]] |>
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


## Chained, v. 2026 ----
# Goal: have a continuous rolling index time series
# Must then chain each month

# Gathering CMDT for all chain parts
 nj_month_indices_original <-
  dplyr::bind_rows(
    nj_index_month_more[[1]] |> 
      dplyr::filter(
        universal_year_period_id <= 56
      ),
    nj_index_month_more_2[[1]] |> 
      dplyr::filter(
        universal_year_period_id <= 112
      ),
    nj_index_month_more_3[[1]] |> 
      dplyr::filter(
        universal_year_period_id > 112
      )
  )

# Number of TRPs and representativity measures should be taken from this!
readr::write_rds(
  nj_month_indices_original,
  "representativity/cmdt_index_month_nj_original_chain_parts.rds"
)

# Next: all but first chain part is not yet chained back to ref year.
# For calculating rolling indices, we only need universal_year_period_id, index_i, var_i.
# Must chain each month
# Chain part 1: 2017-2019, no chaining
# Chain part 2: 2019-2023, chained via 2019
# Chain part 3: 2023-, chained via 2023 and 2019

# nj_index_month_chained_all <-
nj_month_indices_original_with_chain_info <-
  nj_month_indices_original |> 
  dplyr::select(
    universal_year_period_id, x_label, compared_to, period_name, index_i, var_i = var_robust_i
  ) |> 
  dplyr::left_join(
    universal_calendar_periods |> 
      dplyr::select(universal_year_period_id_chain = universal_year_period_id, year, period_name),
    by = dplyr::join_by(compared_to == year, period_name)
  ) |> 
  dplyr::mutate(
    # Need to chain each chain part in consecutive order, identifying the different parts:
    chain_part = as.numeric(factor(compared_to))
  )

# Chain parts
chain_part_1 <- 
  nj_month_indices_original_with_chain_info |> 
  dplyr::filter(chain_part %in% c(1)) |> 
  dplyr::select(
    universal_year_period_id, x_label, period_name, compared_to, index_i, var_i
  )

chain_part_2 <- 
  nj_month_indices_original_with_chain_info |> 
  dplyr::filter(
    chain_part %in% c(1, 2)
  ) |> 
  chain_index_months() |> 
  dplyr::select(
    universal_year_period_id, x_label, period_name, compared_to, index_i, var_i
  )

chain_part_3 <-
  dplyr::bind_rows(
    chain_part_2 |> 
      dplyr::left_join(
        universal_calendar_periods |> 
          dplyr::select(universal_year_period_id_chain = universal_year_period_id, year, period_name),
        by = dplyr::join_by(compared_to == year, period_name)
      ) |> 
      dplyr::mutate(
        # Need to chain in series, identifying the different parts:
        chain_part = 2
      ),
    nj_month_indices_original_with_chain_info |> 
      dplyr::filter(
        chain_part == 3
      ) 
  ) |> 
  chain_index_months()

nj_month_indices_chained <- 
  dplyr::bind_rows(
    chain_part_1,
    chain_part_2,
    chain_part_3
  ) |> 
  dplyr::select(
    universal_year_period_id, x_label, period_name, compared_to, index_i, var_robust_i = var_i
  )

area_index_one_year_nj_chained <- calculate_rolling_area_index_one_year(nj_month_indices_chained)
area_index_three_years_nj_chained <- calculate_rolling_index_multiple_years(area_index_one_year_nj_chained, 3)

list(
  area_index_one_year_nj_chained |>
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
  area_index_three_years_nj_chained |>
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
    "representativity/rolling_cmdt_index_nj_chained.rds"
  )


### With 2019 as reference year ----
# Gathering CMDT for all chain parts
 nj_month_indices_original_2019 <-
  dplyr::bind_rows(
    nj_index_month_more_2[[1]] |> 
      dplyr::filter(
        universal_year_period_id <= 112
      ),
    nj_index_month_more_3[[1]] |> 
      dplyr::filter(
        universal_year_period_id > 112
      )
  )

nj_month_indices_original_with_chain_info_2019 <-
  nj_month_indices_original_2019 |> 
  dplyr::select(
    universal_year_period_id, x_label, compared_to, period_name, index_i, var_i = var_robust_i
  ) |> 
  dplyr::left_join(
    universal_calendar_periods |> 
      dplyr::select(universal_year_period_id_chain = universal_year_period_id, year, period_name),
    by = dplyr::join_by(compared_to == year, period_name)
  ) |> 
  dplyr::mutate(
    # Need to chain each chain part in consecutive order, identifying the different parts:
    chain_part = as.numeric(factor(compared_to))
  )

# Chain parts
chain_part_1_2019 <- 
  nj_month_indices_original_with_chain_info_2019 |> 
  dplyr::filter(chain_part %in% c(1)) |> 
  dplyr::select(
    universal_year_period_id, x_label, period_name, compared_to, index_i, var_i
  )

chain_part_2_2019 <- 
  nj_month_indices_original_with_chain_info_2019 |> 
  dplyr::filter(
    chain_part %in% c(1, 2)
  ) |> 
  chain_index_months() |> 
  dplyr::select(
    universal_year_period_id, x_label, period_name, compared_to, index_i, var_i
  )

nj_month_indices_chained_2019 <- 
  dplyr::bind_rows(
    chain_part_1_2019,
    chain_part_2_2019
  ) |> 
  dplyr::select(
    universal_year_period_id, x_label, period_name, compared_to, index_i, var_robust_i = var_i
  )

area_index_one_year_nj_chained_2019 <- calculate_rolling_area_index_one_year(nj_month_indices_chained_2019)


# TRP index
trp_index_2019_by_month <- 
  nj_index_month_more_2[[2]] |> 
  dplyr::filter(
    !(trp_id %in% c(
      "88125V320152", # Austrått
      "89794V320138", # Hoveveien, negativ korrelasjon mellom denne og Austrått, vegarbeid i nærheten?
      "58562V320296"  # Tanke Svilandsgate
    ))
  )

trp_index_2019_rolling <- calculate_rolling_trp_index_one_year(trp_index_2019_by_month)

readr::write_rds(
  trp_index_2019_rolling,
  "representativity/rolling_cmdt_trp_index_nj_2019.rds"
)


## Chained with toll data ----
# Using okt17-sep18 as reference year

source("exclude_cmdt_extra.R")

# Need to include some TRPs that was deemed unusable in former index (mdt_validated)
mdt_validated_nj <-
  mdt_filtered |>  
  dplyr::left_join(
    universal_calendar_periods |>
      dplyr::select(
        universal_year_period_id,
        year,
        period_name
      ),
    by = dplyr::join_by(year, month == period_name)
  ) |>
  dplyr::inner_join(
    # "inner" works as a filter here!
    trp_weights, # NB! these TRPs are filtered by urban area
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::mutate(
    month = base::factor(month, levels = period_names)
  )


# Chain part 1
# okt17-sep18 -- okt18-sep19
index_month_values_1 <-
  mdt_validated |>
  # mdt_validated_nj |>
  dplyr::filter(
    universal_year_period_id >= 26,
    universal_year_period_id <= 53
  ) |> 
  exclude_periods(exclusions_nj_okt17_sep18__nj_okt18_sep19) |> 
  calculate_area_index_month(population_size)

area_index_month_1 <- index_month_values_1[[1]]
link_index_month_1 <- index_month_values_1[[2]]

# area_index_one_year_1 <- calculate_rolling_area_index_one_year(area_index_month_1)

# Chain part 2
# okt18-sep19 -- 2023
index_month_values_2 <-
  mdt_validated_nj |>
  dplyr::filter(
    universal_year_period_id >= 40,
    universal_year_period_id <= 112
  ) |> 
  exclude_periods(exclusions_nj_okt18_sep19__2023) |> 
  calculate_area_index_month(population_size)

area_index_month_2 <- index_month_values_2[[1]]
link_index_month_2 <- index_month_values_2[[2]]


# Chain part 3
# 2023 -- 
index_month_values_3_x <-
  mdt_validated_nj |>
  dplyr::filter(
    universal_year_period_id >= 99
  ) |> 
  exclude_periods(exclusions_nj_2023__) |> 
  exclude_periods(exclusions_nj_equipment_2023__) |> 
  calculate_area_index_month(population_size)

area_index_month_3 <- index_month_values_3[[1]]
link_index_month_3 <- index_month_values_3[[2]]

# area_index_one_year_3 <- calculate_rolling_area_index_one_year(area_index_month_3)

### Chaining ----
nj_month_indices_with_chain_info <-
  dplyr::bind_rows(
    area_index_month_1,
    area_index_month_2,
    area_index_month_3
  ) |> 
  dplyr::select(
    universal_year_period_id, x_label, compared_to, period_name, compared_to_uypid, reference_period, index_i, var_i = var_robust_i
  ) |> 
  # Need to chain each chain part in consecutive order, identifying the different parts, in the correct order (chronological)
  dplyr::mutate(
    reference_period_uypid_start = base::min(compared_to_uypid),
    .by = reference_period
  ) |> 
  dplyr::mutate(
    chain_part = as.numeric(factor(reference_period_uypid_start))
  )

# Chain parts
chain_part_1 <- 
  nj_month_indices_with_chain_info |> 
  dplyr::filter(chain_part %in% c(1)) |> 
  dplyr::select(
    universal_year_period_id, x_label, period_name, compared_to, compared_to_uypid, reference_period, index_i, var_i
  )

chain_part_2 <- 
  nj_month_indices_with_chain_info |> 
  dplyr::filter(chain_part %in% c(1, 2)) |> 
  chain_index_months()

chain_part_3 <-
  dplyr::bind_rows(
    chain_part_2 |> dplyr::mutate(chain_part = 2),
    nj_month_indices_with_chain_info |> dplyr::filter(chain_part == 3) 
  ) |> 
  chain_index_months()

nj_month_indices_chained <- 
  dplyr::bind_rows(
    chain_part_1,
    chain_part_2,
    chain_part_3
  ) |> 
  dplyr::rename(var_robust_i = var_i)

area_index_one_year_nj_chained <- calculate_rolling_area_index_one_year(nj_month_indices_chained)

area_index_one_year_nj_chained |>
  dplyr::select(
    universal_year_period_id,
    x_label,
    index_p,
    ci_lower,
    ci_upper
  ) |>
  readr::write_rds(
    "representativity/rolling_cmdt_index_nj_chained_toll_no_exclusions.rds"
  )


### Rolling index plot ----
area_index_one_year_nj_chained |> 
visualize_rolling_cmdt_index(
    "Data: Statens vegvesen, Rogaland fylkeskommune",
    "Estimert endring i trafikkmengde, forbedret metode, ingen ekskluderinger",
    paste0("Siste år sammenlignet med okt17-sep18")
  ) +
  theme(
    plot.background = element_rect(fill = svv_background_color),
    panel.background = element_rect(fill = svv_background_color),
    legend.background = element_rect(fill = svv_background_color)
  ) 
  # ggplot2::scale_y_continuous(
  #   limits = c(-18, 6), 
  #   breaks = seq(-18, 6, by = 1)
  # )


### N points plot ----
dplyr::bind_rows(
  area_index_month_1,
  area_index_month_2,
  area_index_month_3
) |> 
dplyr::select(x_label, n_trp) |>
dplyr::mutate(
  x_label = as.factor(x_label) |> forcats::fct_inorder()
) |>
ggplot2::ggplot(aes(x = x_label, y = n_trp)) +
ggplot2::geom_point(color = "#ed9300") +
geom_line(group = '', color = "#ed9300") +
theme_light() +
theme(
  axis.text.x = element_text(vjust = 0.5, angle = 90),
  axis.title.y = element_text(
    margin = margin(t = 0, r = 15, b = 0, l = 0)),
  axis.title.x = element_text(
    margin = margin(t = 15, r = 0, b = 0, l = 0)),
  panel.grid.minor.x = element_blank(),
  plot.caption =
    element_text(
      face = "italic",
      size = 8,
      lineheight = 1.5,
      vjust = 0
    ),
  plot.background = element_rect(fill = svv_background_color),
  panel.background = element_rect(fill = svv_background_color),
  legend.background = element_rect(fill = svv_background_color),
  legend.position = "bottom"
) +
ggplot2::scale_x_discrete(
  name = NULL,
  breaks = ~ dplyr::if_else(stringr::str_detect(.x, "des"), .x, "")
) +
ggplot2::labs(
  x = NULL, y = "Antall punkt", 
  title = "Antall indekspunkt", subtitle = "Forbedret metode, kjedet indeks inkludert bomstasjoner, Nord-Jæren"
)

# Stacked barplot depicting number of toll and trps
dplyr::bind_rows(
  link_index_month_1,
  link_index_month_2,
  link_index_month_3
) |> 
  dplyr::select(trp_id, universal_year_period_id) |> 
  dplyr::left_join(
    universal_calendar_periods |> dplyr::mutate(x_label = as.factor(x_label) |> forcats::fct_inorder()),
    by = "universal_year_period_id"
  ) |> 
  dplyr::left_join(
    dplyr::bind_rows(
      points |> dplyr::select(trp_id) |> dplyr::mutate(source = "Trafikkdata"),
      bomstasjoner_nj |> dplyr::select(trp_id = nvdb_id) |> dplyr::mutate(source = "AutoPASS")
    ),
    by = "trp_id"
  ) |> 
  dplyr::select(x_label, source) |> 
  ggplot2::ggplot(aes(x = x_label, fill = source)) +
  ggplot2::geom_bar() +
  theme_light() +
theme(
  axis.text.x = element_text(vjust = 0.5, angle = 90),
  axis.title.y = element_text(
    margin = margin(t = 0, r = 15, b = 0, l = 0)),
  axis.title.x = element_text(
    margin = margin(t = 15, r = 0, b = 0, l = 0)),
  panel.grid.minor.x = element_blank(),
  plot.caption =
    element_text(
      face = "italic",
      size = 8,
      lineheight = 1.5,
      vjust = 0
    ),
  plot.background = element_rect(fill = svv_background_color),
  panel.background = element_rect(fill = svv_background_color),
  legend.background = element_rect(fill = svv_background_color),
  legend.position = "bottom"
) +
ggplot2::scale_x_discrete(
  name = NULL,
  breaks = ~ dplyr::if_else(stringr::str_detect(.x, "des"), .x, "")
) +
ggplot2::labs(
  x = NULL, y = "Antall punkt", 
  title = "Antall indekspunkt", subtitle = "Forbedret metode, kjedet indeks inkludert bomstasjoner, Nord-Jæren"
)


### Which points? ----
# Tables
# Wide with months as columns, but years as rows
# A column that gives chain part
# Need name and road, but not latlon
point_indexes <-
  dplyr::bind_rows(
    link_index_month_1 |> 
      dplyr::mutate(
        start_of_reference_period = base::min(universal_year_period_id) - 14,
        end_of_reference_period = base::min(universal_year_period_id) - 1
      ),
      link_index_month_2 |> 
      dplyr::mutate(
        start_of_reference_period = base::min(universal_year_period_id) - 14,
        end_of_reference_period = base::min(universal_year_period_id) - 1
      ),
      link_index_month_3 |> 
      dplyr::mutate(
        start_of_reference_period = base::min(universal_year_period_id) - 14,
        end_of_reference_period = base::min(universal_year_period_id) - 1
      )
  ) |> 
  dplyr::left_join(
    universal_calendar_periods |> dplyr::select(universal_year_period_id, x_label_start = x_label),
    by = dplyr::join_by(start_of_reference_period == universal_year_period_id)
  ) |> 
  dplyr::left_join(
    universal_calendar_periods |> dplyr::select(universal_year_period_id, x_label_end = x_label),
    by = dplyr::join_by(end_of_reference_period == universal_year_period_id)
  ) |> 
  dplyr::mutate(
    reference_period = base::paste0(x_label_start, "_", x_label_end) |> stringr::str_remove_all("\\s+"),
    p_abi_p = base::round(p_abi_p, 2)
  ) |> 
  dplyr::select(
    trp_id, year_b, month, p_abi_p, reference_period
  )

point_indexes_wide <-
  point_indexes |> 
  tidyr::pivot_wider(
    names_from = month,
    values_from = p_abi_p
  ) |> 
  dplyr::left_join(
    dplyr::bind_rows(
      points |> dplyr::select(trp_id, name, road_category_and_number) |> dplyr::mutate(source = "Trafikkdata"),
      bomstasjoner_nj |> dplyr::select(trp_id = nvdb_id, name, road_category_and_number) |> dplyr::mutate(source = "AutoPASS")
    ),
    by = "trp_id"
  ) |> 
  dplyr::select(trp_id, name, road_category_and_number, source, reference_period, year = year_b, all_of(period_names)) |> 
  dplyr::arrange(road_category_and_number, name, year)

writexl::write_xlsx(
  point_indexes_wide,
  "spesialuttak/nj_punktindeks_regneeksempel.xlsx"
)

# The exclusions
# Check which exclusions are used in today's index:
# mdt_manual_exclusions_here <-
#   mdt_manual_exclusions |> 
#   dplyr::filter(
#     trp_id %in% trp_weights$trp_id
#   ) |> 
#   dplyr::select(
#     trp_id, uyp_start = from_universal_year_period_id, uyp_end = to_universal_year_period_id
#   )

nj_exclusions <-
  dplyr::bind_rows(
    exclusions_nj_okt17_sep18__nj_okt18_sep19 |> dplyr::mutate(indeksperiode = "(okt 17-sep 18) - (okt 18-sep 19)"),
    exclusions_nj_okt18_sep19__2023 |> dplyr::mutate(indeksperiode = "(okt 18-sep 19) - 2023"),
    exclusions_nj_2023__ |> dplyr::mutate(indeksperiode = "2023 - ")
  ) |> 
  dplyr::left_join(
    dplyr::bind_rows(
      points |> dplyr::select(trp_id, name, road_category_and_number),
      bomstasjoner_nj |> dplyr::select(trp_id = nvdb_id, name, road_category_and_number)
    ),
    by = "trp_id"
  ) |> 
  dplyr::left_join(
    universal_calendar_periods |> dplyr::select(universal_year_period_id, x_label_start = x_label),
    by = dplyr::join_by(uyp_start == universal_year_period_id)
  ) |> 
  dplyr::left_join(
    universal_calendar_periods |> dplyr::select(universal_year_period_id, x_label_end = x_label),
    by = dplyr::join_by(uyp_end == universal_year_period_id)
  ) |> 
  dplyr::mutate(
    ekskludert =
      dplyr::case_when(
        is.na(x_label_start) & is.na(x_label_end) ~ "Hele indeksperioden",
        !is.na(x_label_start) & is.na(x_label_end) ~ base::paste0("Fom ", x_label_start, " og ut indeksperioden"),
        !is.na(x_label_start) & !is.na(x_label_end) ~ base::paste0("Fom ", x_label_start, " tom ", x_label_end)
      )
  ) |> 
  dplyr::select(
    indeksperiode,
    hendelse = text,
    punkt_id = trp_id, punktnavn = name, veg = road_category_and_number,
    ekskludert
  )

writexl::write_xlsx(
  nj_exclusions,
  "spesialuttak/nj_regneeksempel_ekskluderinger.xlsx"
)


# Maps
# Need a df with unique points in each chain part
# latlon
point_indexes_for_map <-
  point_indexes |> 
  dplyr::select(trp_id, reference_period) |> 
  dplyr::distinct() |> 
  dplyr::left_join(
    dplyr::bind_rows(
      points |> dplyr::select(trp_id, name, road_category_and_number, lat, lon) |> dplyr::mutate(source = "Trafikkregistrering"),
      bomstasjoner_nj |> dplyr::select(trp_id = nvdb_id, name, road_category_and_number, lat, lon) |> dplyr::mutate(source = "AutoPASS")
    ),
    by = "trp_id"
  )

readr::write_rds(
  point_indexes_for_map,
  "representativity/nj_updated_example_with_toll.rds"
)

