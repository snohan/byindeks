# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
  source("indexpoints_tidying_functions.R")
  source("index_calculation_functions.R")
  library(tictoc)
  library(writexl)
  library(sf)
}


# TRP ----
trp <-
  get_points() |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS"
  ) |>
  dplyr::select(
    trp_id, name, road_reference, municipality_name
  ) |>
  dplyr::distinct()

trp_time_span <- get_trp_data_time_span()


# Links ----
# links <- sf::st_read("traffic-links-2022.geojson")
# links_reduced <-
#   links |>
#   dplyr::select(
#     nvdb_id = nvdbId,
#     trp_id = primaryTrpId
#   )
# remove(links)


# Tromsø 2019-2022 ----
trp_trs <-
  trp |>
  dplyr::filter(
    municipality_name == "Tromsø"
  ) |>
  dplyr::select(
    trp_id, name, road_reference
  ) |>
  dplyr::left_join(
    trp_time_span,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(
    first_data_with_quality_metrics < "2019-02-01",
    latest_daily_traffic > "2022-12-01"
  )


## AADT to check coverage ----
aadt <- get_aadt_for_trp_list(trp_trs$trp_id)

aadt_tidy <-
  aadt |>
  dplyr::filter(
    year %in% c(2019, 2022)
  ) |>
  dplyr::select(
    trp_id, year, coverage
  )


## TRP ok ----
trp_trs_coverage <-
  trp_trs |>
  dplyr::left_join(
    aadt_tidy,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(
    coverage > 50
  ) |>
  dplyr::summarise(
    n_years = n(),
    .by = c(trp_id, name, road_reference)
  ) |>
  dplyr::filter(
    n_years == 2
  ) |>
  dplyr::arrange(
    road_reference
  )


## TRP index ----

tictoc::tic()
trp_index_data <-
  calculate_trp_index(
    "tromso",
    trp_trs_coverage$trp_id[15],
    2022,
    2023
  )
tictoc::toc()


## City index ----
all_trp_index_data <-
  base::list.files(path = "trp_index/tromso", full.names = TRUE) |>
  purrr::map(~ readr::read_rds(.x)) |>
  purrr::list_rbind() |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_months = n(),
    .by = c(trp_id)
  ) |>
  dplyr::filter(
    n_months >= 6
  ) |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
    weight = traffic_base / sum(traffic_base),
    city_index = (sum(traffic_calc) / sum(traffic_base) - 1 ) * 100,
    deviation = weight * (index_p - city_index)^2
  )


trp_index_meta_data <-
  all_trp_index_data |>
  dplyr::left_join(
    trp,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    index_p
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    name
  )

writexl::write_xlsx(
  trp_index_meta_data,
  "spesialuttak/trp_index_tromso.xlsx"
)

city_index_tromso <-
  all_trp_index_data |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_trp = n(),
    sum_squared_weight = sum(weight^2),
    n_eff = 1 / sum_squared_weight,
    variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation)
  ) |>
  dplyr::mutate(
    period = "2019-2022",
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
    standard_error = sqrt(sum_squared_weight * variance_p),
    ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error, 1),
    ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error, 1)
  ) |>
  dplyr::select(
    period,
    index_p,
    ci_lower,
    ci_upper,
    n_trp,
    n_eff,
    variance_p,
    sum_squared_weight,
    standard_error
  )

readr::write_rds(
  city_index_tromso,
  "data_indexpoints_tidy/city_index_tromso_2019_2022.rds"
)


# Test Dramsvegen ----
dramsvegen_ht_base_year <-
  get_hourly_traffic_by_length(
    "30868V1109333",
    paste0(as.character(2022), "-05-01T00:00:00.000+02:00"),
    paste0(as.character(2022), "-06-01T00:00:00.000+02:00")
  ) |>
  calculate_hourly_index_traffic()

dramsvegen_dt_base_year <-
  get_dt_by_length_for_trp(
    "30868V1109333",
    paste0(as.character(2022), "-01-01T00:00:00.000+01:00"),
    paste0(as.character(2022 + 1), "-01-01T00:00:00.000+01:00")
  ) |>
  dplyr::filter(
    length_range == "[..,5.6)"
  )


dramsvegen_ht_calc_year <-
  get_hourly_traffic_by_length(
    "30868V1109333",
    paste0(as.character(2023), "-05-01T00:00:00.000+02:00"),
    paste0(as.character(2023), "-06-01T00:00:00.000+02:00")
  ) |>
  calculate_hourly_index_traffic()

dramsvegen_dt_calc_year <-
  get_dt_by_length_for_trp(
    "30868V1109333",
    paste0(as.character(2023), "-01-01T00:00:00.000+01:00"),
    paste0(as.character(2023 + 1), "-01-01T00:00:00.000+01:00")
  ) |>
  dplyr::filter(
    length_range == "[..,5.6)"
  )


dramsvegen_index_data <-
  dplyr::inner_join(
    dramsvegen_ht_base_year,
    dramsvegen_ht_calc_year,
    by = join_by(month, day, hour),
    suffix = c("_base", "_calc")
  )


tictoc::tic()
trp_index_data <-
  calculate_trp_index(
    "test",
    "30868V1109333",
    2022,
    2023
  )
tictoc::toc()

dramsvegen <-
  readr::read_rds(
    file = paste0("trp_index/test/30868V1109333_2022_2023.rds")
  ) |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1)
  )


# Nord-Jæren 2017-2019 direct ----
trps_aadt_in_period_tidy <-
  readr::read_rds("nj_trp_aadt.rds") |>
  dplyr::select(
    trp_id,
    year,
    good_enough
  ) |>
  dplyr::filter(
    good_enough == TRUE
  )

trps_good_enough_both_years <-
  dplyr::inner_join(
    trps_aadt_in_period_tidy |>
      dplyr::filter(
        year == 2017
      ),
    trps_aadt_in_period_tidy |>
      dplyr::filter(
        year == 2019
      ),
    by = join_by(trp_id)
  ) |>
  dplyr::left_join(
    trps_filtered,
    by = join_by(trp_id)
  )

trps_not_eligible_2019_due_to_new_roads <-
  c(
    "43296V319721", # Åsedalen: ny arm mellom E39 og Hoveveien
    "54577V319746" # Hillevågstunnelen
  )

# I tillegg kan Auglend være med (Nortraf jan-feb 2017)

trp_2017_2019 <-
  trps_good_enough_both_years |>
  dplyr::filter(
    !(trp_id %in% trps_not_eligible_2019_due_to_new_roads)
  )

{
  tictoc::tic()
  trp_index_data <-
    calculate_trp_index(
      "njaeren_2017_2019",
      trp_2017_2019$trp_id[18],
      2017,
      2019
    )
  tictoc::toc()
}

all_trp_index_data <-
  base::list.files(path = "trp_index/njaeren_2017_2019", full.names = TRUE) |>
  purrr::map(~ readr::read_rds(.x)) |>
  purrr::list_rbind() |>
  # In 2017 and 2019, Easter was entirely in April.
  # dplyr::filter(
  #   !(trp_id == "99781V2303021" & month == 4),
  #   !(trp_id == "36178V320198" & month == 4)
  # ) |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_months = n(),
    .by = c(trp_id)
  ) |>
  dplyr::filter(
    n_months >= 6
  ) |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(2),
    weight = traffic_base / sum(traffic_base),
    city_index = (sum(traffic_calc) / sum(traffic_base) - 1 ) * 100,
    deviation = weight * (index_p - city_index)^2,
    index_i = traffic_calc / traffic_base,
    city_index_i = sum(traffic_calc) / sum(traffic_base),
    deviation_i = weight * (index_i - city_index_i)^2
  )

trp_index_meta_data <-
  all_trp_index_data |>
  dplyr::left_join(
    trp,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    index_p
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    name
  )

city_index_njaeren_2017_2019 <-
  all_trp_index_data |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_trp = n(),
    sum_squared_weight = sum(weight^2),
    n_eff = 1 / sum_squared_weight,
    variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
    variance_i = (1 / (1 - sum_squared_weight)) * sum(deviation_i)
  ) |>
  dplyr::mutate(
    period = "2017-2019",
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
    index_i = traffic_calc / traffic_base,
    standard_error = sqrt(sum_squared_weight * variance_p),
    standard_error_i = sqrt(sum_squared_weight * variance_i),
    ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error, 1),
    ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error, 1)
  ) |>
  dplyr::select(
    period,
    index_i,
    index_p,
    ci_lower,
    ci_upper,
    n_trp,
    n_eff,
    variance_p,
    variance_i,
    sum_squared_weight,
    standard_error,
    standard_error_i
  )

readr::write_rds(
  city_index_njaeren_2017_2019,
  "trp_index/city_index_njaeren_2017_2019.rds"
)

# Resultat begrenset til 24 opprinnelige punkt
# Direkte indeks: -3,5 (standarfeil 3,1)
# Opprinnelig kjedet indeks: -4,6 (standarfeil 2,1)
# Forskjellen kommer nok av at Hillevågtunnelen ikke er med i den direkte pga NorTraf-data.
# Den hadde cirka -10.
# I den opprinnelige kjedete indeksen var Eikeberget ikke med 18-19.
# Den skulle ha gitt et lite positivt bidrag.


# Nord-Jæren 2019-2023 ----
trps_aadt_in_period_tidy <-
  readr::read_rds("nj_trp_aadt.rds") |>
  dplyr::select(
    trp_id,
    year,
    good_enough
  ) |>
  dplyr::filter(
    good_enough == TRUE
  )

trps_good_enough_both_years <-
  dplyr::inner_join(
    trps_aadt_in_period_tidy |>
      dplyr::filter(
        year == 2019
      ),
    trps_aadt_in_period_tidy |>
      dplyr::filter(
        year == 2023
      ),
    by = join_by(trp_id)
  ) |>
  dplyr::left_join(
    trps_filtered,
    by = join_by(trp_id)
  )

trps_not_eligible_2019_2023_due_to_new_roads <-
  c(
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
    "32842V319521"  # Mosheim
  )

trp_2019_2023 <-
  trps_good_enough_both_years |>
  dplyr::filter(
    !(trp_id %in% trps_not_eligible_2019_2023_due_to_new_roads)
  )

trp_names <-
  trp_2019_2023 |>
  dplyr::select(
    trp_id,
    name,
    municipality_name
  )


## 2019-2023 sliding ----
reference_year <- 2019

mdt <-
  purrr::map_dfr(
    c(2019, 2020, 2021, 2022, 2023),
    ~ get_mdt_by_length_for_trp_list(trp_2019_2023$trp_id, .x)
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
  dplyr::left_join(
    trp_names,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    mdt = mdt_length_range,
    coverage,
    length_quality,
    #sub_area = municipality_name
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

last_year_month <- lubridate::as_date("2023-12-01")

source("exclude_trp_mdts_list.R")

trp_mdt_ok_refyear <-
  mdt_validated |>
  filter_mdt(reference_year) |>
  purrr::pluck(1)

source("mdt_check.R")

plot_mdt_comparisons |>
  plotly::ggplotly()

## All possible window indices
all_12_month_indices <-
  calculate_rolling_indices(12)

all_24_month_indices <-
  calculate_rolling_indices(24)

all_36_month_indices <-
  calculate_rolling_indices(36)

list(
  all_12_month_indices,
  all_24_month_indices,
  all_36_month_indices
) |>
  readr::write_rds("trp_index/njaeren_rolling_indices_2019_2023.rds")


## 2019-2023 direct ----
{
  tictoc::tic()
  trp_index_data <-
    calculate_trp_index(
      "njaeren_2019_2023",
      trp_2019_2023$trp_id[48],
      2019,
      2023
    )
  tictoc::toc()
}

all_trp_index_data <-
  base::list.files(path = "trp_index/njaeren_2019_2023", full.names = TRUE) |>
  purrr::map(~ readr::read_rds(.x)) |>
  purrr::list_rbind() |>
  # Easter in April both years.
  #dplyr::filter(
    #!(trp_id == "36178V320198" & month == 4)
  #) |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_months = n(),
    .by = c(trp_id)
  ) |>
  dplyr::filter(
    n_months >= 6
  ) |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(2),
    weight = traffic_base / sum(traffic_base),
    city_index = (sum(traffic_calc) / sum(traffic_base) - 1 ) * 100,
    deviation = weight * (index_p - city_index)^2,
    index_i = traffic_calc / traffic_base,
    city_index_i = sum(traffic_calc) / sum(traffic_base),
    deviation_i = weight * (index_i - city_index_i)^2
  )

trp_index_meta_data <-
  all_trp_index_data |>
  dplyr::left_join(
    trp,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    index_p
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    index_p
  )

city_index_njaeren_2019_2023 <-
  all_trp_index_data |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_trp = n(),
    sum_squared_weight = sum(weight^2),
    n_eff = 1 / sum_squared_weight,
    variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
    variance_i = (1 / (1 - sum_squared_weight)) * sum(deviation_i)
  ) |>
  dplyr::mutate(
    period = "2019-2023",
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
    index_i = traffic_calc / traffic_base,
    standard_error = sqrt(sum_squared_weight * variance_p),
    standard_error_i = sqrt(sum_squared_weight * variance_i),
    ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error, 1),
    ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error, 1)
  ) |>
  dplyr::select(
    period,
    index_i,
    index_p,
    ci_lower,
    ci_upper,
    n_trp,
    n_eff,
    variance_p,
    variance_i,
    sum_squared_weight,
    standard_error,
    standard_error_i
  )

readr::write_rds(
  city_index_njaeren_2019_2023,
  "trp_index/city_index_njaeren_2019_2023.rds"
)


# Nord-Jæren 2017-2023 chained ----
## 2017-2023 chain with sliding ----
chain_start_year_from_to <- "2017-2019"

index_2017_2019 <-
  readr::read_rds("trp_index/city_index_njaeren_2017_2019.rds") |>
  dplyr::mutate(
    index_type = "direct",
    months = "jan-des",
    year_base = 2017,
    year = 2019,
    month = 12
  )

chain_link_se_p <- index_2017_2019$standard_error
chain_link_index_i <- index_2017_2019$index_i

sliding_index_2019_2023 <-
  readr::read_rds("trp_index/njaeren_rolling_indices_2019_2023.rds") |>
  dplyr::bind_rows() |>
  # If review
  dplyr::filter(
    month_object == "2023-12-01"
  )

index_jaeren_2017_2019_2023_direct_sliding_chained <-
  sliding_index_2019_2023 |>
  dplyr::select(
    index_period,
    month_object,
    month_n,
    year,
    window,
    n_trp,
    index_i,
    standard_error_p
  ) |>
  dplyr::mutate(
    index_period =
      paste0(
        stringr::str_sub(chain_start_year_from_to, 1, 4),
        stringr::str_sub(index_period, 5, -1)
      ),
    chained_index_i = index_i * chain_link_index_i,
    index_p = (chained_index_i - 1) * 100,
    standard_error =
      100 * sqrt(
        index_i^2 * 1e-4 * chain_link_se_p^2 +
          chain_link_index_i^2 * 1e-4 * standard_error_p^2 +
          1e-4 * chain_link_se_p^2 * 1e-4 * standard_error_p^2
      ),
    ci_lower = round(index_p - 1.96 * standard_error, 1),
    ci_upper = round(index_p + 1.96 * standard_error, 1)
  ) |>
  dplyr::select(
    index_period,
    #month_object,
    month = month_n,
    year,
    index_type = window,
    n_trp,
    index_i = chained_index_i,
    index_p,
    standard_error,
    ci_lower,
    ci_upper
  )

index_jaeren_2017_2019_2023_direct_sliding_all <-
  dplyr::bind_rows(
    index_2017_2019 |>
      dplyr::select(
        #year_base,
        year,
        month,
        index_period = period,
        index_type,
        n_trp,
        index_i,
        index_p,
        standard_error,
        ci_lower,
        ci_upper
      ) |>
      dplyr::mutate(
        alternative = "A2_12_months"
      ),
    index_2017_2019 |>
      dplyr::select(
        #year_base,
        year,
        month,
        index_period = period,
        index_type,
        n_trp,
        index_i,
        index_p,
        standard_error,
        ci_lower,
        ci_upper
      ) |>
      dplyr::mutate(
        alternative = "A2_24_months"
      ),
    index_2017_2019 |>
      dplyr::select(
        #year_base,
        year,
        month,
        index_period = period,
        index_type,
        n_trp,
        index_i,
        index_p,
        standard_error,
        ci_lower,
        ci_upper
      ) |>
      dplyr::mutate(
        alternative = "A2_36_months"
      ),
    sliding_index_2019_2023 |>
      dplyr::select(
        year,
        month = month_n,
        index_period = index_period,
        index_type = window,
        n_trp,
        index_i,
        index_p,
        standard_error = standard_error_p,
        ci_lower,
        ci_upper
      ) |>
      dplyr::mutate(
        alternative = paste0("A2_", index_type)
      ),
    index_jaeren_2017_2019_2023_direct_sliding_chained |>
      dplyr::mutate(
        alternative = paste0("A2_", index_type)
      )
  )

# Official results
readr::write_rds(
  index_jaeren_2017_2019_2023_direct_sliding_all,
  "trp_index/index_jaeren_direct_2017_2019_sliding.rds"
)

writexl::write_xlsx(
  index_jaeren_2017_2019_2023_direct_sliding_all,
  path = paste0("data_indexpoints_tidy/rolling_indices_952.xlsx")
)

# For TRP review
readr::write_rds(
  index_jaeren_2017_2019_2023_direct_sliding_all,
  "trp_index/index_jaeren_2017_2019_2023_direct_sliding_all.rds"
)


## 2017-2023 chain with directs ----
index_jaeren_2017_2019_2023_direct <-
  dplyr::bind_rows(
    readr::read_rds("trp_index/city_index_njaeren_2017_2019.rds"),
    readr::read_rds("trp_index/city_index_njaeren_2019_2023.rds")
  ) |>
  dplyr::mutate(
    year_base = stringr::str_sub(period, 1, 4),
    year = stringr::str_sub(period, 6, 9),
    month = 12
  )

chained_17_23 <-
  calculate_two_year_index(index_jaeren_2017_2019_2023_direct) |>
  dplyr::mutate(
    ci_lower = round(index_p - 1.96 * standard_error, 1),
    ci_upper = round(index_p + 1.96 * standard_error, 1)
  )

index_jaeren_2017_2019_2023_direct_all <-
  index_jaeren_2017_2019_2023_direct |>
  dplyr::mutate(
    index_type = "direct"
  ) |>
  dplyr::bind_rows(
    chained_17_23
  ) |>
  dplyr::mutate(
    year_from_to = paste0(year_base, "-", year),
    month_name_short = lubridate::month(month, label = TRUE),
    period = paste0("jan-", month_name_short)
  ) |>
  dplyr::select(
    -sum_squared_weight,
    -variance_i,
    -variance_p,
    -n_eff,
    -standard_error_i
  )

readr::write_rds(
  index_jaeren_2017_2019_2023_direct_all,
  "trp_index/city_index_njaeren_2017_2019_2023_direct.rds"
)

# Nord-Jæren 2017-2023 direct ----
# Chosen TRPs good enough in 2023

trps_aadt_in_period_tidy <-
  readr::read_rds("nj_trp_aadt.rds") |>
  dplyr::select(
    trp_id,
    year,
    good_enough
  ) |>
  dplyr::filter(
    good_enough == TRUE
  )

trps_good_enough_both_years <-
  dplyr::inner_join(
    trps_aadt_in_period_tidy |>
      dplyr::filter(
        year == 2017
      ),
    trps_aadt_in_period_tidy |>
      dplyr::filter(
        year == 2023
      ),
    by = join_by(trp_id)
  ) |>
  dplyr::left_join(
    trps_filtered,
    by = join_by(trp_id)
  )

trps_not_eligible_2023_due_to_new_roads <-
  c(
    "17949V320695", # Bybrua sør
    "10795V320297", # Randabergveien
    "68351V319882", # Kannik
    "57279V320244", # Storhaugtunnelen
    "43296V319721", # Åsedalen: ny arm mellom E39 og Hoveveien
    "54577V319746", # Hillevågstunnelen
    "55507V319881", # Madlaveien Mosvatnet
    "71535V319524", # Lassa
    "83652V319725", # Strandgata nord
    "92102V319885", # Bergelandstunnelen
    "50749V319525"  # Byhaugtunnelen sør
  )

# I tillegg kan Auglend være med (Nortraf jan-feb 2017)

trp_2017_2023 <-
  trps_good_enough_both_years |>
  dplyr::filter(
    !(trp_id %in% trps_not_eligible_2023_due_to_new_roads)
  )

{
  tictoc::tic()
  trp_index_data <-
    calculate_trp_index(
      "trp_index/njaeren_2017_2023",
      trp_2017_2023$trp_id[20],
      2017,
      2023
    )
  tictoc::toc()
}

all_trp_index_data <-
  base::list.files(path = "trp_index/njaeren_2017_2023", full.names = TRUE) |>
  purrr::map(~ readr::read_rds(.x)) |>
  purrr::list_rbind() |>
  dplyr::filter(
   !(trp_id == "36178V320198" & month == 4)
  ) |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_months = n(),
    .by = c(trp_id)
  ) |>
  dplyr::filter(
    n_months >= 6
  ) |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(2),
    weight = traffic_base / sum(traffic_base),
    city_index = (sum(traffic_calc) / sum(traffic_base) - 1 ) * 100,
    deviation = weight * (index_p - city_index)^2,
    index_i = traffic_calc / traffic_base,
    city_index_i = sum(traffic_calc) / sum(traffic_base),
    deviation_i = weight * (index_i - city_index_i)^2
  )

trp_index_meta_data <-
  all_trp_index_data |>
  dplyr::left_join(
    trp,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    index_p
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    trp_id
  )

city_index_njaeren_2017_2023 <-
  all_trp_index_data |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_trp = n(),
    sum_squared_weight = sum(weight^2),
    n_eff = 1 / sum_squared_weight,
    variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
    variance_i = (1 / (1 - sum_squared_weight)) * sum(deviation_i)
  ) |>
  dplyr::mutate(
    period = "2017-2023",
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
    index_i = traffic_calc / traffic_base,
    standard_error = sqrt(sum_squared_weight * variance_p),
    standard_error_i = sqrt(sum_squared_weight * variance_i),
    ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error, 1),
    ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error, 1)
  ) |>
  dplyr::select(
    period,
    index_i,
    index_p,
    ci_lower,
    ci_upper,
    n_trp,
    n_eff,
    variance_p,
    variance_i,
    sum_squared_weight,
    standard_error,
    standard_error_i
  )

readr::write_rds(
  city_index_njaeren_2017_2023,
  "trp_index/city_index_njaeren_2017_2023.rds"
)


# TRD direct ----
# Need
# 1. 2014-2019 (no toll data, mostly NorTraf -> use DT?)
# 2. 2019-2023

## TRP ----
trd_trp <- get_published_pointindex_for_months_trondheim(960, 2023, 12)[[1]]

# As no toll station data is available from 2014, might add some TRPs at same place
trd_trp_add <- c(
  "32375V72155", # Væretunnelen
  "36935V72359"  # Selsbakk
)

## 2014-2019 ----
trd_trp_meta <-
  trp |>
  dplyr::filter(
    trp_id %in% c(trd_trp, trd_trp_add)
  ) |>
  dplyr::select(
    trp_id, name, road_reference
  ) |>
  dplyr::left_join(
    trp_time_span,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(
    first_data < "2014-08-01"
  ) |>
  dplyr::arrange(
    first_data
  )

{
  tictoc::tic()
  trp_index_data <-
    calculate_trp_index_without_quality(
      "trd_2014_2019",
      trd_trp_meta$trp_id[4],
      2014,
      2019
    )
  tictoc::toc()
}

all_trp_index_data_14_19 <-
  base::list.files(path = "trp_index/trd_2014_2019", full.names = TRUE) |>
  purrr::map(~ readr::read_rds(.x)) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(2)
  ) |>
  dplyr::left_join(
    trp,
    by = join_by(trp_id)
  ) |>
  # Need to exclude som TRPs from before Strindheimtunnelen opened.
  dplyr::filter(
    !(trp_id == "81077V72158" & month < 7), # Havnegata
    !(trp_id == "10236V72161" & month < 7), # Bakke kirke
    !(trp_id == "88356V72157" & month < 7), # Jernbanebrua
    !(trp_id == "21801V72158" & month < 7), # Brattørbrua
    !(trp_id == "65625V41945") # Kong Øysteins veg
  ) |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_months = n(),
    .by = c(trp_id)
  ) |>
  dplyr::filter(
    n_months >= 5
  ) |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(2),
    weight = traffic_base / sum(traffic_base),
    city_index = (sum(traffic_calc) / sum(traffic_base) - 1 ) * 100,
    deviation = weight * (index_p - city_index)^2,
    index_i = traffic_calc / traffic_base,
    city_index_i = sum(traffic_calc) / sum(traffic_base),
    deviation_i = weight * (index_i - city_index_i)^2
  )

trp_index_meta_data <-
  all_trp_index_data |>
  dplyr::left_join(
    trp,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    index_p
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    trp_id
  )

city_index_trd_2014_2019 <-
  all_trp_index_data_14_19 |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_trp = n(),
    sum_squared_weight = sum(weight^2),
    n_eff = 1 / sum_squared_weight,
    variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
    variance_i = (1 / (1 - sum_squared_weight)) * sum(deviation_i)
  ) |>
  dplyr::mutate(
    period = "2014-2019",
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
    index_i = traffic_calc / traffic_base,
    standard_error = sqrt(sum_squared_weight * variance_p),
    standard_error_i = sqrt(sum_squared_weight * variance_i),
    ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error, 1),
    ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error, 1)
  ) |>
  dplyr::select(
    period,
    index_i,
    index_p,
    ci_lower,
    ci_upper,
    n_trp,
    n_eff,
    variance_p,
    variance_i,
    sum_squared_weight,
    standard_error,
    standard_error_i
  )

readr::write_rds(
  city_index_trd_2014_2019,
  "trp_index/city_index_trd_2014_2019.rds"
)


## 2014-2023 ----
# Chaining
# Choosing to use official 2019-2023 chained index
trd_19_23 <-
  readr::read_rds(
    file = paste0("data_indexpoints_tidy/byindeks_960.rds")
  ) |>
  dplyr::filter(
    year == 2023,
    index_type == "chained"
  )

trd_2014_2019_2023 <-
  dplyr::bind_rows(
    city_index_trd_2014_2019 |>
      dplyr::mutate(
        year_base = stringr::str_sub(period, 1, 4) |> as.numeric(),
        year = stringr::str_sub(period, 6, 9) |> as.numeric(),
        month = 12
      ),
    trd_19_23
  ) |>
  calculate_two_year_index() |>
  dplyr::mutate(
    ci_lower = round(index_p - 1.96 * standard_error, 1),
    ci_upper = round(index_p + 1.96 * standard_error, 1)
  )


## 2019-2023 direct special ----
# Just nov and dec to avoid pandemic and Nydalsbrua work

### Toll stations ----
toll_direct <-
  readr::read_rds(
    file = "H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser_direkte.rds",
  ) |>
  dplyr::filter(
    class == "lette"
  ) |>
  dplyr::mutate(
    years = paste0("2019-", lubridate::year(month_calc)),
    month = lubridate::month(month_calc),
    type = "Bom"
  ) |>
  dplyr::select(
    trp_id,
    years,
    month,
    traffic_base = monthly_volume_base,
    traffic_calc = monthly_volume_calc,
    n_days,
    index_p,
    type
  )


### TRPs ----
trd_trp_meta_19_23 <-
  trp |>
  dplyr::filter(
    trp_id %in% trd_trp
  ) |>
  dplyr::select(
    trp_id, name, road_reference
  ) |>
  dplyr::left_join(
    trp_time_span,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(
    first_data < "2019-01-01"
  ) |>
  dplyr::arrange(
    trp_id
  )

{
  tictoc::tic()
  trp_index_data <-
    calculate_trp_index(
      "trd_2019_2023",
      trd_trp_meta_19_23$trp_id[1], # ikke 14, 19, 20, 24, 26, 27
      2019,
      2023
    )
  tictoc::toc()
}


### Toll and TRP ----
all_trp_index_data_19_23 <-
  base::list.files(path = "trp_index/trd_2019_2023", full.names = TRUE) |>
  purrr::map(~ readr::read_rds(.x)) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(2),
    type = "TRP"
  ) |>
  dplyr::left_join(
    trp,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
   trp_id,
   name,
   years,
   month,
   traffic_base,
   traffic_calc,
   n_days,
   index_p,
   type
  ) |>
  dplyr::bind_rows(
    toll_direct |> dplyr::filter(years == "2019-2023")
  ) |>
  dplyr::filter(
    month %in% c(11, 12)
  #   !(trp_id == "81077V72158" & month < 7), # Havnegata
  #   !(trp_id == "10236V72161" & month < 7), # Bakke kirke
  #   !(trp_id == "88356V72157" & month < 7), # Jernbanebrua
  #   !(trp_id == "21801V72158" & month < 7), # Brattørbrua
  #   !(trp_id == "65625V41945") # Kong Øysteins veg
  ) |>
  # Stop to check data quality
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_months = n(),
    .by = c(trp_id, type)
  ) |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(2),
    weight = traffic_base / sum(traffic_base),
    city_index = (sum(traffic_calc) / sum(traffic_base) - 1 ) * 100,
    deviation = weight * (index_p - city_index)^2,
    index_i = traffic_calc / traffic_base,
    city_index_i = sum(traffic_calc) / sum(traffic_base),
    deviation_i = weight * (index_i - city_index_i)^2
  )

trp_index_meta_data_19_23 <-
  all_trp_index_data_19_23 |>
  dplyr::left_join(
    trp,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    index_p
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    trp_id
  )


### City index ----
city_index_trd_2019_2023 <-
  all_trp_index_data_19_23 |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_trp = n(),
    sum_squared_weight = sum(weight^2),
    n_eff = 1 / sum_squared_weight,
    variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
    variance_i = (1 / (1 - sum_squared_weight)) * sum(deviation_i),
    #.by = type
  ) |>
  dplyr::mutate(
    period = "2019-2023",
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
    index_i = traffic_calc / traffic_base,
    standard_error = sqrt(sum_squared_weight * variance_p),
    standard_error_i = sqrt(sum_squared_weight * variance_i),
    ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error, 1),
    ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error, 1)
  ) |>
  dplyr::select(
    #type,
    period,
    index_i,
    index_p,
    ci_lower,
    ci_upper,
    n_trp,
    n_eff,
    variance_p,
    variance_i,
    sum_squared_weight,
    standard_error,
    standard_error_i
  )

readr::write_rds(
  city_index_trd_2019_2023,
  "trp_index/city_index_trd_2019_2023.rds"
)


## 2019-2024 direct special ----
# Just Jan and Feb to avoid pandemic and Nydalsbrua work
{
  tictoc::tic()
  trp_index_data <-
    calculate_trp_index(
      "trd_2019_2024",
      trd_trp_meta_19_23$trp_id[13], # ikke 2, 5-8, 14, 18, 19, 20, 24, 26, 27
      2019,
      2024
    )
  tictoc::toc()
}


### Toll and TRP ----
all_trp_index_data_19_24 <-
  base::list.files(path = "trp_index/trd_2019_2024", full.names = TRUE) |>
  purrr::map(~ readr::read_rds(.x)) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(2),
    type = "TRP"
  ) |>
  dplyr::left_join(
    trp,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    name,
    years,
    month,
    traffic_base,
    traffic_calc,
    n_days,
    index_p,
    type
  ) |>
  dplyr::bind_rows(
    toll_direct |> dplyr::filter(years == "2019-2024")
  ) |>
  dplyr::filter(
    month %in% c(1, 2)
  ) |>
  # Stop to check data quality
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_months = n(),
    .by = c(trp_id, type)
  ) |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(2),
    weight = traffic_base / sum(traffic_base),
    city_index = (sum(traffic_calc) / sum(traffic_base) - 1 ) * 100,
    deviation = weight * (index_p - city_index)^2,
    index_i = traffic_calc / traffic_base,
    city_index_i = sum(traffic_calc) / sum(traffic_base),
    deviation_i = weight * (index_i - city_index_i)^2
  )


### City index ----
city_index_trd_2019_2024 <-
  all_trp_index_data_19_24 |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_trp = n(),
    sum_squared_weight = sum(weight^2),
    n_eff = 1 / sum_squared_weight,
    variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
    variance_i = (1 / (1 - sum_squared_weight)) * sum(deviation_i),
    .by = type
  ) |>
  dplyr::mutate(
    period = "2019-2024",
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
    index_i = traffic_calc / traffic_base,
    standard_error = sqrt(sum_squared_weight * variance_p),
    standard_error_i = sqrt(sum_squared_weight * variance_i),
    ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error, 1),
    ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error, 1)
  ) |>
  dplyr::select(
    type,
    period,
    index_i,
    index_p,
    ci_lower,
    ci_upper,
    n_trp,
    n_eff,
    variance_p,
    variance_i,
    sum_squared_weight,
    standard_error,
    standard_error_i
  )

readr::write_rds(
  city_index_trd_2019_2024,
  "trp_index/city_index_trd_2019_2024.rds"
)


