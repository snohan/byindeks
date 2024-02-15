# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
  library(tictoc)
  library(writexl)
  library(sf)
}


# Functions ----
calculate_hourly_index_traffic <- function(traffic_by_length) {

  # IN: hourly traffic by length and lane
  # OUT: hourly traffic when all lanes present

  data_tidy <-
    traffic_by_length |>
    tibble::as_tibble() |>
    dplyr::mutate(
      length_quality = round(length_quality),
      total_coverage = round(total_coverage)
    ) |>
    dplyr::filter(
      length_range == "[..,5.6)",
      length_quality >= 95, # TODO: remove this filter
      # can't remove single hours based on this. lenght diff is based on daily values in order to avoid
      # nightly hours with little traffic be filtered too often
      # same reasoning behind filtering by total coverage and not length coverage here
      total_coverage >= 99
    )

  # lanes_each_month <-
  #   data_tidy |>
  #   dplyr::mutate(
  #     month = lubridate::month(from)
  #   ) |>
  #   dplyr::select(
  #     lane,
  #     month
  #   ) |>
  #   dplyr::distinct() |>
  #   dplyr::summarise(
  #     lanes_month = base::paste(lane, collapse = "#"),
  #     .by = month
  #   )

  hourly_traffic <-
    data_tidy |>
    dplyr::select(
      from,
      traffic,
      #lane
    ) |>
    #dplyr::summarise(
    #  lanes_hour = base::paste(lane, collapse = "#"),
    #  traffic = sum(traffic),
    #  .by = from
    #) |>
    dplyr::mutate(
      month = lubridate::month(from)
    ) |>
    #dplyr::left_join(
    #  lanes_each_month,
    #  by = join_by(month)
    #) |>
    #dplyr::filter(
    #  lanes_hour == lanes_month,
    #) |>
    dplyr::mutate(
      day = lubridate::day(from),
      hour = lubridate::hour(from)
    ) |>
    dplyr::select(
      month,
      day,
      hour,
      #lanes_hour,
      traffic
    ) |>
    # Adding the two hours when DST is set back in October
    dplyr::summarise(
      traffic = sum(traffic),
      .by = c(tidyselect::everything(), -traffic)
    )

  return(hourly_traffic)
}

# trp_id <- trp_2017[16]
# trp_id <- "30868V1109333"
# calc_year <- 2023
# base_year <- 2022


calculate_trp_index <- function(subfolder_name, trp_id, base_year, calc_year) {

  ht_base_year <-
    get_hourly_traffic_by_length(
      trp_id,
      paste0(as.character(base_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(base_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    calculate_hourly_index_traffic()

  dt_base_year <-
    get_dt_by_length_for_trp(
      trp_id,
      paste0(as.character(base_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(base_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    dplyr::select(
      from,
      length_quality_base = length_quality
      # yes, must use length_quality and not length_coverage,
      # e.g. if just 16 hours for a day, length_quality might be high, but length_coverage will be low
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      month = lubridate::month(from),
      day = lubridate::mday(from),
      length_quality_base = round(length_quality_base)
    ) |>
    dplyr::select(
      -from
    )

  ht_calc_year <-
    get_hourly_traffic_by_length(
      trp_id,
      paste0(as.character(calc_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(calc_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    calculate_hourly_index_traffic()

  dt_calc_year <-
    get_dt_by_length_for_trp(
      trp_id,
      paste0(as.character(calc_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(calc_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    dplyr::select(
      from,
      length_quality_calc = length_quality
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      month = lubridate::month(from),
      day = lubridate::mday(from),
      length_quality_calc = round(length_quality_calc)
    ) |>
    dplyr::select(
      -from
    )

  trp_index_data <-
    dplyr::inner_join(
      ht_base_year,
      ht_calc_year,
      by = join_by(month, day, hour),
      #by = join_by(month, day, hour, lanes_hour),
      suffix = c("_base", "_calc")
    ) |>
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_hours = n(),
      .by = c(month, day)
    ) |>
    # remove days with less than 95 % length quality
    dplyr::left_join(
      dt_base_year,
      by = join_by(month, day)
    ) |>
    dplyr::left_join(
      dt_calc_year,
      by = join_by(month, day)
    ) |>
    dplyr::mutate(
      ok_length =
        length_quality_calc >= 95 & length_quality_base >= 95
    ) |>
    dplyr::filter(
      n_hours >= 16,
      ok_length == TRUE
    ) |>
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_days = n(),
      .by = c(month)
    ) |>
    dplyr::filter(
      n_days >= 16
    ) |>
    dplyr::mutate(
      trp_id = trp_id,
      years = paste0(as.character(base_year), "-", as.character(calc_year))
    )

  readr::write_rds(
    trp_index_data,
    file = paste0("trp_index/", subfolder_name, "/", trp_id, "_", base_year, "_", calc_year, ".rds")
  )

  return(trp_index_data)
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


# Nord-Jæren 2017 vs. 2019 ----
# Will reference year 2019 be better than 2017?
## Which TRPs were good in 2019 ----
trp_njaeren <-
  trp |>
  dplyr::filter(
    municipality_name %in% c("Randaberg", "Stavanger", "Sola", "Sandnes")
  ) |>
  dplyr::left_join(
    trp_time_span,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(
    first_data_with_quality_metrics < "2019-02-01",
    latest_daily_traffic > "2019-12-01"
  )


## AADT to check coverage ----
aadt <- get_aadt_for_trp_list(trp_njaeren$trp_id)

aadt_tidy <-
  aadt |>
  dplyr::filter(
    year %in% c(2019)
  ) |>
  dplyr::select(
    trp_id, year, coverage
  )


## 2017-2019 ----
# A direct index with the original 24 TRPs
# Does this direct index have lower uncertainty than the chained?
trp_2017 <- get_published_pointindex_for_months(952, 2020, 1)[[1]]

{
  tictoc::tic()
  trp_index_data <-
    calculate_trp_index(
      "njaeren",
      trp_2017[19],
      2017,
      2019
    )
  tictoc::toc()
}

# Utelatt:
# Åsedalen pga ny arm til E39 til Hoveveien
# Hillevågtunnelen pga NorTraf-data tom juli 2017
# Kannik: lite data i 2019

all_trp_index_data <-
  base::list.files(path = "trp_index/njaeren", full.names = TRUE) |>
  purrr::map(~ readr::read_rds(.x)) |>
  purrr::list_rbind() |>
  dplyr::filter(
    !(trp_id == "99781V2303021" & month == 4)
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

# Resultat
# Direkte indeks: -3,5 (standarfeil 3,1)
# Opprinnelig kjedet indeks: -4,6 (standarfeil 2,1)
# Forskjellen kommer nok av at Hillevågtunnelen ikke er med i den direkte pga NorTraf-data.
# Den hadde cirka -10.
# I den opprinnelige kjedete indeksen var Eikeberget ikke med 18-19.
# Den skulle ha gitt et lite positivt bidrag.


## 2019-2023 ----
# Three year sliding window
# Fetching the TRPs agreed on in the proposal to extend number of TRPs
reference_year <- 2019
trp_2019 <- get_published_pointindex_for_months(10952, 2021, 1)[[1]]

trp_2019_df <-
  tibble::as_tibble_col(
    trp_2019,
    column_name = "trp_id"
  )

# Some of these TRPs were not yet operational in 2019
trp_njaeren_used <-
  trp_njaeren |>
  dplyr::right_join(
    trp_2019_df,
    by = "trp_id"
  ) |>
  dplyr::filter(
    #trp_id %in% trp_2019
    is.na(name)
  ) |>
  dplyr::select(
    trp_id
  ) |>
  dplyr::left_join(
    trp,
    by = "trp_id"
  )

# Choosing agreed upon TRPs that were good in 2019
trp_2019_chosen <-
  trp_njaeren |>
  dplyr::filter(
    trp_id %in% trp_2019
  )


### Get MDTs ----
mdt <-
  purrr::map_dfr(
    c(2019, 2020, 2021, 2022, 2023),
    ~ get_mdt_by_length_for_trp_list(trp_2019_chosen$trp_id, .x)
  )

trp_mdt_ok_refyear <-
  mdt_validated |>
  # dplyr::filter(
  #   trp_id %in% city_trps # avoid toll stations appearing here as they've already been checked
  # ) |>
  filter_mdt(reference_year) |>
  purrr::pluck(1)



## 2017-2023 ----
# Combined index and uncertainty


# Nord-Jæren 2017-2023 ----
## 2017-2023 ----
# Chosen TRPs good enough in 2023

nj_2023_chosen<-
  readr::read_rds(
    "chosen_links_nj_2023.rds"
  )


# A direct index with the original 24 TRPs
# Does this direct index have lower uncertainty than the chained?
trp_2017 <- get_published_pointindex_for_months(952, 2020, 1)[[1]]

trps_not_eligible_2023_due_to_new_roads <-
  c(
    "17949V320695", # Bybrua sør
    "10795V320297", # Randabergveien
    "68351V319882", # Kannik
    "57279V320244", # Storhaugtunnelen
    "43296V319721", # Åsedalen: ny arm mellom E39 og Hoveveien
    "54577V319746"  # Hillevågtunnelen: NorTraf i 2017
  )

trp_2017_eligible <-
  trp_2017[!(trp_2017 %in% trps_not_eligible_2023_due_to_new_roads)]

trp_2017_2023 <-
  trp_2017_eligible[trp_2017_eligible %in% nj_2023_chosen$trp_id]

{
  tictoc::tic()
  trp_index_data <-
    calculate_trp_index(
      "njaeren_2023",
      trp_2017_2023[14],
      2017,
      2023
    )
  tictoc::toc()
}

all_trp_index_data <-
  base::list.files(path = "trp_index/njaeren_2023", full.names = TRUE) |>
  purrr::map(~ readr::read_rds(.x)) |>
  purrr::list_rbind() |>
  # dplyr::filter(
  #   !(trp_id == "99781V2303021" & month == 4)
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
