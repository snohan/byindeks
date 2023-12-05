source("rmd_setup.R")
source("indexpoints_tidying_functions.R")

# Traffic Data API calls to get points metadata and aadt
source("get_from_trafficdata_api.R")


# Through traffic ----
through_traffic <-
  readr::read_csv2("through_traffic/through_traffic.csv")

trp_and_route <-
  readr::read_csv2("through_traffic/trp_and_route.csv")

through_traffic_mdt_nedre_glomma <-
  readxl::read_excel(
    "through_traffic/mdt_gjennomgangstrafikk.xlsx",
    sheet = 1,
    skip = 1
  ) |>
  dplyr::mutate(
    area = "nedre_glomma"
  ) |>
  dplyr::select(
    area,
    year,
    month,
    rv110_n,
    e6_n,
    rv22_rv110_internal
  ) |>
  tidyr::pivot_longer(
    cols = c("rv110_n", "e6_n", "rv22_rv110_internal"),
    names_to = "route",
    values_to = "mdt_lmv"
  )

through_traffic_mdt_trondheim <-
  readxl::read_excel(
    "through_traffic/mdt_gjennomgangstrafikk.xlsx",
    sheet = 2,
    skip = 1
  ) |>
  dplyr::mutate(
    area = "trondheim"
  ) |>
  dplyr::select(
    area,
    year,
    month,
    e14_e,
    e6_internal,
    e39_v
  ) |>
  tidyr::pivot_longer(
    cols = c("e14_e", "e6_internal", "e39_v"),
    names_to = "route",
    values_to = "mdt_lmv"
  )

through_traffic_mdt <-
  dplyr::bind_rows(
    through_traffic_mdt_nedre_glomma,
    through_traffic_mdt_trondheim
  )

# Ser korrelasjon juni-sept på rutene E6 S - E14 og E6 N - E14.
# Rart at E6 N - E39 er høyere enn E6 S - E39,
# eller er det fordi mange som kommer sørfra egentlig kommer sør for Berkåk og kjører Orkdalen i stedet?


# Nedre Glomma ----
city_number <- 953
present_year <- 2023
index_month <- 10

#source("city_reference_year.R")
# Through traffic MDT available only from 2019
# Need to calculate rolling index from 2019 just for the purpose of comparing with and without through traffic
reference_year <- 2019

last_year_month <-
  lubridate::as_date(
    paste0(
      present_year,
      "-",
      index_month,
      "-01"
    )
  )

## Yearly inex ----
city_index <-
  get_published_index_for_months(
    953,
    2022,
    12
  ) |>
  dplyr::filter(
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    period == "year_to_date",
    month == 12
  )

pointindex <-
  get_published_pointindex_for_months(
    953,
    2022,
    12
  )

trp_indexes <-
  pointindex[[2]] |>
  dplyr::filter(
    period == "year_to_date",
    month == 12
  ) |>
  dplyr::select(
    area_name,
    trp_id,
    year,
    month,
    length_base_volume_short,
    length_calc_volume_short,
    #index_total_coverage,
    length_coverage,
    index_short
    #index_total_p
  )

trp_meta_data <-
  get_trp_metadata_by_list(trp_indexes$trp_id) |>
  dplyr::select(
    trp_id,
    name,
    from,
    to,
    road_reference,
    lat, lon
  ) |>
  dplyr::distinct() |>
  split_road_system_reference() |>
  dplyr::select(
    trp_id,
    name,
    from,
    to,
    road_category_and_number,
    lat, lon
  )


trp_and_through_traffic <-
  trp_meta_data |>
  dplyr::left_join(
    trp_and_route,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::left_join(
    trp_indexes,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::select(
    area_name,
    trp_id,
    name,
    #from,
    #to,
    road_category_and_number,
    lat, lon,
    route,
    area,
    year,
    month,
    length_base_volume_short,
    length_calc_volume_short,
    length_coverage,
    index_short
  ) |>
  dplyr::mutate(
    year_base = year - 1
  ) |>
  dplyr::left_join(
    through_traffic |>
      dplyr::select(
        area, route, year, aadt_lmv
      ),
    by = dplyr::join_by(
      area,
      route,
      year
    )
  ) |>
  dplyr::left_join(
    through_traffic |>
      dplyr::select(
        area, route, year, aadt_lmv
      ),
    by = dplyr::join_by(
      area,
      route,
      year_base == year
    ),
    suffix = c("_calc", "_base")
  ) |>
  dplyr::mutate(
    month_object = lubridate::as_date(paste0(year, "-", month, "-01")),
    length_base_volume_short_adjusted =
      dplyr::case_when(
        is.na(route) ~ length_base_volume_short,
        TRUE ~ length_base_volume_short - aadt_lmv_base * 365 * length_coverage / 100
      ),
    length_calc_volume_short_adjusted =
      dplyr::case_when(
        is.na(route) ~ length_calc_volume_short,
        TRUE ~ length_calc_volume_short - aadt_lmv_calc * 365 * length_coverage / 100
      ),
    index_short_adjusted = ((length_calc_volume_short_adjusted / length_base_volume_short_adjusted - 1) * 100) |> round(2),
    label_text =
      paste(
        name, "<br/>",
        road_category_and_number
      ) |> lapply(htmltools::HTML)
  )

readr::write_rds(
  trp_and_through_traffic,
  file = "through_traffic/trp_and_through_traffic.rds"
)


city_index_adjusted <-
  dplyr::bind_rows(
    trp_and_through_traffic |>
      dplyr::select(
        area_name,
        month_object,
        trp_id,
        length_base_volume_short,
        length_calc_volume_short,
        index_short
      ) |>
      dplyr::mutate(
        adjusted = FALSE
      ),
    trp_and_through_traffic |>
      dplyr::select(
        area_name,
        month_object,
        trp_id,
        length_base_volume_short = length_base_volume_short_adjusted,
        length_calc_volume_short = length_calc_volume_short_adjusted,
        index_short = index_short_adjusted
      ) |>
      dplyr::mutate(
        adjusted = TRUE
      )
  ) |>
  dplyr::group_by(
    area_name,
    month_object,
    adjusted
  ) |>
  dplyr::mutate(
    weight = (length_base_volume_short / sum(length_base_volume_short))
  ) |>
  dplyr::summarise(
    city_base_volume = sum(length_base_volume_short),
    city_calc_volume = sum(length_calc_volume_short),
    index_p = (city_calc_volume / city_base_volume - 1 ) * 100,
    n_trp = n(),
    standard_deviation = sqrt((1 / (1 - sum(weight^2) )) * sum(weight * (index_short - index_p)^2) ),
    standard_error = sqrt(sum(weight^2) * standard_deviation^2),
    ci_lower = round(index_p + stats::qt(0.025, n_trp) * standard_error, 1),
    ci_upper = round(index_p - stats::qt(0.025, n_trp) * standard_error, 1),
    .groups = "drop"
  )|>
  dplyr::select(
    -city_base_volume,
    -city_calc_volume
  )


readr::write_rds(
  city_index_adjusted,
  file = "through_traffic/city_index_adjusted.rds"
)


## Rolling index ----
mdt_filtered <-
  readr::read_rds(
    paste0(
      "data_indexpoints_tidy/mdt_",
      city_number,
      ".rds"
    )
  )

source("exclude_trp_mdts_list.R")


# Rolling index with through traffic
all_36_month_indices <-
  calculate_rolling_indices(36) |>
  dplyr::mutate(
    through_traffic = TRUE
  )

# Subtract through traffic by month by TRP in mdt_validated
mdt_validated_without_through_traffic <-
  mdt_validated |>
  dplyr::left_join(
    trp_and_route,
    by = join_by(trp_id)
  ) |>
  dplyr::left_join(
    through_traffic_mdt,
    by = join_by(area, route, year, month)
  ) |>
  dplyr::mutate(
    mdt_original = mdt,
    mdt_adjusted =
      dplyr::case_when(
        is.na(mdt_lmv) ~ mdt,
        TRUE ~ mdt - mdt_lmv
      )
  ) |>
  dplyr::select(-mdt) |>
  dplyr::rename(mdt = mdt_adjusted)


mdt_validated <- mdt_validated_without_through_traffic

all_36_month_indices_adjusted <-
  calculate_rolling_indices(36) |>
  dplyr::mutate(
    through_traffic = FALSE
  )

# Put together
dplyr::bind_rows(
  all_36_month_indices_adjusted,
  all_36_month_indices
) |>
  readr::write_rds(
    file =
      paste0(
        "data_indexpoints_tidy/rolling_indices_adjusted_",
        city_number,
        ".rds"
      )
  )


# Trondheim ----
city_number <- 960
present_year <- 2023
index_month <- 10

source("city_reference_year.R")

last_year_month <-
  lubridate::as_date(
    paste0(
      present_year,
      "-",
      index_month,
      "-01"
    )
  )


## Yearly index ----
trp_trd <-
  readr::read_rds(
    #"data_indexpoints_tidy/indekspunkt_960.rds"
    "index_trp_metadata/trp_960.rds"
  )

trd_trp_index <-
  readr::read_rds(
    "data_indexpoints_tidy/trp_index_960.rds"
  ) |>
  dplyr::filter(
    year == 2022
  )

trd_trp_index_tidy <-
  trp_trd |>
  dplyr::left_join(
    trp_and_route,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::left_join(
    trd_trp_index,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::select(
    #area_name,
    trp_id,
    name,
    #from,
    #to,
    road_category_and_number,
    lat, lon,
    route,
    area,
    year,
    month_object = month,
    length_base_volume_short = base_volume,
    length_calc_volume_short = calc_volume,
    length_coverage = coverage,
    index_short = index
  ) |>
  dplyr::mutate(
    year_base = year - 1,
    index_short = round(index_short, 2)
  ) |>
  dplyr::left_join(
    through_traffic |>
      dplyr::select(
        area, route, year, aadt_lmv
      ),
    by = dplyr::join_by(
      area,
      route,
      year
    )
  ) |>
  dplyr::left_join(
    through_traffic |>
      dplyr::select(
        area, route, year, aadt_lmv
      ),
    by = dplyr::join_by(
      area,
      route,
      year_base == year
    ),
    suffix = c("_calc", "_base")
  ) |>
  dplyr::mutate(
    #month_object = lubridate::as_date(paste0(year, "-", month, "-01")),
    length_base_volume_short_adjusted =
      dplyr::case_when(
        is.na(route) ~ length_base_volume_short,
        TRUE ~ length_base_volume_short - aadt_lmv_base * 365 * length_coverage / 100
      ),
    length_calc_volume_short_adjusted =
      dplyr::case_when(
        is.na(route) ~ length_calc_volume_short,
        TRUE ~ length_calc_volume_short - aadt_lmv_calc * 365 * length_coverage / 100
      ),
    index_short_adjusted = ((length_calc_volume_short_adjusted / length_base_volume_short_adjusted - 1) * 100) |> round(2),
    label_text =
      paste(
        name, "<br/>",
        road_category_and_number
      ) |> lapply(htmltools::HTML),
    area_name = "Trondheimsområdet"
  )

readr::write_rds(
  trd_trp_index_tidy,
  file = "through_traffic/trd_trp_and_through_traffic.rds"
)


city_index_adjusted_trd <-
  dplyr::bind_rows(
    trd_trp_index_tidy |>
      dplyr::select(
        area_name,
        month_object,
        trp_id,
        length_base_volume_short,
        length_calc_volume_short,
        index_short
      ) |>
      dplyr::mutate(
        adjusted = FALSE
      ),
    trd_trp_index_tidy |>
      dplyr::select(
        area_name,
        month_object,
        trp_id,
        length_base_volume_short = length_base_volume_short_adjusted,
        length_calc_volume_short = length_calc_volume_short_adjusted,
        index_short = index_short_adjusted
      ) |>
      dplyr::mutate(
        adjusted = TRUE
      )
  ) |>
  dplyr::filter(
    !is.na(index_short)
  ) |>
  dplyr::group_by(
    area_name,
    month_object,
    adjusted
  ) |>
  dplyr::mutate(
    weight = (length_base_volume_short / sum(length_base_volume_short))
  ) |>
  dplyr::summarise(
    city_base_volume = sum(length_base_volume_short),
    city_calc_volume = sum(length_calc_volume_short),
    index_p = (city_calc_volume / city_base_volume - 1 ) * 100,
    n_trp = n(),
    standard_deviation = sqrt((1 / (1 - sum(weight^2) )) * sum(weight * (index_short - index_p)^2) ),
    standard_error = sqrt(sum(weight^2) * standard_deviation^2),
    ci_lower = round(index_p + stats::qt(0.025, n_trp) * standard_error, 1),
    ci_upper = round(index_p - stats::qt(0.025, n_trp) * standard_error, 1),
    .groups = "drop"
  )|>
  dplyr::select(
    -city_base_volume,
    -city_calc_volume
  )


readr::write_rds(
  city_index_adjusted_trd,
  file = "through_traffic/city_index_adjusted_trd.rds"
)


## Rolling index ----
mdt_filtered <-
  readr::read_rds(
    paste0(
      "data_indexpoints_tidy/mdt_",
      city_number,
      ".rds"
    )
  )

source("exclude_trp_mdts_list.R")

# Subtract through traffic by month by TRP in mdt_validated
mdt_validated_without_through_traffic <-
  mdt_validated |>
  dplyr::left_join(
    trp_and_route,
    by = join_by(trp_id)
  ) |>
  dplyr::left_join(
    through_traffic_mdt,
    by = join_by(area, route, year, month)
  ) |>
  dplyr::mutate(
    mdt_original = mdt,
    mdt_adjusted =
      dplyr::case_when(
        is.na(mdt_lmv) ~ mdt,
        TRUE ~ mdt - mdt_lmv
      )
  ) |>
  dplyr::select(-mdt) |>
  dplyr::rename(mdt = mdt_adjusted)


mdt_validated <- mdt_validated_without_through_traffic

all_36_month_indices_adjusted <-
  calculate_rolling_indices(36) |>
  dplyr::mutate(
    through_traffic = FALSE
  )

# Rolling index with through traffic
rolling_indices <-
  readr::read_rds(
    paste0("data_indexpoints_tidy/rolling_indices_", city_number, ".rds")
  )

city_36_month <-
  rolling_indices[[3]] |>
  dplyr::mutate(
    through_traffic = TRUE
  )

# Put together
dplyr::bind_rows(
  all_36_month_indices_adjusted,
  city_36_month
) |>
readr::write_rds(
  file =
    paste0(
      "data_indexpoints_tidy/rolling_indices_adjusted_",
      city_number,
      ".rds"
    )
)
