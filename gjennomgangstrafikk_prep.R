library(tidyverse)

# Traffic Data API calls to get points metadata and aadt
source("get_from_trafficdata_api.R")
source("split_road_system_reference.R")


# Through traffic ----
through_traffic <-
  readr::read_csv2("through_traffic/through_traffic.csv")

trp_and_route <-
  readr::read_csv2("through_traffic/trp_and_route.csv")


# Nedre Glomma ----
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


# Trondheim ----
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
