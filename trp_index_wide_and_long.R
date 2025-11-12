# Widening for table view
pointindex_prepared_all_wide_total_index <-
  pointindex_prepared_all |>
  dplyr::select(
    name,
    road_category_and_number,
    adt,
    month,
    index_total_p
  ) |>
  tidyr::pivot_wider(
    names_from = month,
    values_from = index_total_p,
    names_prefix = "m"
  ) |>
  # make all these names take up four rows so the table always takes up same amount of vertical space
  dplyr::mutate(
    value_type =
      paste(
        " ", "<br/>",
        "index", "<br/>",
        "total", "<br/>",
        "p"
      )
  )

pointindex_prepared_all_wide_coverage <-
  pointindex_prepared_all |>
  dplyr::select(name, road_category_and_number, adt, month, index_total_coverage) |>
  tidyr::pivot_wider(
    names_from = month,
    values_from = index_total_coverage,
    names_prefix = "m"
  ) |>
  dplyr::mutate(
    value_type =
      paste(
        " ", "<br/>",
        "index", "<br/>",
        "total", "<br/>",
        "coverage"
      )
  )

# pointindex_prepared_all_wide_length_coverage <-
#   pointindex_prepared_all |>
#   dplyr::select(name, road_category_and_number, adt, month, length_coverage) |>
#   tidyr::pivot_wider(
#     names_from = month,
#     values_from = length_coverage,
#     names_prefix = "m"
#   ) |>
#   dplyr::mutate(
#     value_type =
#       paste(
#         " ", "<br/>",
#         " ", "<br/>",
#         "length", "<br/>",
#         "coverage"
#       )
#   )

pointindex_prepared_all_wide_short <-
  pointindex_prepared_all |>
  dplyr::select(name, road_category_and_number, adt, month, index_short) |>
  tidyr::pivot_wider(
    names_from = month,
    values_from = index_short,
    names_prefix = "m"
  ) |>
  dplyr::mutate(
    value_type =
      paste(
        " ", "<br/>",
        "index", "<br/>",
        "short", "<br/>",
        "p"
      )
  )

pointindex_prepared_all_wide_long <-
  pointindex_prepared_all |>
  dplyr::select(name, road_category_and_number, adt, month, index_long) |>
  tidyr::pivot_wider(
    names_from = month,
    values_from = index_long,
    names_prefix = "m"
  ) |>
  dplyr::mutate(
    value_type =
      paste(
        " ", "<br/>",
        "index", "<br/>",
        "long", "<br/>",
        "p"
      )
  )

pointindex_prepared_all_wide_check <-
  pointindex_prepared_all |>
  dplyr::select(name, road_category_and_number, adt, month, total_short_long_check) |>
  tidyr::pivot_wider(
    names_from = month,
    values_from = total_short_long_check,
    names_prefix = "m"
  ) |>
  dplyr::mutate(
    value_type =
      paste(
        "total", "<br/>",
        "short", "<br/>",
        "long", "<br/>",
        "check"
      )
  )

pointindex_prepared_all_wide_diff <-
  pointindex_prepared_all |>
  dplyr::select(name, road_category_and_number, adt, month, diff_total_short) |>
  tidyr::pivot_wider(
    names_from = month,
    values_from = diff_total_short,
    names_prefix = "m"
  ) |>
  dplyr::mutate(
    value_type =
      paste(
        " ", "<br/>",
        "diff", "<br/>",
        "total", "<br/>",
        "short"
      )
  )

pointindex_wide <-
  dplyr::bind_rows(
    pointindex_prepared_all_wide_total_index,
    pointindex_prepared_all_wide_coverage,
    #pointindex_prepared_all_wide_length_coverage,
    pointindex_prepared_all_wide_short,
    pointindex_prepared_all_wide_long,
    pointindex_prepared_all_wide_check,
    pointindex_prepared_all_wide_diff
  ) |>
  dplyr::rename(
    road = road_category_and_number,
  ) |>
  dplyr::arrange(name)

# Longing :) for plot
pointindex_long <-
  pointindex_prepared_all |>
  dplyr::select(trp_id:road_category_and_number)

pointindex_long_total <-
  pointindex_long |>
  dplyr::select(
    trp_id,
    year,
    month,
    index = index_total_p,
    coverage = index_total_coverage,
    length_excluded,
    name,
    road_category_and_number
  ) |>
  dplyr::mutate(length_range = "total")

pointindex_long_short <-
  pointindex_long |>
  dplyr::select(
    trp_id,
    year,
    month,
    index_short,
    index_long,
    coverage = length_coverage,
    length_excluded,
    name,
    road_category_and_number
  ) |>
  tidyr::pivot_longer(
    cols = c("index_short", "index_long"),
    names_to = "length_range",
    values_to = "index"
  )

pointindex_long_2 <-
  dplyr::bind_rows(
    pointindex_long_total,
    pointindex_long_short
  ) |>
  dplyr::mutate(
    length_range =
      dplyr::case_when(
        length_range == "total" ~ "alle",
        length_range == "index_short" ~ "lette",
        length_range == "index_long" ~ "tunge"
      )
  ) |>
  dplyr::mutate(
    month_object = lubridate::make_date(year = year, month = month),
    month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)
  )