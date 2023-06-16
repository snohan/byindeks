split_road_system_reference <- function(df) {

  df_with_split_reference <-
    df |>
    tidyr::separate(
      col = road_reference,
      into = c("road_system", "intersection_part"),
      sep = "[[:blank:]][[:alpha:]]{1}D",
      remove = FALSE,
      fill = "right"
    ) |>
    dplyr::mutate(
      road_category = stringr::str_sub(road_system, 1, 1)
    ) |>
    dplyr::mutate(
      road_category =
        factor(
          road_category,
          levels = c("E", "R", "F", "K", "P", "S")
        )
    ) |>
    tidyr::separate(
      col = road_system,
      into = c("road", "section_meter"),
      sep = " S"
    ) |>
    dplyr::mutate(
      road_number = as.numeric(stringr::str_sub(road, 3, -1))
    ) |>
    dplyr::mutate(
      road_category_and_number = paste0(road_category, "v ", road_number)
    ) |>
    tidyr::separate(
      col = section_meter,
      into = c("section_number", "subsection_meter"),
      sep = "D",
      convert = TRUE
    ) |>
    tidyr::separate(
      col = subsection_meter,
      into = c("subsection_number", "meter"),
      sep = " m",
      convert = TRUE
    ) |>
    tidyr::separate(
      col = intersection_part,
      into = c("intersection_part_number", "intersection_meter"),
      sep = " m",
      convert = TRUE
    ) |>
    dplyr::arrange(
      road_category,
      road_number,
      section_number,
      subsection_number,
      meter,
      intersection_part_number,
      intersection_meter
    )
}


split_road_system_reference_section <- function(df) {

  # df must have column "road_reference_section"

  df_with_split_road_system_reference_section <-
    df |>
    dplyr::mutate(
      reverse_road_reference = stringi::stri_reverse(road_reference_section)
    ) |>
    tidyr::separate(
      col = reverse_road_reference,
      into = c("meter_section", "road_system"),
      sep = "m[[:blank:]]",
      extra = "merge",
      remove = TRUE,
      fill = "right"
    ) |>
    dplyr::mutate(
      road_system = stringi::stri_reverse(road_system),
      meter_section = stringi::stri_reverse(meter_section)
    ) |>
    tidyr::separate(
      col = meter_section,
      into = c("meter_start", "meter_end"),
      sep = "-"
    ) |>
    dplyr::mutate(
      meter_start = as.numeric(meter_start),
      meter_end = as.numeric(meter_end)
    ) |>
    dplyr::relocate(
      meter_start,
      meter_end,
      .after = road_system
    )

}
