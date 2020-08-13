split_road_system_reference <- function(df) {

  df_with_split_reference <- df %>%
    tidyr::separate(road_reference, c("road_system", "intersection_part"),
                    sep = "[[:blank:]][[:alpha:]]{1}D",
                    remove = FALSE, fill = "right") %>%
    dplyr::mutate(road_category = stringr::str_sub(road_system, 1, 1)) %>%
    dplyr::mutate(road_category = factor(road_category,
                                         levels = c("E", "R", "F", "K", "P"))) %>%
    tidyr::separate(road_system, c("road", "section_meter"),
                    sep = " S") %>%
    dplyr::mutate(road_number = as.numeric(stringr::str_sub(road, 3, -1))) %>%
    dplyr::mutate(road_category_and_number = paste0(road_category, "v", road_number)) %>%
    tidyr::separate(section_meter, c("section_number", "subsection_meter"),
                    sep = "D", convert = TRUE) %>%
    tidyr::separate(subsection_meter, c("subsection_number", "meter"),
                    sep = " m", convert = TRUE) %>%
    tidyr::separate(intersection_part, c("intersection_part_number", "intersection_meter"),
                    sep = " m", convert = TRUE)  %>%
    dplyr::arrange(road_category, road_number,
                   section_number, subsection_number, meter,
                   intersection_part_number, intersection_meter)
}