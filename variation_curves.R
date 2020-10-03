# Variation curves

source("get_from_trafficdata.R")
library(plotly)

# Trp by municipality ####
points <- get_points() %>%
  dplyr::select(trp_id, name, road_reference, municipality_name,
                lat, lon, traffic_type, number_of_directions) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::filter(traffic_type == "VEHICLE",
                number_of_directions == 2)

# Trondheim ####
points_trondheim <- points %>%
  dplyr::filter(municipality_name == "Trondheim",
                traffic_type == "VEHICLE") %>%
  dplyr::filter(!stringr::str_detect(road_reference, "KD"))

variation_curves_trondheim <-
  get_trp_average_hour_of_day_traffic_for_all_day_types_for_trp_list(points_trondheim$trp_id,
                                                                     "2018")

variation_curves_trondheim_low_coverage <- variation_curves_trondheim %>%
  dplyr::group_by(trp_id, day_type) %>%
  dplyr::summarise(mean_coverage = mean(coverage)) %>%
  dplyr::filter(mean_coverage < 50)

variation_curves_trondheim_info <- variation_curves_trondheim %>%
  dplyr::left_join(points_trondheim) %>%
  dplyr::filter(!(trp_id %in% variation_curves_trondheim_low_coverage$trp_id))

#test <- get_trp_average_hour_of_day_traffic_for_all_day_types("79743V1125914", "2019")

test %>% ggplot(aes(start_of_hour, average_hour_of_day_traffic_relative, color = day_type)) +
  geom_line() +
  xlab("timestart") +
  ylab("andel trafikk")

kurver_alle <- variation_curves_trondheim_info %>%
  dplyr::filter(day_type == "ALL") %>%
  ggplot(aes(start_of_hour, average_hour_of_day_traffic_relative, color = name)) +
  geom_line() +
  xlab("timestart") +
  ylab("andel trafikk")

kurver_alle %>% plotly::ggplotly()

minst_16 <- variation_curves_trondheim_info %>%
  dplyr::filter(start_of_hour == 8,
                day_type == "ALL")

# Tromsø ####
points_chosen <- points %>%
  dplyr::filter(municipality_name == "Tromsø",
                traffic_type == "VEHICLE") %>%
  dplyr::filter(!stringr::str_detect(road_reference, "KD"))


# Trp urban areas ####
# with help from tettsteder.R

# For easy viewing
urban_areas_no_geo <- urban_areas %>%
  sf::st_drop_geometry()

# TODO: creat function that takes urban area name and returns a plot

plot_average_hour_of_day_for_urban_area <- function(urban_area_name_input,
                                                    year_input = "2019",
                                                    day_type_input = "ALL") {

  # Needs trp_urban from tettsteder.R first

  trps_in_urban_area <- trp_urban %>%
    dplyr::filter(urban_area_name == urban_area_name_input)

  variation_curves_chosen <-
    get_trp_average_hour_of_day_traffic_for_all_day_types_for_trp_list(
      trps_in_urban_area$trp_id, year_input)

  # Removing curves with low coverage
  variation_curves_low_coverage <- variation_curves_chosen %>%
    dplyr::group_by(trp_id, day_type) %>%
    dplyr::summarise(mean_coverage = mean(coverage)) %>%
    dplyr::filter(mean_coverage < 50)

  variation_curves_info <- variation_curves_chosen %>%
    dplyr::left_join(trp_urban) %>%
    dplyr::filter(!(trp_id %in% variation_curves_low_coverage$trp_id))

  curves_chosen <- variation_curves_info %>%
    dplyr::filter(day_type == day_type_input) %>%
    ggplot(aes(start_of_hour, average_hour_of_day_traffic_relative, color = name)) +
    geom_line() +
    xlab("timestart") +
    ylab("andel trafikk")

  curves <- curves_chosen %>% plotly::ggplotly()

  return(curves)
}


hamar <- plot_average_hour_of_day_for_urban_area("Hamar")
moss <- plot_average_hour_of_day_for_urban_area("Moss")


trp_oslo <- trp_urban %>%
  dplyr::filter(urban_area_name == "Oslo")

trp_bergen <- trp_urban %>%
  dplyr::filter(urban_area_name == "Bergen")


variation_curves_chosen <-
  get_trp_average_hour_of_day_traffic_for_all_day_types_for_trp_list(trp_oslo$trp_id,
                                                                     "2019")

variation_curves_low_coverage <- variation_curves_chosen %>%
  dplyr::group_by(trp_id, day_type) %>%
  dplyr::summarise(mean_coverage = mean(coverage)) %>%
  dplyr::filter(mean_coverage < 50)

variation_curves_info <- variation_curves_chosen %>%
  dplyr::left_join(trp_urban_area) %>%
  dplyr::filter(!(trp_id %in% variation_curves_low_coverage$trp_id))

curves_chosen <- variation_curves_info %>%
  dplyr::filter(day_type == "ALL") %>%
  ggplot(aes(start_of_hour, average_hour_of_day_traffic_relative, color = name)) +
  geom_line() +
  xlab("timestart") +
  ylab("andel trafikk")

curves_chosen %>% plotly::ggplotly()



