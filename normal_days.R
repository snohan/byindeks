# Find normal days for several years

# Normal daily traffic per month per trp ####

# 2016 - 2019
# day_of_week_traffic_2016 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
#   border_trp_ids,
#   2016
#   )
#
# day_of_week_traffic_2017 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
#   border_trp_ids,
#   2017
#   )
#
# day_of_week_traffic_2018 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
#   border_trp_ids,
#   2018
#   )
#
# day_of_week_traffic_2019 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
#   border_trp_ids,
#   2019
#   )
#
# day_of_week_traffic_average <- dplyr::bind_rows(
#   day_of_week_traffic_2016,
#   day_of_week_traffic_2017,
#   day_of_week_traffic_2018,
#   day_of_week_traffic_2019
#   ) %>%
#   dplyr::group_by(trp_id, month, day_name) %>%
#   dplyr::summarise(
#     daily_traffic = round(mean(average_day_of_week_traffic),
#                           digits = 0),
#     included_days = sum(included_days),
#     possible_days = sum(possible_days)
#   ) %>%
#   dplyr::mutate(
#     day_number_of_week = dplyr::case_when(
#       day_name == "MONDAY" ~ 1,
#       day_name == "TUESDAY" ~ 2,
#       day_name == "WEDNESDAY" ~ 3,
#       day_name == "THURSDAY" ~ 4,
#       day_name == "FRIDAY" ~ 5,
#       day_name == "SATURDAY" ~ 6,
#       day_name == "SUNDAY" ~ 7
#     )
#   )

# write normal days to csv, and read it in when running report to save time etc.
# write.csv2(day_of_week_traffic_average,
#            file = "border_normal_days.csv",
#            row.names = F)

# 2020

day_of_week_traffic_2020 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
  #border_trp_ids,
  trps$trp_id,
  2020
  )

day_of_week_traffic_average_2020 <- dplyr::bind_rows(
  day_of_week_traffic_2020
  ) %>%
  dplyr::group_by(trp_id, month, day_name) %>%
  dplyr::summarise(
    daily_traffic = round(mean(average_day_of_week_traffic),
                          digits = 0),
    included_days = sum(included_days),
    possible_days = sum(possible_days)
  ) %>%
  dplyr::mutate(
    day_number_of_week = dplyr::case_when(
      day_name == "MONDAY" ~ 1,
      day_name == "TUESDAY" ~ 2,
      day_name == "WEDNESDAY" ~ 3,
      day_name == "THURSDAY" ~ 4,
      day_name == "FRIDAY" ~ 5,
      day_name == "SATURDAY" ~ 6,
      day_name == "SUNDAY" ~ 7
    )
  )

# write normal days to csv, and read it in when running report to save time etc.
write.csv2(day_of_week_traffic_average_2020,
           #file = "border_normal_days_2020.csv",
           file = "city_normal_days_2020.csv",
           row.names = F)


# Normal daily traffic per month per trp ####

# day_of_week_traffic_2016 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
#   trps$trp_id,
#   2016
#   )
#
# day_of_week_traffic_2017 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
#   trps$trp_id,
#   2017
#   )
#
# day_of_week_traffic_2018 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
#   trps$trp_id,
#   2018
#   )
#
# day_of_week_traffic_2019 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
#   trps$trp_id,
#   2019
#   )
#
# day_of_week_traffic_average <- dplyr::bind_rows(
#   day_of_week_traffic_2016,
#   day_of_week_traffic_2017,
#   day_of_week_traffic_2018,
#   day_of_week_traffic_2019
#   ) %>%
#   dplyr::group_by(trp_id, month, day_name) %>%
#   dplyr::summarise(
#     daily_traffic = round(mean(average_day_of_week_traffic),
#                           digits = 0),
#     included_days = sum(included_days),
#     possible_days = sum(possible_days)
#   ) %>%
#   dplyr::mutate(
#     day_number_of_week = dplyr::case_when(
#       day_name == "MONDAY" ~ 1,
#       day_name == "TUESDAY" ~ 2,
#       day_name == "WEDNESDAY" ~ 3,
#       day_name == "THURSDAY" ~ 4,
#       day_name == "FRIDAY" ~ 5,
#       day_name == "SATURDAY" ~ 6,
#       day_name == "SUNDAY" ~ 7
#     )
#   )

# write normal days to csv, and read it in when running report to save time etc.
# write.csv2(day_of_week_traffic_average,
#            file = "city_normal_days.csv",
#            row.names = F)