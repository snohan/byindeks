# Find normal days for several years
# Generate trp table in daily_index scripts first.

# Normal daily traffic per month per border trp ----

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

# 2020 ----

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


# Normal daily traffic per month per city trp ####

day_of_week_traffic_2016 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
  trps$trp_id,
  2016
  )

day_of_week_traffic_2017 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
  trps$trp_id,
  2017
  )

day_of_week_traffic_2018 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
  trps$trp_id,
  2018
  )

day_of_week_traffic_2019 <- get_trp_average_day_of_week_traffic_by_month_for_a_year_for_trp_list(
  trps$trp_id,
  2019
  )

day_of_week_traffic_average <- dplyr::bind_rows(
  day_of_week_traffic_2016,
  day_of_week_traffic_2017,
  day_of_week_traffic_2018,
  day_of_week_traffic_2019
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
write.csv2(day_of_week_traffic_average,
           file = "city_normal_days.csv",
           row.names = F)


# Normal daily easter traffic per trp ####
# Defining the days of Easter to range from (inclusive) Friday before Palm Sunday to
# (inclusive) Easter Monday.

# Too few complete trps with good coverage in 2016
# easter_2016 <- get_dt_for_trp_list(trps$trp_id,
#                                    "2016-03-18T00:00:00+01:00",
#                                    "2016-03-29T00:00:00+02:00") %>%
#   dplyr::mutate(easter_day = dplyr::case_when(
#     from == "2016-03-18" ~ "Fredag",
#     from == "2016-03-19" ~ "Lørdag",
#     from == "2016-03-20" ~ "Palmesøndag",
#     from == "2016-03-21" ~ "Mandag",
#     from == "2016-03-22" ~ "Tirsdag",
#     from == "2016-03-23" ~ "Onsdag",
#     from == "2016-03-24" ~ "Skjærtorsdag",
#     from == "2016-03-25" ~ "Langfredag",
#     from == "2016-03-26" ~ "Påskeaften",
#     from == "2016-03-27" ~ "1. påskedag",
#     from == "2016-03-28" ~ "2. påskedag",
#     TRUE ~ "Annen dag"
#   ))
#
# easter_2016_complete <- easter_2016 %>%
#   dplyr::filter(coverage > 99) %>%
#   dplyr::group_by(point_id) %>%
#   dplyr::summarise(n_easter_days = n())

easter_2017 <- get_dt_for_trp_list(trps$trp_id,
                                   "2017-04-07T00:00:00+02:00",
                                   "2017-04-18T00:00:00+02:00") %>%
  dplyr::mutate(easter_day = dplyr::case_when(
    from == "2017-04-07" ~ "Fredag",
    from == "2017-04-08" ~ "Lørdag",
    from == "2017-04-09" ~ "Palmesøndag",
    from == "2017-04-10" ~ "Mandag",
    from == "2017-04-11" ~ "Tirsdag",
    from == "2017-04-12" ~ "Onsdag",
    from == "2017-04-13" ~ "Skjærtorsdag",
    from == "2017-04-14" ~ "Langfredag",
    from == "2017-04-15" ~ "Påskeaften",
    from == "2017-04-16" ~ "1. påskedag",
    from == "2017-04-17" ~ "2. påskedag",
    TRUE ~ "Annen dag"
  ))

easter_2017_complete <- easter_2017 %>%
  dplyr::filter(coverage > 99) %>%
  dplyr::group_by(point_id) %>%
  dplyr::summarise(n_easter_days = n()) %>%
  dplyr::filter(n_easter_days == 11)

easter_2017_filtered <- easter_2017 %>%
  dplyr::filter(point_id %in% easter_2017_complete$point_id)

easter_2018 <- get_dt_for_trp_list(trps$trp_id,
                                   "2018-03-23T00:00:00+01:00",
                                   "2018-04-03T00:00:00+02:00") %>%
  dplyr::mutate(easter_day = dplyr::case_when(
    from == "2018-03-23" ~ "Fredag",
    from == "2018-03-24" ~ "Lørdag",
    from == "2018-03-25" ~ "Palmesøndag",
    from == "2018-03-26" ~ "Mandag",
    from == "2018-03-27" ~ "Tirsdag",
    from == "2018-03-28" ~ "Onsdag",
    from == "2018-03-29" ~ "Skjærtorsdag",
    from == "2018-03-30" ~ "Langfredag",
    from == "2018-03-31" ~ "Påskeaften",
    from == "2018-04-01" ~ "1. påskedag",
    from == "2018-04-02" ~ "2. påskedag",
    TRUE ~ "Annen dag"
  ))

easter_2018_complete <- easter_2018 %>%
  dplyr::filter(coverage > 99) %>%
  dplyr::group_by(point_id) %>%
  dplyr::summarise(n_easter_days = n()) %>%
  dplyr::filter(n_easter_days == 11)

easter_2018_filtered <- easter_2018 %>%
  dplyr::filter(point_id %in% easter_2018_complete$point_id)


easter_2019 <- get_dt_for_trp_list(trps$trp_id,
                                   "2019-04-12T00:00:00+02:00",
                                   "2019-04-23T00:00:00+02:00") %>%
  dplyr::mutate(easter_day = dplyr::case_when(
    from == "2019-04-12" ~ "Fredag",
    from == "2019-04-13" ~ "Lørdag",
    from == "2019-04-14" ~ "Palmesøndag",
    from == "2019-04-15" ~ "Mandag",
    from == "2019-04-16" ~ "Tirsdag",
    from == "2019-04-17" ~ "Onsdag",
    from == "2019-04-18" ~ "Skjærtorsdag",
    from == "2019-04-19" ~ "Langfredag",
    from == "2019-04-20" ~ "Påskeaften",
    from == "2019-04-21" ~ "1. påskedag",
    from == "2019-04-22" ~ "2. påskedag",
    TRUE ~ "Annen dag"
  ))

easter_2019_complete <- easter_2019 %>%
  dplyr::filter(coverage > 99) %>%
  dplyr::group_by(point_id) %>%
  dplyr::summarise(n_easter_days = n()) %>%
  dplyr::filter(n_easter_days == 11)

easter_2019_filtered <- easter_2019 %>%
  dplyr::filter(point_id %in% easter_2019_complete$point_id)

easter_days <- c("Fredag",
                "Lørdag",
                "Palmesøndag",
                "Mandag",
                "Tirsdag",
                "Onsdag",
                "Skjærtorsdag",
                "Langfredag",
                "Påskeaften",
                "1. påskedag",
                "2. påskedag")

mean_easter_per_trp_2017_2019 <- dplyr::bind_rows(easter_2017_filtered,
                                                  easter_2018_filtered,
                                                  easter_2019_filtered) %>%
  dplyr::mutate(easter_day = factor(easter_day, levels = easter_days)) %>%
  dplyr::group_by(point_id, easter_day) %>%
  dplyr::summarise(total_volume = floor(mean(total_volume)),
                   included_days = n()) %>%
  dplyr::mutate(period = "2017-2019")

# write normal days to csv, and read it in when running report to save time etc.
write.csv2(mean_easter_per_trp_2017_2019,
           file = "easter_normal_days_2017_2019.csv",
           row.names = F)

easter_2020 <- get_dt_for_trp_list(trps$trp_id,
                                   "2020-04-03T00:00:00+02:00",
                                   "2020-04-14T00:00:00+02:00") %>%
  dplyr::mutate(easter_day = dplyr::case_when(
    from == "2020-04-03" ~ "Fredag",
    from == "2020-04-04" ~ "Lørdag",
    from == "2020-04-05" ~ "Palmesøndag",
    from == "2020-04-06" ~ "Mandag",
    from == "2020-04-07" ~ "Tirsdag",
    from == "2020-04-08" ~ "Onsdag",
    from == "2020-04-09" ~ "Skjærtorsdag",
    from == "2020-04-10" ~ "Langfredag",
    from == "2020-04-11" ~ "Påskeaften",
    from == "2020-04-12" ~ "1. påskedag",
    from == "2020-04-13" ~ "2. påskedag",
    TRUE ~ "Annen dag"
  ))

easter_2020_complete <- easter_2020 %>%
  dplyr::filter(coverage > 99) %>%
  dplyr::group_by(point_id) %>%
  dplyr::summarise(n_easter_days = n()) %>%
  dplyr::filter(n_easter_days == 11)

easter_2020_filtered <- easter_2020 %>%
  dplyr::filter(point_id %in% easter_2020_complete$point_id) %>%
  dplyr::mutate(period = "2020")

# write normal days to csv, and read it in when running report to save time etc.
write.csv2(easter_2020_filtered,
           file = "easter_days_2020.csv",
           row.names = F)

easter_2021 <- get_dt_for_trp_list(trps$trp_id,
                                   "2021-03-26T00:00:00+01:00",
                                   "2021-04-06T00:00:00+02:00") %>%
  dplyr::mutate(easter_day = dplyr::case_when(
    from == "2021-03-26" ~ "Fredag",
    from == "2021-03-27" ~ "Lørdag",
    from == "2021-03-28" ~ "Palmesøndag",
    from == "2021-03-29" ~ "Mandag",
    from == "2021-03-30" ~ "Tirsdag",
    from == "2021-03-31" ~ "Onsdag",
    from == "2021-04-01" ~ "Skjærtorsdag",
    from == "2021-04-02" ~ "Langfredag",
    from == "2021-04-03" ~ "Påskeaften",
    from == "2021-04-04" ~ "1. påskedag",
    from == "2021-04-05" ~ "2. påskedag",
    TRUE ~ "Annen dag"
  ))

easter_2021_complete <- easter_2021 %>%
  dplyr::filter(coverage > 99) %>%
  dplyr::group_by(point_id) %>%
  dplyr::summarise(n_easter_days = n()) %>%
  dplyr::filter(n_easter_days == 11)

easter_2021_filtered <- easter_2021 %>%
  dplyr::filter(point_id %in% easter_2021_complete$point_id) %>%
  dplyr::mutate(period = "2021")

# write normal days to csv, and read it in when running report to save time etc.
write.csv2(easter_2021_filtered,
           file = "easter_days_2021.csv",
           row.names = F)

