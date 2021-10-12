# Prep city indices for use in PowerBI

# Traffic Data API calls to get points metadata and aadt
source("get_from_trafficdata_api.R")
source("split_road_system_reference.R")

# Fetching published index from Traffic Data API
# TODO: include n_points, needed to calculate SE

# City numbers                Refyear Last update
# Bergen 8952                 2018    2021-8
# Buskerudbyen 1952           2016    2021-8
# Grenland 955                2016    2021-8
# Kristiansand og omegn 957   2016    2021-4
# Nedre Glomma 953            2016    2021-8
# Nord-Jæren 952              2017    2021-8
# Oslo 959                    2018    2021-8
# Trondheim 960               2019    2021-3
# Tromsø 961                  2016    2021-8

index_2020_bergen <- get_published_index_for_months(8952, 2021, 4)
index_2020_buskerud <- get_published_index_for_months(1952, 2021, 4)
index_2020_grenland <- get_published_index_for_months(955, 2021, 4)
index_2020_krs_omegn <- get_published_index_for_months(957, 2021, 4)
index_2020_glomma <- get_published_index_for_months(953, 2021, 4)
index_2020_jaren <- get_published_index_for_months(952, 2021, 4)
index_2020_oslo <- get_published_index_for_months(959, 2021, 4)
index_2020_tromso <- get_published_index_for_months(961, 2021, 4)

index_2020 <- dplyr::bind_rows(index_2020_bergen,
                               index_2020_buskerud,
                               index_2020_grenland,
                               index_2020_krs_omegn,
                               index_2020_glomma,
                               index_2020_jaren,
                               index_2020_oslo,
                               index_2020_tromso) %>%
  dplyr::select(area_name, road_category, length_range, year, month, period,
                index_p) %>%
  dplyr::mutate(length_range = dplyr::case_when(length_range == "[..,..)" ~ "alle",
                                                length_range == "[..,5.6)" ~ "lette",
                                                length_range == "[5.6,..)" ~ "tunge")) %>%
  dplyr::mutate(road_category = dplyr::case_when(
    road_category == "FYLKESVEG" ~ "Fylkesveg",
    road_category == "EUROPAVEG_RIKSVEG" ~ "Europa- og riksveg",
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG" ~ "Europa-, riks- og fylkesveg")) %>%
  dplyr::filter(road_category == "Europa-, riks- og fylkesveg") %>%
  dplyr::select(-road_category)

# Including Trondheim with results in csv:
city_trd <-
  read.csv2(
    paste0("data_indexpoints_tidy/byindeks_960.csv")) %>%
  dplyr::filter(year == 2021,
                month == 3,
                year_base == 2020) %>%
  dplyr::mutate(period = "year_to_date") %>%
  dplyr::select(area_name, length_range, year, month, period, index_p)

city_monthly_trd <-
  read.csv2(
    paste0("data_indexpoints_tidy/byindeks_maanedlig_960.csv")) %>%
  dplyr::filter(year == 2021) %>%
  dplyr::select(area_name, length_range, year, month, period, index_p)

index_2020_complete <- dplyr::bind_rows(index_2020,
                                        city_trd,
                                        city_monthly_trd) %>%
  dplyr::mutate(month_object = lubridate::make_date(year = year, month = month),
                month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE))

# Write to file for use in PowerBI (because TRD is special)
index_2020_complete %>%
  dplyr::mutate(length_range = stringr::str_to_title(length_range),
                period = dplyr::case_when(
                  period == "month" ~ "Måned",
                  period == "year_to_date" ~ "Hittil i år"
                )) %>%
  write.csv2(file = "city_indices_for_powerbi.csv", row.names = F)