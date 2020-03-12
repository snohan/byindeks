# Change in traffic per day

# Packages are to be loaded through sourcing rmd_setup.R in the Rmd report file.
source("rmd_setup.R")

# Traffic Data API calls to get points metadata and aadt
source("get_from_trafficdata_api.R")

# Points ####

# Points used in each city
cities_points <- read.csv2("data_points_raw/cities_points.csv")

# All points from Traffic Data API
points <- get_points() %>%
  #dplyr::select(trp_id, name, road_reference, lat, lon) %>%
  dplyr::distinct(trp_id, .keep_all = T)

# Trps
trp_trondheim_2017_ids <- cities_points %>%
  dplyr::filter(city_area_name == "Trondheim",
                agreement_start == 2017) %>%
  dplyr::select(trp_id) %>%
  filter(!(trp_id %in% c("65625V41945")))

# Adding metadata to trps
trp_trondheim_2017 <- dplyr::left_join(trp_trondheim_2017_ids, points) %>%
  filter(!is.na(name))

# Days we want
week_2019_from <- "2019-03-11T00:00:00+01:00"
week_2019_to <- "2019-03-18T00:00:00+01:00"

week_2020_from <- "2020-03-02T00:00:00+01:00"
week_2020_to <- "2020-03-12T00:00:00+01:00"

ukedager <- c("Mandag",
              "Tirsdag",
              "Onsdag",
              "Torsdag",
              "Fredag",
              "Lørdag",
              "Søndag")

norske_ukedager <- data.frame(weekday = c(1, 2, 3, 4, 5, 6, 7),
                              ukedag = ordered(ukedager,
                                               levels = ukedager))

# Fetch daily traffic from API
dt_2019 <- get_dt_for_trp_list(trp_trondheim_2017$trp_id,
                        week_2019_from,
                        week_2019_to)


dt_2020 <- get_dt_for_trp_list(trp_trondheim_2017$trp_id,
                               week_2020_from,
                               week_2020_to)


dt <- bind_rows(dt_2019, dt_2020) %>%
  select(-point_name) %>%
  mutate(year = lubridate::year(from),
         weekno = lubridate::isoweek(from),
         weekday = lubridate::wday(from,
                                   week_start = getOption("lubridate.week.start", 1))) %>%
  select(-from)

# Compare this week to 2020 week 10
dt_2020_week_10 <- dt %>%
  filter(year == 2020) %>%
  pivot_wider(id_cols = c(point_id, weekday),
              names_from = weekno,
              values_from = total_volume,
              names_prefix = "week_",
              values_fill = list(total_volume = NA)) %>%
  mutate(index = round((week_11 / week_10 - 1) * 100, digits = 1)) %>%
  left_join(norske_ukedager) %>%
  select(point_id, ukedag, index) %>%
  left_join(points, by = c("point_id" = "trp_id")) %>%
  select(ukedag, name, road_reference, municipality_name, index)



# TODO: compare this week to same week 2019





