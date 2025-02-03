# Is the selection of city index points representative for the area?
# Is the observed TRP indexes coming from the same distribution as the population of traffic links?

# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  library(tidyverse)
  library(readxl)
  library(fitdistrplus)
  library(gamlss)
  library(moments)
  library(corrplot)
  library(car)
  library(pwr)
  library(tidygraph)
  library(ggraph)
  library(paletteer)
  library(leaflet)
  library(sf)
  source("get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/traffic_link_functions.R")
  source("H:/Programmering/R/byindeks/leaflet_nvdb_map_setup.R")
}

trp_continuous <-
  get_points() |>
  dplyr::filter(
    registration_frequency == "CONTINUOUS"
  ) |>
  dplyr::select(
    trp_id
  ) |>
  dplyr::distinct()




# Traffic links ----
# 1. Look for prediction variables when the traffic link index is the target variable.
# 2. Compare sample links (those with index value, or theoretically those with TRP) with link population per prediction variable.

all_links <-
  jsonlite::fromJSON("C:/Users/snohan/Desktop/traffic-links-2023.json") |>
  dplyr::filter(
    hasOnlyPublicTransportLanes == FALSE,
    isFerryTrafficLink == FALSE
  ) |>
  dplyr::select(
    id,
    roadCategory,
    roadSystemReferences,
    countyIds,
    municipalityIds,
    associatedTrpIds,
    associatedTollStationIds,
    length,
    minLanes,
    maxLanes,
    highestSpeedLimit,
    functionClasses,
    functionalRoadClasses,
    urbanRatio,
    numberOfEstablishments,
    numberOfEmployees,
    numberOfInhabitants,
    trafficVolumes
  )

all_links_tidy <-
  all_links |>
  dplyr::rowwise() |>
  dplyr::filter(
    !is.null(unlist(functionalRoadClasses)),
    !is.null(unlist(functionClasses))
  ) |>
  dplyr::mutate(
    functional_class = min(unlist(functionalRoadClasses)),
    function_class = dplyr::first(unlist(functionClasses)) |> stringr::str_sub(1,1),
    road_category = dplyr::last(unlist(roadCategory))
  ) |>
  dplyr::select(
    -functionClasses,
    -functionalRoadClasses,
    -roadSystemReferences
  ) |>
  tidyr::unnest(
    trafficVolumes
  ) |>
  dplyr::filter(
    year == 2023,
    trafficVolumeResolution == "ADT",
    trafficVolumeType == "GUESSTIMATED"
  ) |>
  dplyr::select(
    id,
    road_category,
    municipalityIds,
    associatedTrpIds,
    #associatedTollStationIds,
    #length,
    minLanes,
    #maxLanes,
    highestSpeedLimit,
    functional_class,
    function_class,
    aadt = trafficVolumeValue,
    traffic_work_km = trafficWorkValue,
    urbanRatio
    #numberOfEstablishments,
    #numberOfEmployees,
    #numberOfInhabitants
  ) |>
  tidyr::unnest(
    municipalityIds
  ) |>
  dplyr::mutate(
    city =
      dplyr::case_when(
        # NOT OLD NUMBERS EVEN THOUGH IT IS 2023!
        #municipalityIds %in% c(5001, 5028, 5029, 5059, 5031, 5035) ~ "Trondheim",
        municipalityIds %in% c(4601, 4626, 4627, 4631, 4624) ~ "Bergen",
        municipalityIds %in% c(1127, 1103, 1124, 1108) ~ "Nord-Jæren", # should Stavanger and Sandnes be limited?
        #municipalityIds %in% c(301, 3205, 3207, 3201, 3203, 3209, 3240, 3216, 3228, 3226, 3220) ~ "Oslo",
        municipalityIds %in% c(3301, 3312, 3314, 3303) ~ "Buskerudbyen",
        municipalityIds %in% c(3105, 3107) ~ "Nedre Glomma",
        #municipalityIds %in% c(4001, 4003, 4010, 4012) ~ "Grenland",
        #municipalityIds %in% c(4204, 4223, 4218, 4216, 4215) ~ "Kristiansand"
      ),
    functional_class =
      dplyr::case_when(
        functional_class > 5 ~ 5,
        TRUE ~ functional_class
      )
  ) |>
  dplyr::select(
    -municipalityIds
  ) |>
  # Remove duplicates (those crossing municipalities)
  dplyr::distinct() |>
  dplyr::filter(
    !is.na(city),
    road_category != "Privat veg"
  ) |>
  tidyr::unnest(
    # Will duplicate links with more than one TRP
    associatedTrpIds,
    keep_empty = TRUE
  ) |>
  # tidyr::unnest(
  #   # Gives no duplicates
  #   associatedTollStationIds,
  #   keep_empty = TRUE
  # ) |>
  dplyr::mutate(
    #associatedTollStationIds = as.character(associatedTollStationIds),
    road_category = factor(road_category),
    minLanes = factor(minLanes),
    highestSpeedLimit = factor(highestSpeedLimit),
    functional_class = factor(functional_class),
    function_class = factor(function_class),
    traffic_work_Mkm = traffic_work_km * 1e-6,
    #log_traffic_work_Mkm = log(traffic_work_Mkm + 1),
    #log_log_traffic_work_Mkm = log(log_traffic_work_Mkm + 1),
    #sqrt_traffic_work_Mkm = sqrt(traffic_work_Mkm + 0.5)
  ) |>
  # Making urban ratio a factor
  dplyr::mutate(
    urbanRatio =
      dplyr::case_when(
        urbanRatio == 0 ~ "rural",
        urbanRatio > 0 & urbanRatio < 1 ~ "inter",
        urbanRatio == 1 ~ "urban"
      ) |>
      factor()
  )

traffic_link_id_and_trp_id <-
  all_links_tidy |>
  dplyr::select(
    id,
    trp_id = associatedTrpIds
  ) |>
  dplyr::filter(
    !is.na(trp_id)
  )

# Putting toll ids in same column as trp_id
# Will duplicate those with both TRP and toll station
# all_links_tidy_2 <-
#   dplyr::bind_rows(
#     all_links_tidy |>
#       dplyr::select(
#         -associatedTollStationIds
#       ) |>
#       dplyr::rename(
#         p_id = associatedTrpIds
#       ),
#     all_links_tidy |>
#       dplyr::select(
#         -associatedTrpIds
#       ) |>
#       dplyr::rename(
#         p_id = associatedTollStationIds
#       ) |>
#       dplyr::filter(
#         !is.na(p_id)
#       )
#   )
#
# all_links_tidy_no_duplicates <-
#   all_links_tidy_2 |>
#   dplyr::select(
#     -p_id
#   ) |>
#   dplyr::distinct()

all_links_tidy_no_duplicates <-
  all_links_tidy |>
  dplyr::select(
    -associatedTrpIds
  ) |>
  dplyr::distinct()

# Look at how links are distributed
link_stats <-
  all_links_tidy_no_duplicates |>
  dplyr::summarise(
    n_links = n(),
    sum_traffic_work = sum(traffic_work_Mkm),
    .by = c(city)
  )


## AADT and traffic work
# all_links_tidy_no_duplicates |>
#   ggplot(aes(aadt)) +
#   geom_histogram() +
#   facet_wrap(facets = "city")
#
# all_links_tidy_no_duplicates |>
#   ggplot(aes(log_traffic_work_Mkm)) +
#   geom_histogram() +
#   facet_wrap(facets = "city")
#
# all_links_tidy_no_duplicates |>
#   ggplot(aes(log_traffic_work_Mkm)) +
#   geom_boxplot() +
#   facet_wrap(facets = "city")
#
# all_links_tidy_no_duplicates |>
#   ggplot(aes(sqrt_traffic_work_Mkm)) +
#   geom_histogram() +
#   facet_wrap(facets = "city")
#
# all_links_tidy_no_duplicates |>
#   ggplot(aes(log_log_traffic_work_Mkm)) +
#   geom_histogram() +
#   facet_wrap(facets = "city")
#
# Poisson? Negative binomial?

#all_links_tidy_no_duplicates$traffic_work_Mkm |> base::summary()

all_links_tidy_no_duplicates |>
  ggplot(aes(road_category)) +
  geom_bar() +
  facet_wrap(facets = "city")

all_links_tidy_no_duplicates |>
  ggplot(aes(functional_class)) +
  geom_bar() +
  facet_wrap(facets = "city")
# Suggests that values over 5 should be mapped to 5.
# Then maybe group 0_1, 2_3 and 4_5.
# Oslo is unique - does this reflect the nature of the traffic? Doubtful.

all_links_tidy_no_duplicates |>
  ggplot(aes(function_class)) +
  geom_bar() +
  facet_wrap(facets = "city")
# Again, Oslo is unique. Supposedly this does not reflect a true difference in the nature of the traffic network.
# I.e. the classification of roads are not comparable.
# Maybe group into A, B_C and D_E.

all_links_tidy_no_duplicates |>
  ggplot(aes(urbanRatio)) +
  geom_bar() +
  facet_wrap(facets = "city")
# Oslo is significantly more urban.
# Maybe reduce to two categories: urban_inter and rural.


all_links_tidy_no_duplicates |>
  ggplot(aes(numberOfInhabitants)) +
  geom_histogram() +
  facet_wrap(facets = "city")

all_links_tidy_no_duplicates |>
  ggplot(aes(aadt)) +
  geom_histogram() +
  facet_wrap(facets = c("function_class", "city"))


### City TRPs ----
# city_id <-
#   c(960, 952, 8952, 959, 1952, 955, 957)
#
# city_names <-
#   c("Trondheim", "Nord-Jæren", "Bergen", "Oslo", "Buskerudbyen", "Grenland", "Kristiansand")
#
# city_names_and_ids <-
#   tibble::tibble(
#     city_id,
#     city_names
#   )
#
# city_trp_info <-
#   purrr::map(
#     city_id,
#     ~ readr::read_rds(
#       file = paste0(
#         "index_trp_metadata/trp_",
#         .x,
#         ".rds"
#       )
#     ) |>
#       dplyr::mutate(
#         city_id = .x
#       )
#   ) |>
#   dplyr::bind_rows() |>
#   dplyr::select(
#     city_id,
#     p_id = trp_id,
#     name
#   ) |>
#   dplyr::filter(
#     # Removing toll station "Nord for Sluppen bru" which has been moved
#     !(p_id == "1017875672")
#   ) |>
#   dplyr::left_join(
#     city_names_and_ids,
#     by = join_by(city_id)
#   )

# TODO: links on municipality roads


## TRP index ----
trp_index_raw <-
  purrr::map(
    c(952, 8952, 1952, 18952),
    ~ get_published_pointindex_for_months(.x, 2024, 12)[[2]]
  ) |>
  purrr::list_rbind()

trp_index <-
  trp_index_raw |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "month"
  ) |>
  # dplyr::slice_max(
  #   order_by = month,
  #   by = c(trp_id, year)
  # ) |>
  dplyr::select(
    trp_id,
    index = index_short,
    month
  ) |>
  dplyr::left_join(
    traffic_link_id_and_trp_id,
    by = join_by(trp_id)
  )  |>
  dplyr::filter(
    !is.na(index),
    # Not Easter
    month %in% c(1, 2, 5:12),
    # Not municipality roads
    !is.na(id),
    # remove outliers
    index < 10,
    index > -10
  )

link_index <-
  trp_index |>
  dplyr::left_join(
    all_links_tidy_no_duplicates,
    by = dplyr::join_by(id)
  )


#### Correlations numerical variables ----
corr_numeric <- tibble::tibble()
for (v in c(1, 2, 5:12)) {

  this_month <-
    link_index|>
    dplyr::filter(
      month == v
    ) |>
    dplyr::select(
      -month,
      -traffic_work_km
    ) |>
    purrr::keep(is.numeric) |>
    stats::cor(
      method = "spearman"
    ) |>
    tibble::as_tibble() |>
    dplyr::slice(1) |>
    dplyr::mutate(
      month = v
    )

  corr_numeric <-
    dplyr::bind_rows(
      corr_numeric,
      this_month
    )

}

# None!

link_index |>
  purrr::keep(is.numeric) |>
  stats::cor(
    method = "spearman"
  ) |>
  corrplot::corrplot(
    type = "lower"
  )


#### Correlation categorical variables ----
link_index |>
  # dplyr::filter(
  #   city == "Bergen"
  # ) |>
  ggplot(aes(road_category, index)) +
  geom_boxplot() +
  facet_wrap(~month)

link_index |>
  dplyr::mutate(
   month = factor(month)
  ) |>
  ggplot(aes(month, index)) +
  geom_boxplot()

link_index |>
  # dplyr::filter(
  #   city == "Bergen"
  # ) |>
  ggplot(aes(function_class, index)) +
  geom_boxplot() +
  facet_wrap(~month)

# None!

# Look for groups with more equal variance, in order to lower uncertainty.
# Compare box sizes

# ANOVA
test_aov <-
  stats::aov(
    index ~ function_class,
    data = link_index
  )

summary(test_aov)

hist(test_aov$residuals)
shapiro.test(test_aov$residuals)
# fairly normal

# Equal variances?
car::leveneTest(
  index ~ function_class,
  data = link_index
)
# No

stats::oneway.test(
  index ~ function_class,
  data = link_index,
  var.equal = FALSE
)
# Groups do not have significantly different means if ?


# TODO: Select features as is? Or transform?
# Should set up a prediction model in order to be able to really select the most important features?
# Looking too hard for something that isn't really there?

# Same analysis, but also looking at F_lamba_half
month_trp_index <-
  purrr::map(
    c(1952, 955, 957, 952, 959, 8952),
    ~ get_published_pointindex_for_months(.x, 2023, 12)[[2]]
  ) |>
  purrr::list_rbind() |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "month"
  ) |>
  dplyr::select(
    area_name,
    trp_id,
    year,
    month,
    period,
    index_total_coverage,
    length_coverage,
    length_calc_volume_short,
    length_base_volume_short,
    index_short
  ) |>
  dplyr::mutate(
    F_lambda_half = 2 * (sqrt(length_calc_volume_short) - sqrt(length_base_volume_short))
  )

### Check for normality
unique_city_names <-
  month_trp_index |>
  dplyr::select(
    area_name
  ) |>
  distinct()

month_trp_index |>
  # dplyr::filter(
  #   area_name == unique_city_names$area_name[6]
  # ) |>
  ggplot(
    aes(F_lambda_half)
    #aes(index_short)
  ) +
  geom_histogram() +
  facet_wrap(
    facets = vars(month),
    ncol = 3
  )

month_trp_index |>
  # dplyr::filter(
  #   area_name == unique_city_names$area_name[6]
  # ) |>
  ggplot(aes(sample = F_lambda_half)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(
    facets = vars(month),
    ncol = 3
  )

# Looks fairly normal, and fairly same for index_short and F.

## Monthly index and links
month_trp_index_links <-
  month_trp_index |>
  dplyr::left_join(
    all_links_tidy_2,
    by = join_by(trp_id == p_id)
  ) |>
  dplyr::select(
    city,
    #trp_id,
    month,
    #index_short,
    F_lambda_half,
    # Possible predictors, categorical
    road_category,
    urbanity = urbanRatio,
    # minLanes,
    # maxLanes,
    # highestSpeedLimit,
    functional_class,
    function_class,
    # Possible predictors, numerical
    length,
    aadt,
    numberOfEstablishments,
    numberOfEmployees,
    numberOfInhabitants
  ) |>
  # Removing TRPs without match
  dplyr::filter(
    !is.na(road_category)
  ) |>
  dplyr::mutate(
    #functional_class_max_lanes = paste(functional_class, maxLanes, sep = "_"),
    #speed_min_lanes = paste(highestSpeedLimit, minLanes, sep = "_"),
    #road_cat_urban = paste(road_category, urbanRatio, sep = "_"),
    #function_class_min_lanes = paste(function_class, minLanes, sep = "_")
    #speed_urban = paste(highestSpeedLimit, urbanRatio, sep = "_")
    # min_lanes_groups =
    #   dplyr::case_when(
    #     minLanes %in% c(1, 2, 3) ~ "<4",
    #     TRUE ~ ">=4"
    #   ),
    month = as.factor(month),
    road_category =
      dplyr::case_when(
        road_category == "Europaveg" ~ "R",
        road_category == "Riksveg" ~ "R",
        road_category == "Fylkesveg" ~ "F"
      ),
    # speed_limit_grouped =
    #   dplyr::case_when(
    #     highestSpeedLimit %in% c(30, 40, 50) ~ "<=50",
    #     highestSpeedLimit %in% c(60, 70, 80) ~ "<=80",
    #     TRUE ~ ">80"
    #   ),
    urbanity =
      dplyr::case_when(
        urbanity %in% c("urban", "inter") ~ "urban",
        TRUE ~ "rural"
      ),
    #traffic_work = length * aadt,
    #interaction_term = paste(road_category, urbanity, sep = "_"),
    length = log(length + 1),
    aadt_group =
      dplyr::case_when(
        aadt < 5000 ~ "<5k",
        aadt < 25000 ~ "<25k",
        TRUE ~ ">25k"
      ),
    aadt = log(aadt + 1),
    dplyr::across(
      c(F_lambda_half, aadt, length, tidyselect::starts_with("number")),
      ~ as.numeric(scale(.x))
    )
  )

# Not suitable to use speed or lanes here, as cities differ in occurrence.


# Categorical variables
month_trp_index_links |>
  ggplot(
    aes(
      x = aadt_group,
      y = F_lambda_half
    )
  ) +
  geom_boxplot() +
  facet_wrap(facets = "city")
# No obvious groupings!
# Month is one, obviously.


# Numerical variables
month_trp_index_links |>
  dplyr::select(
    F_lambda_half, aadt, length, tidyselect::starts_with("number")
  ) |>
  stats::cor(
    method = "spearman"
  ) |>
  corrplot::corrplot(
    type = "lower"
  )

month_trp_index_links |>
  ggplot(aes(F_lambda_half, aadt)) +
  geom_point()



## Statistical distance ----
# Comparing sample and population - do they look alike?

# Kji-kvadrat test (med MC)
test <- chisq.test(
  x,
  y,
  simulate.p.value = TRUE
)


# og
# Total Variation distance (not the  supremum definition, which is event-wise, rather the pd-one - easy to understand)
# Hellinger distance (between 0 and 1, more difficult, 0 to 1)
# Kullback-Leibler Divergence (too complicated, from 0 to Inf)


# Power analysis (least n) ----
# Say we want to detect wether traffic is changed, i.e. different from 0 % change.

# What effect size do we need to detect?
# Cohen's d is the difference between two means divided by a standard deviation for the data.
# If the difference is 1 %-point and the sd is about 5, then d is 0.2.
# What is the empirical sd in terms of %-points?

# Power: By what probability do we want to detect it? 0.9?
# Significance level is 0.05.
# We need to detect if change is unequal to zero, i.e. both positive and negative.
# What is the smallest sample size that fullfills this?

# What is the typical weighted sd under normal circumstances?
city_ids <- c(
  "8952",
  "1952",
  "955",
  "957",
  "953",
  "952",
  "959"
)

city_indices <-
  purrr::map(
    city_ids,
    ~ get_published_index(., 2022, 12)
  ) |>
  purrr::list_rbind()

city_indices_tidy <-
  city_indices |>
  dplyr::filter(
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    day_type == "ALL",
    period == "year_to_date"
  )

mean(city_indices_tidy$standard_deviation)
median(city_indices_tidy$standard_deviation)
# 4

cohen_d <- 2 / 4

# Choose wanted power and difference in mean
power_analysis <-
  pwr::pwr.t.test(
  n = NULL,
  d = cohen_d,
  sig.level = 0.05,
  power = 0.8,
  type = "one.sample",
  alternative = "two.sided"
)

power_analysis
plot(power_analysis)


# Ratio of mean and variance ----
# What distribution is traffic volume following?
# Depends on
# - time interval length
# - time interval type (morning, day, night, weekday, weekend)
# - lane type
# - area type (urban or rural)


# Log ratio ----
# An antisymmetric, additive and normed measure of change
# Either ln (p) or 2 * (sqrt(v_2) - sqrt(v_1))

point_index_tests <-
  tibble::tibble(
    base_volume = c(
      100,
      1000,
      10000,
      10200,
      1200,
      200,
      2000,
      2000,
      2000,
      20000,
      20050,
      20000,
      1000000,
      1000900,
      1000000,
      100
    ),
    calc_volume = c(
      200,
      1200,
      10200,
      10000,
      1000,
      100,
      2000,
      2050,
      2100,
      20050,
      20250,
      20250,
      1000900,
      1000000,
      1010000,
      101
    )
  ) |>
  dplyr::mutate(
    pi_i = calc_volume / base_volume,
    pi_p = 100 * (calc_volume / base_volume - 1),
    pi_i_ln = 100 * log(pi_i),
    f_pi_i_lambda_half = (calc_volume - base_volume) / sqrt(base_volume),
    F_pi_i_lambda_half = 2 * (sqrt(calc_volume) - sqrt(base_volume))
  ) |>
  dplyr::summarise(
    pi_i = sum(calc_volume) / sum(base_volume),
    total_F_pi_i_lambda_half = 2 * (sqrt(sum(calc_volume)) - sqrt(sum(base_volume))),
    mean_F_pi_i_lambda_half = mean(F_pi_i_lambda_half)
  )

sum(point_index_tests$f_pi_i_lambda_half[10:11])
sum(point_index_tests$F_pi_i_lambda_half[10:11])

# I.e. F is additive, but f is not.
# F has high values when comparing monthly traffic, but this will of course be smaller when we would compare monthly daily traffic.

# F for the city doesn't say much. It would still be perilous to compare cities.


# Traffic links from Adm ----

#layers <- sf::st_layers("C:/Users/snohan/Desktop/traffic_links_2023_2024-10-08.geojson")
#names(links)

links <-
  sf::st_read(
    #"C:/Users/snohan/Desktop/traffic_links_2023_2024-10-11.geojson",
    "C:/Users/snohan/Desktop/traffic_links_2024_2025-01-28.geojson",
    as_tibble = TRUE
    #query = "SELECT * FROM \"traffic_links_2024_2025-01-28\" LIMIT 150"
  ) |>
  dplyr::select(
    link_id = id,
    roadSystemReferences,
    from = startTrafficNodeId,
    to = endTrafficNodeId,
    municipalityIds,
    associatedTrpIds,
    hasOnlyPublicTransportLanes,
    isFerryRoute,
    function_class = functionClass,
    length,
    trafficVolumes
  ) |>
  dplyr::mutate(
    function_class = as.factor(function_class)
  ) |>
  dplyr::filter(
    hasOnlyPublicTransportLanes == FALSE,
    isFerryRoute == FALSE
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    trp_id = stringr::str_extract_all(associatedTrpIds, "(?<=\")[:alnum:]+(?=\")")
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    # From list to character
    point_id = purrr::map(trp_id, ~ purrr::pluck(., 1)) # NB! What if there is more than one!
  ) |>
  tidyr::unnest(
    point_id,
    keep_empty = TRUE
    # TODO: remove periodic trps
  ) |>
  dplyr::mutate(
    point_id =
      dplyr::case_when(
        point_id %in% trp_continuous$trp_id ~ point_id,
        TRUE ~ NA_character_
      )
  ) |>
  dplyr::select(
    -hasOnlyPublicTransportLanes,
    -isFerryRoute,
    -associatedTrpIds,
    -trp_id
  ) |>
  sf::st_as_sf()

traffic_volumes <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id,
    trafficVolumes
  ) |>
  dplyr::filter(
    !is.na(trafficVolumes)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    traffic_volumes = list(jsonlite::fromJSON(trafficVolumes))
  ) |>
  # Need to remove empty lists before unnesting.
  dplyr::filter(
    purrr::map_int(list(traffic_volumes), ~length(.)) > 0
  ) |>
  tidyr::unnest(
    traffic_volumes
  ) |>
  dplyr::filter(
    year == 2023,
    trafficVolumeResolution == "ADT"
  ) |>
  dplyr::select(
    link_id,
    trafficVolumeValue,
    #year,
    #coverage,
    trafficWorkValue
    #correctedStandardError,
    #sourceType,
    #registrationFrequency
  ) |>
  dplyr::summarise(
    traffic_volume = mean(trafficVolumeValue) |> round(),
    traffic_work_km = mean(trafficWorkValue),
    .by = "link_id"
  )

nodes <-
  sf::st_read("C:/Users/snohan/Desktop/traffic-nodes-2023_2024-10-11.geojson") |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    #numberOfUndirectedLinks
  ) |>
  tibble::as_tibble()

all_municipality_ids <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id,
    all_municipality_ids = municipalityIds
  )

not_intersected <- function(x, y) !sf::st_intersects(x, y)

## City link population ----

### Nord-Jæren
# NB! Need to remove duplicate links (crossing municipality boundaries)
municipality_ids_nj <- c(1127, 1103, 1124, 1108)

nj_polygon_north <-
  tibble::tibble(
    lon = c(
      5.7866566,
      6.1565635,
      5.8272271,
      5.4658784
    ),
    lat = c(
      59.0119612,
      59.284531,
      59.3473959,
      59.0678125
    )
  ) |>
  tibble::rowid_to_column("id") |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("POLYGON")

links_nj_central <-
  links |>
  tidyr::unnest_longer(
    municipalityIds,
    values_to = "municipality_id"
  ) |>
  dplyr::filter(
    municipality_id %in% municipality_ids_nj,
    function_class %in% c("A", "B", "C", "D")
  ) |>
  dplyr::select(
    -municipality_id,
    -trafficVolumes
  ) |>
  dplyr::distinct() |>
  dplyr::left_join(
    all_municipality_ids,
    by = dplyr::join_by(link_id)
  ) |>
  dplyr::rowwise() |>
  dplyr:::mutate(
    not_border_crossing =
      purrr::map_lgl(
        unlist(all_municipality_ids),
        ~ all(. %in% municipality_ids_nj)
      ) |> all()
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    not_border_crossing == TRUE
  ) |>
  dplyr::left_join(
    traffic_volumes,
    by = join_by(link_id)
  ) |>
  dplyr::select(
    -not_border_crossing,
    -all_municipality_ids
  ) |>
  dplyr::filter(
    !is.na(traffic_work_km)
  ) |>
  sf::st_as_sf() |>
  sf::st_filter(nj_polygon_north, .predicate = not_intersected)


# Need to remove links with function class D in eastern part
nj_polygon_east <-
  tibble::tibble(
    lon = c(
      5.7653664,
      5.8236117,
      6.4870498,
      6.0794556
    ),
    lat = c(
      58.9062453,
      58.815903,
      58.8686332,
      59.0519824
    )
  ) |>
  tibble::rowid_to_column("id") |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("POLYGON")

links_nj_east_D <-
  links_nj_central |>
  sf::st_filter(nj_polygon_east, .predicate = st_intersects) #|>
  #dplyr::filter(
  #  function_class == "D"
  #)

links_nj_central_reduced <-
  links_nj_central |>
  dplyr::filter(
    !(link_id %in% links_nj_east_D$link_id),
    # Removing som links in Figgjo
    !(link_id == "0.0-1.0@320083"),
    !(link_id == "0.80529774-0.9731575@320669"),
    !(link_id == "0.72327267-0.80529774@320669"),
    !(link_id == "0.9731575-1.0@320669")
  )

# Look at missing data
# links_nj_missing <-
#   links_nj |>
#   dplyr::filter(
#     is.na(traffic_work)
#   )

# Map
links_nj_central_reduced |>
  map_links_with_function_class()

readr::write_rds(links_nj_central_reduced, "representativity/link_population_nj.rds")



## Rolling index ----
# TODO: use already checked MDT data to recalculate - differences will be
# - traffic work weights
# - finite population corrected CI

# Need traffic_work per link that has TRP in order to calculate weighted mean
mdt_tw <-
  links_nj_central_reduced |>
  sf::st_drop_geometry() |>
  dplyr::select(
    trp_id = point_id,
    traffic_work
  ) |>
  dplyr::filter(
    !is.na(trp_id)
  )


{
  present_year <- 2024
  index_month <- 12
  city_number <- 952
}

source("set_time_references.R")

mdt_filtered <-
  readr::read_rds(
    file =
      paste0(
        "data_indexpoints_tidy/mdt_",
        city_number,
        ".rds"
      )
  )

source("exclude_trp_mdts_list.R")

filter_mdt <- function(mdt_df, year_dbl) {

  mdt_df |>
    dplyr::filter(
      year == year_dbl,
      coverage >= 50, # this is length_coverage!
      length_quality >= 98.5
    ) |>
    dplyr::select(
      trp_id,
      year,
      month,
      mdt
    ) |>
    dplyr::group_by(
      trp_id,
      year
    ) |>
    dplyr::summarise(
      n_months = n(),
      mean_mdt = mean(mdt),
      .groups = "drop"
    ) |>
    dplyr::filter(
      n_months >= 10
    )

}


# For testing
base_year <- reference_year
#last_year_month <- "2024-12-01"
window_length <- 36
grouping <- "by_area"
mdt_df <- mdt_validated

test <- calculate_rolling_indices_by_mdt(reference_year, last_year_month, window_length, mdt_validated, grouping)

calculate_rolling_indices_by_mdt <-
  function(base_year, last_year_month, window_length, mdt_df, grouping) {

    # Window length is a number of months, a multiple of 12
    # Grouping must be either:
    # by_area
    # by_sub_area
    # by_trp

    least_number_of_month_enums <-
      dplyr::case_when(
        window_length == 36 ~ 2,
        TRUE ~ 0
      )

    least_number_of_months <-
      dplyr::case_when(
        window_length == 36 ~ 31,
        window_length == 24 ~ 20,
        window_length == 12 ~ 9
      )

    last_year_month <-
      lubridate::as_date(last_year_month)

    mean_mdt_in_window <-
      mdt_df |>
      dplyr::filter(
        year_month %in%
          base::seq.Date(
            from = last_year_month - base::months(window_length - 1),
            to = last_year_month,
            by = "month"
          )
      ) |>
      dplyr::filter(
        coverage >= 50,
        length_quality >= 98.5
      ) |>
      dplyr::group_by(
        trp_id,
        month
      ) |>
      dplyr::summarise(
        n_months = n(),
        mean_mdt = base::mean(mdt),
        .groups = "drop_last"
      ) |>
      dplyr::filter(
        n_months >= least_number_of_month_enums
      ) |>
      dplyr::group_by(
        trp_id
      ) |>
      dplyr::summarise(
        n_months = sum(n_months),
        mean_mdt = base::mean(mean_mdt),
        .groups = "drop"
      ) |>
      dplyr::filter(
        n_months >= least_number_of_months
      )

    index_df <-
      dplyr::inner_join(
        filter_mdt(mdt_df, base_year),
        mean_mdt_in_window,
        by = "trp_id"
      ) |>
      dplyr::mutate(
        w = mean_mdt.x / sum(mean_mdt.x),
        trp_index_i = mean_mdt.y / mean_mdt.x,
        #weigted_mean = sum(w * trp_index_i), # same as index_i :)
        index_i = sum(mean_mdt.y) / sum(mean_mdt.x),
        sd_component = w * (trp_index_i - index_i)^2
      )

    if(grouping == "by_area") {
      index_df_grouped <-
        index_df |>
        dplyr::summarise(
          index_i = sum(mean_mdt.y) / sum(mean_mdt.x),
          index_p = (index_i - 1) * 100,
          n_trp = n(),
          n_eff = 1 / sum(w^2),
          sd_sample_p = 100 * sqrt(sum(sd_component) * (1/(1 - 1/n_eff))),
          standard_error_p = sd_sample_p / sqrt(n_eff),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error_p, 1),
          ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error_p, 1)
        )
    }

    if(grouping == "by_sub_area") {
      index_df_grouped <-
        index_df |>
        dplyr::left_join(
          sub_areas,
          by = join_by(trp_id)
        ) |>
        dplyr::summarise(
          index_i = sum(mean_mdt.y) / sum(mean_mdt.x),
          index_p = (index_i - 1) * 100,
          n_trp = n(),
          n_eff = 1 / sum(w^2),
          sd_sample_p = 100 * sqrt(sum(sd_component) * (1/(1 - 1/n_eff))),
          standard_error_p = sd_sample_p / sqrt(n_eff),
          .by = sub_area
        ) |>
        dplyr::mutate(
          ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error_p, 1),
          ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error_p, 1)
        )
    }

    if(grouping == "by_trp") {
      index_df_grouped <-
        index_df
    }

    index_df_final <-
      index_df_grouped |>
      dplyr::mutate(
        index_period =
          paste0(
            base_year,
            " - (",
            (last_year_month - base::months(window_length - 1)) |>
              lubridate::month(label = TRUE),
            " ",
            (last_year_month - base::months(window_length - 1)) |>
              lubridate::year(),
            " - ",
            last_year_month |>
              lubridate::month(label = TRUE),
            " ",
            last_year_month |>
              lubridate::year(),
            ")"
          ),
        month_object = last_year_month
      ) |>
      dplyr::mutate(
        month_n = lubridate::month(month_object),
        year = lubridate::year(month_object),
        window = paste0(window_length, "_months")
      )

    return(index_df_final)

  }


calculate_rolling_indices <- function(window_length, grouping = "by_area") {

  base::stopifnot(window_length %% 12 == 0)

  n_years <- window_length / 12

  first_possible_year_month <-
    lubridate::as_date(
      paste0(
        reference_year + n_years,
        "-12-01"
      )
    )

  year_months_possible <-
    base::seq.Date(
      from = first_possible_year_month,
      to = last_year_month,
      by = "month"
    )

  purrr::map_dfr(
    year_months_possible,
    ~ calculate_rolling_indices_by_mdt(reference_year, .x, window_length, mdt_validated, grouping)
  )

}


# TODO: include MDT for 2023 and 2024 and calculate index
# TODO: alternative selections of TRPs
# 1. Original 24
# 2. Suggested 59
# 3. Repeated random selection of n


# City TRPs
links_with_city_trp_nj <-
  links_nj |>
  sf::st_drop_geometry() |>
  tidyr::unnest_longer(
    trp_id
  ) |>
  dplyr::inner_join(
    city_trp_info |>
      dplyr::filter(
        city_names == "Nord-Jæren"
      ),
    by = join_by(trp_id == p_id)
  ) |>
  dplyr::select(
    link_id,
    city_trp_id = trp_id
  )

links_nj_final <-
  links_nj |>
  dplyr::left_join(
    links_with_city_trp_nj,
    by = join_by(link_id)
  ) |>
  dplyr::select(
    -trp_id
  ) |>
  sf::st_as_sf()



## Traffic graph ----
# Create graph
nodes_nj <-
  dplyr::bind_rows(
    nodes |>
      dplyr::filter(
        id %in% links_nj_final$from
      ),
    nodes |>
      dplyr::filter(
        id %in% links_nj_final$to
      )
  ) |>
  dplyr::distinct() |>
  tibble::as_tibble() |>
  tibble::rowid_to_column("id_int") |>
  dplyr::select(
    id = id_int,
    id_original = id
  )

links_nj_final_plain <-
  links_nj_final |>
  sf::st_drop_geometry() |>
  dplyr::left_join(
    nodes_nj,
    by = join_by(from == id_original)
  ) |>
  dplyr::rename(
    from_int = id
  ) |>
  dplyr::left_join(
    nodes_nj,
    by = join_by(to == id_original)
  ) |>
  dplyr::rename(
    to_int = id
  ) |>
  dplyr::select(
    -from,
    -to
  ) |>
  dplyr::rename(
    from = from_int,
    to = to_int
  ) |>
  # dplyr::mutate(
  #   edge_id = paste0(from, "_", to)
  # ) |>
  dplyr::arrange(
    from
  ) |>
  dplyr::mutate(
    city_trp =
      dplyr::case_when(
        is.na(city_trp_id) ~ 0.5,
        TRUE ~ 1
      )
  ) |>
  dplyr::select(
    -city_trp_id
  ) |>
  dplyr::left_join(
    traffic_volumes,
    by = join_by(link_id)
  )

graph <-
  tidygraph::tbl_graph(
    nodes = nodes_nj,
    node_key = "id",
    edges = links_nj_final_plain,
    directed = FALSE
  )


# Verify that graph contains one single component
components <- igraph::components(graph)
components$csize

# If more than one, keeping the largest
graph_tidy <- igraph::largest_component(graph)

# NB! Node ids are being reset to 1:n
igraph::E(graph_tidy)
igraph::edge_attr(graph_tidy)
edges <- igraph::as_edgelist(graph_tidy)

# Or, if more than one
# biggest_component_id <- which.max(components$csize)
#
# nodes_in_biggest_component <-
#   nodes_nj |>
#   dplyr::mutate(
#     component_membership = components$membership
#   ) |>
#   dplyr::filter(
#     component_membership == biggest_component_id
#   )
#
# graph_tidy <- igraph::induced_subgraph(graph, nodes_in_biggest_component$id)
#igraph::isomorphic(test, graph_tidy)


# Plot
ggraph(graph, layout = 'auto') +
  geom_edge_link(
    aes(
      edge_colour = functional_road_class,
      edge_width = city_trp
    )
  ) +
  geom_node_point()
  #paletteer::scale_color_paletteer_d("LaCroixColoR::PeachPear")

igraph::diameter(graph)

# Clusters
g_clusters <- igraph::cluster_edge_betweenness(graph)
g_clusters$membership
sizes(g_clusters)
plot(g_clusters, graph)


# Centrality
G_graph <-
  graph |>
  igraph::set_edge_attr(
    name = "edge_betweenness",
    value = igraph::edge_betweenness(graph)
  )

igraph::edge_attr(G_graph)

# Plot
ggraph(G_graph, layout = 'auto') +
  geom_edge_link(
    aes(
      edge_colour = edge_betweenness,
      edge_width = city_trp
    )
  ) +
  geom_node_point()



## Line graph ----
# I.e. links will be nodes and vice versa.
# This in order to do analysis on links' spread and closeness

# line_graph <-
#   igraph::make_line_graph(graph) |>
#   tidygraph::as_tbl_graph()
# But edge attributes lostfrom original graph to the nodes in line graph

# ggraph(line_graph, layout = 'auto') +
#   geom_edge_link() +
#   geom_node_point()
#
# igraph::diameter(line_graph)
# igraph::E(line_graph)
#igraph::vertex_attr(line_graph)


# Buliding a line graph manually to retain attributes
# Let the original traffic link graph be G, and let the line graph be L.
# Links in G will be nodes in L. Nodes in L will be connected if they were adjacent in G.
L_nodes <-
  links_nj_final_plain |>
  tibble::rowid_to_column("id") |>
  dplyr::mutate(
    city_trp_lgl = city_trp == 1
  ) |>
  dplyr::select(
    -from,
    -to
  )

# 1. Starting with the links in G, find which node ids they were connected to:
links_nj_stripped <-
  links_nj_final_plain |>
  dplyr::select(
    link_id, from, to
  ) |>
  tidyr::pivot_longer(
    cols = c(from, to),
    names_to = "new_link_id",
    values_to = "connection_id"
  ) |>
  dplyr::select(
    -new_link_id
  )

# 2. To find adjacent links in G, self join the list of links and their node ids:
L_links <-
  links_nj_stripped |>
  dplyr::left_join(
    links_nj_stripped,
    by = join_by(connection_id),
    relationship = "many-to-many"
  ) |>
  # Must remove self loops
  dplyr::filter(
    !(link_id.x == link_id.y)
  ) |>
  # Must remove duplicates
  dplyr::rowwise() |>
  dplyr::mutate(
    sorted_id = list(c(link_id.x, link_id.y)) |> purrr::map(sort)
  ) |>
  dplyr::mutate(
    new_from = purrr::pluck(sorted_id, 1, 1),
    new_to = purrr::pluck(sorted_id, 2, 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    connection_id, # need to have this here to not discard parallell links
    new_from,
    new_to
  ) |>
  dplyr::distinct() |>
  dplyr::select(
    -connection_id
  ) |>
  # Add new node ids
  dplyr::left_join(
    L_nodes |>
      dplyr::select(
        id, link_id
      ),
    by = join_by(new_from == link_id)
  ) |>
  dplyr::rename(
    from = id
  ) |>
  dplyr::left_join(
    L_nodes |>
      dplyr::select(
        id, link_id
      ),
    by = join_by(new_to == link_id)
  ) |>
  dplyr::select(
    from,
    to = id
  )


# 3. Build the graph
L_graph <-
  tidygraph::tbl_graph(
    nodes = L_nodes,
    node_key = "id",
    edges = L_links,
    directed = FALSE
  )

ggraph(L_graph, layout = 'auto') +
  geom_edge_link() +
  geom_node_point(
    aes(
      color = functional_road_class,
      size = city_trp
    )
  )

igraph::diameter(L_graph)
igraph::vertex_attr(L_graph)
degrees <- igraph::degree_distribution(L_graph)



## Node coverage ----
percentage_nodes_sampled <- nrow(links_with_city_trp_nj) / nrow(links_nj_final_plain)

# Including nearest neighbors
selected_neighbors <-
  L_nodes |>
  dplyr::rowwise() |>
  dplyr::mutate(
    neighbors = list(igraph::neighbors(L_graph, id))
  ) |>
  dplyr::filter(
    city_trp == 1
  )

selected_nodes_and_neighbors <-
  selected_neighbors |>
  tidyr::unnest(
    neighbors
  ) |>
  dplyr::select(
    nodes = neighbors
  ) |>
  dplyr::mutate(
    nodes = as.numeric(nodes)
  ) |>
  dplyr::bind_rows(
    selected_neighbors |>
      dplyr::select(
        nodes = id
      )
  ) |>
  dplyr::distinct()

percentage_nodes_and_neighbors_sampled <- nrow(selected_nodes_and_neighbors) / nrow(links_nj_final_plain)


## Mean distance to chosen nodes ----
igraph::mean_distance(L_graph)

non_selected_nodes <-
  L_nodes |>
  dplyr::filter(
    city_trp < 1
  )

distances <-
  igraph::distances(
    L_graph,
    v = non_selected_nodes$id,
    to = selected_neighbors$id
  ) |>
  tibble::as_tibble()

shortest_distances <-
  distances |>
  dplyr::mutate(
    shortest = purrr::pmap_dbl(distances, min)
  )

mean_shortest_distances <- mean(shortest_distances$shortest)
sd_shortest_distances <- sd(shortest_distances$shortest)
max_shortest_distances <- max(shortest_distances$shortest)


## Weighted coverage ----
# Traffic work is what counts
percentage_traffic_work <-
  L_nodes |>
   dplyr::summarise(
    traffic_work = sum(traffic_work, na.rm = TRUE),
    .by = "city_trp_lgl"
  ) |>
  tidyr::pivot_wider(
    names_from = "city_trp_lgl",
    names_prefix = "trp_",
    values_from = "traffic_work"
  ) |>
  dplyr::mutate(
    total_tw = sum(trp_TRUE, trp_FALSE),
    percentage_tw = trp_TRUE / total_tw
  )


## Compare distributions ----
# Traffic work
L_nodes |>
  ggplot2::ggplot(aes(traffic_work)) +
  geom_histogram() +
  facet_wrap(
    #~ city_trp_lgl,
    ~ functional_road_class,
    ncol = 1)
# Large scale of traffic work - useless


# Functional road class
L_nodes |>
  dplyr::summarise(
    n = n(),
    .by = c(city_trp_lgl, functional_road_class)
  ) |>
  tidyr::pivot_wider(
    names_from = city_trp_lgl,
    names_prefix = "trp_",
    values_from = n
  ) |>
  dplyr::arrange(
    functional_road_class
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    trp_all = sum(trp_FALSE, trp_TRUE, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    percentage_TRUE = trp_TRUE / sum(trp_TRUE, na.rm = TRUE),
    percentage_all = trp_all / sum(trp_FALSE, trp_TRUE, na.rm = TRUE)
  )

L_nodes |>
  ggplot2::ggplot(aes(functional_road_class)) +
  geom_bar() +
  facet_wrap(
    ~ city_trp_lgl,
    ncol = 1)
