# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  library(tidyverse)
  library(readxl)
  library(corrplot)
  library(car)
  library(pwr)
  source("get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/traffic_link_functions.R")
}


# Link prep ----
## Traffic links without geometry ----
all_links <-
  jsonlite::fromJSON("H:/Trafikkdata/trafikklenker/traffic-links-2023.json") |>
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

all_links_tidy_no_duplicates <-
  all_links_tidy |>
  dplyr::select(
    -associatedTrpIds
  ) |>
  dplyr::distinct()

#
traffic_link_id_and_trp_id <-
  all_links_tidy |>
  dplyr::select(
    id,
    trp_id = associatedTrpIds
  ) |>
  dplyr::filter(
    !is.na(trp_id)
  )

## Toll stations ----
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


## Link stats overall ----
link_stats <-
  all_links_tidy_no_duplicates |>
  dplyr::summarise(
    n_links = n(),
    sum_traffic_work = sum(traffic_work_Mkm),
    .by = c(city)
  )


# Link variable distributions ----
# Look at how link variables are distributed

## AADT and traffic work ----
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


# Ratio of mean and variance
# What distribution is traffic volume following?
# Depends on
# - time interval length
# - time interval type (morning, day, night, weekday, weekend)
# - lane type
# - area type (urban or rural)


## Other variables ----
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


# Prediction variables for index ----
# Look for prediction variables when the traffic link index is the target variable.

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

readr::write_rds(
  link_index,
  "representativity/links_and_index.rds"
)


## Correlations numerical variables ----
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


## Correlation categorical variables ----
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
  dplyr::filter(
    city == "Bergen",
    function_class != "E"
  ) |>
  ggplot(aes(function_class, index)) +
  geom_boxplot(
    outliers = FALSE
  ) +
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
# Same analysis using F_lamba_half does not give different conclusions
# dplyr::mutate(
#   F_lambda_half = 2 * (sqrt(length_calc_volume_short) - sqrt(length_base_volume_short))
# )

# Check for normality
# unique_city_names <-
#   month_trp_index |>
#   dplyr::select(
#     area_name
#   ) |>
#   distinct()
#
# month_trp_index |>
#   # dplyr::filter(
#   #   area_name == unique_city_names$area_name[6]
#   # ) |>
#   ggplot(
#     aes(F_lambda_half)
#     #aes(index_short)
#   ) +
#   geom_histogram() +
#   facet_wrap(
#     facets = vars(month),
#     ncol = 3
#   )
#
# month_trp_index |>
#   # dplyr::filter(
#   #   area_name == unique_city_names$area_name[6]
#   # ) |>
#   ggplot(aes(sample = F_lambda_half)) +
#   stat_qq() +
#   stat_qq_line() +
#   facet_wrap(
#     facets = vars(month),
#     ncol = 3
#   )

# Looks fairly normal, and fairly same for index_short and F.

## Monthly index and links
# month_trp_index_links <-
#   month_trp_index |>
#   dplyr::left_join(
#     all_links_tidy_2,
#     by = join_by(trp_id == p_id)
#   ) |>
#   dplyr::select(
#     city,
#     #trp_id,
#     month,
#     #index_short,
#     F_lambda_half,
#     # Possible predictors, categorical
#     road_category,
#     urbanity = urbanRatio,
#     # minLanes,
#     # maxLanes,
#     # highestSpeedLimit,
#     functional_class,
#     function_class,
#     # Possible predictors, numerical
#     length,
#     aadt,
#     numberOfEstablishments,
#     numberOfEmployees,
#     numberOfInhabitants
#   ) |>
#   # Removing TRPs without match
#   dplyr::filter(
#     !is.na(road_category)
#   ) |>
#   dplyr::mutate(
#     #functional_class_max_lanes = paste(functional_class, maxLanes, sep = "_"),
#     #speed_min_lanes = paste(highestSpeedLimit, minLanes, sep = "_"),
#     #road_cat_urban = paste(road_category, urbanRatio, sep = "_"),
#     #function_class_min_lanes = paste(function_class, minLanes, sep = "_")
#     #speed_urban = paste(highestSpeedLimit, urbanRatio, sep = "_")
#     # min_lanes_groups =
#     #   dplyr::case_when(
#     #     minLanes %in% c(1, 2, 3) ~ "<4",
#     #     TRUE ~ ">=4"
#     #   ),
#     month = as.factor(month),
#     road_category =
#       dplyr::case_when(
#         road_category == "Europaveg" ~ "R",
#         road_category == "Riksveg" ~ "R",
#         road_category == "Fylkesveg" ~ "F"
#       ),
#     # speed_limit_grouped =
#     #   dplyr::case_when(
#     #     highestSpeedLimit %in% c(30, 40, 50) ~ "<=50",
#     #     highestSpeedLimit %in% c(60, 70, 80) ~ "<=80",
#     #     TRUE ~ ">80"
#     #   ),
#     urbanity =
#       dplyr::case_when(
#         urbanity %in% c("urban", "inter") ~ "urban",
#         TRUE ~ "rural"
#       ),
#     #traffic_work = length * aadt,
#     #interaction_term = paste(road_category, urbanity, sep = "_"),
#     length = log(length + 1),
#     aadt_group =
#       dplyr::case_when(
#         aadt < 5000 ~ "<5k",
#         aadt < 25000 ~ "<25k",
#         TRUE ~ ">25k"
#       ),
#     aadt = log(aadt + 1),
#     dplyr::across(
#       c(F_lambda_half, aadt, length, tidyselect::starts_with("number")),
#       ~ as.numeric(scale(.x))
#     )
#   )

# Not suitable to use speed or lanes here, as cities differ in occurrence.


# Categorical variables
# month_trp_index_links |>
#   ggplot(
#     aes(
#       x = aadt_group,
#       y = F_lambda_half
#     )
#   ) +
#   geom_boxplot() +
#   facet_wrap(facets = "city")
# No obvious groupings!
# Month is one, obviously.


# Numerical variables
# month_trp_index_links |>
#   dplyr::select(
#     F_lambda_half, aadt, length, tidyselect::starts_with("number")
#   ) |>
#   stats::cor(
#     method = "spearman"
#   ) |>
#   corrplot::corrplot(
#     type = "lower"
#   )
#
# month_trp_index_links |>
#   ggplot(aes(F_lambda_half, aadt)) +
#   geom_point()


# Prediction variables for traffic work ----

all_links_tidy_no_duplicates |>
  dplyr::filter(
    #city == "Bergen",
    function_class != "E"
  ) |>
  ggplot(aes(function_class, traffic_work_Mkm)) +
  geom_boxplot(
    outliers = FALSE
  ) +
  facet_wrap(facets = "city")

  all_links_tidy_no_duplicates |>
  ggplot(aes(function_class, aadt)) +
  geom_boxplot(
    outliers = FALSE
  )
