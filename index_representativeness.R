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
  source("get_from_trafficdata_api.R")
}


# RTM links ----
## Population: RTM links ----
read_rtm_data <- function(file_path) {

  readxl::read_excel(
    file_path#,
    #n_max = 500
    ) |>
    dplyr::select(
      start = A,
      end = B,
      distance = DISTANCE,
      road_category_id = LINKTYPE,
      road_ref = VEGREFERANS,
      from_meter = FROMMETER,
      to_meter = TOMETER,
      delstrekning = DELSTREKN,
      VS,
      VEGTYPE,
      direction = NVDBRETNING,
      aadt = CD_ADT, # Car driver AADT per direction
      ydt_apbasert = CD_APBASERT,
      ydt_arbeid = CD_ARBEID,
      ydt_flyplass = CD_FLYPLASS,
      ydt_fritid = CD_FRITID,
      ydt_hentlev = CD_HENTLEV,
      ydt_ntm6 = CD_NTM6,
      ydt_privat = CD_PRIVAT,
      ydt_skole = CD_SKOLE,
      ydt_sverige = CD_SVERIGE,
      ydt_tjeneste = CD_TJENESTE
      # TODO: calculate work for each intention
    ) |>
    dplyr::filter(
      aadt > 0,
      road_category_id %in% c(1, 2, 3),
      delstrekning < 100, # keep only roads for motor vehicles
      VS == "V", # only existing roads
      VEGTYPE != 19290 # no roundabouts
    ) |>
    # Summing directions
    dplyr::rowwise() |>
    dplyr::mutate(
      id = list(stringr::str_sort(c(start, end))) |>
        purrr::map_chr(paste, collapse = "_"),
      meters = list(stringr::str_sort(c(from_meter, to_meter), numeric = TRUE)) |>
        purrr::map_chr(paste, collapse = "_"),
      traffic_work = distance * aadt
    ) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      aadt = sum(aadt),
      traffic_work = sum(traffic_work),
      .by = c(id, meters, road_ref)
    ) |>
    tidyr::separate_wider_delim(
      cols = id,
      delim = "_",
      names = c("from_id", "to_id"),
      too_few = "align_start"
    ) |>
    tidyr::separate_wider_delim(
      cols = meters,
      delim = "_",
      names = c("from_meter", "to_meter"),
      too_few = "align_start"
    ) |>
    dplyr::mutate(
      from_meter = base::as.numeric(from_meter),
      to_meter = base::as.numeric(to_meter)
    ) |>
    dplyr::select(
      from_id,
      to_id,
      road_ref,
      from_meter,
      to_meter,
      aadt,
      traffic_work
    ) |>
    dplyr::arrange(
      road_ref,
      from_meter
    ) |>
    dplyr::summarise(
      from_id = dplyr::first(from_id),
      to_id = dplyr::last(to_id),
      traffic_work = sum(traffic_work),
      from_meter = min(from_meter),
      to_meter = max(to_meter),
      .by = c(road_ref, aadt)
    )

}

# column_names <-
#   readxl::read_excel(
#     "rtm/rtm_trondheim_2020_ny.xlsx",
#     n_max = 1
#   )
#
# names(column_names) |>
#   stringr::str_sort()

rtm_trd_20 <- read_rtm_data("rtm/rtm_trondheim_2020_ny.xlsx")
rtm_trd_22 <- read_rtm_data("rtm/rtm_trondheim_2022_ny.xlsx")

# Lenketype 30 er sonetilknytning og cd_tot angir antall turer.

rtm_20_22 <-
  dplyr::inner_join(
    rtm_trd_20,
    rtm_trd_22,
    by = dplyr::join_by(from_id, to_id, road_ref, from_meter, to_meter),
    suffix = c("_20", "_22")
  ) |>
  dplyr::mutate(
    index_i = aadt_22 / aadt_20,
    index_p = 100 * (index_i - 1)
  )

rtm_index <-
  rtm_20_22 |>
  dplyr::summarise(
    area_index_i = sum(aadt_22) / sum(aadt_20),
    area_index_p = 100 * (area_index_i - 1),
    mean_p = mean(index_p),
    sd_p = sd(index_p)
  )


## Sample: RTM links with TRP ----
city_trp <-
  readr::read_rds(
    file = paste0("data_indexpoints_tidy/indekspunkt_960.rds")
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    meter,
    intersection_meter
  ) |>
  dplyr::mutate(
    road_ref = stringr::str_replace(road_reference, "[:space:]m.*", ""),
    meter_join =
      dplyr::case_when(
        is.na(intersection_meter) ~ meter,
        TRUE ~ intersection_meter
      )
  )

rtm_trp <-
  dplyr::inner_join(
    city_trp,
    rtm_20_22,
    by = dplyr::join_by(
      road_ref == road_ref,
      dplyr::between(meter_join, from_meter, to_meter)
    )
  )

# TODO: sort out trouble caused by missing intersection info in RTM data

## Sample: City TRP index ----
# Need to find TRP index 2020-2022, so will use chaining
city_trp_index <-
  readr::read_rds(
    file = paste0("data_indexpoints_tidy/indekspunkt_960.rds")
  ) |>
  dplyr::select(
   trp_id,
    name,
    road_reference,
    station_type_short,
    index_2021,
    index_2022
  ) |>
  dplyr::filter(
    dplyr::if_all(
      .cols = tidyselect::starts_with("index"),
      .fns = ~ !is.na(.x)
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::starts_with("index"),
      .fns = ~ (. / 100) + 1)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(index = prod(c_across(tidyselect::starts_with("index")))) |>
  dplyr::mutate(index = round(100 * (index - 1), digits = 1))


## Comparison ----
# So we have a population and a sample. This may be seen as two samples.
# We need to know if this sample can be considered to come from the population.
# Thus we need to compare their distributions. Is there any assumptions in methods for comparing?
# If they do not resemble any known distribution, they may well need a transformation.
# https://book.stat420.org/transformations.html
# https://www.r-bloggers.com/2020/01/a-guide-to-data-transformation/
# http://fmwww.bc.edu/repec/bocode/t/transint.html

# Yeo-Johnson Transformation?

## Population ----
# 1900 data samples should ensure normality assumptions to be safe
fitdistrplus::descdist(rtm_20_22$index_p)
fitdistrplus::plotdist(rtm_20_22$index_p, demp = TRUE)

fit_norm <- fitdistrplus::fitdist(rtm_20_22$index_p, "norm")
plot(fit_norm)
#summary(fit_norm)
# Looks OK
moments::skewness(rtm_20_22$index_p)
# 23, i.e. high positive/right skew
# Should use transformation

moments::kurtosis(rtm_20_22$index_p)
# 620, i.e. very heavy tails (leptokurtic)
# Should use transformation for right skew

# Skedasticity?

# Plot ecdf
both_samples <-
  dplyr::bind_rows(
    city_trp_index |>
      dplyr::select(
        id = trp_id,
        index_p = index
      ) |>
      dplyr::mutate(
        source = "city_index"
      ),
    rtm_20_22 |>
      dplyr::select(
        id,
        index_p
      ) |>
      dplyr::mutate(
        source = "rtm"
      )
  )

ggplot2::ggplot(
  both_samples,
  aes(index_p, colour = source)
) +
  stat_ecdf()

# Trying
# fit_norm_2 <- gamlss::fitDist(rtm_20_22$index_p, trace = FALSE, try.gamlss = TRUE)
# summary(fit_norm_2)
# Supposedly, too many data points lead this wrong.


# These fail because of large sample size
#stats::shapiro.test(rtm_20_22$index_p)
#stats::ks.test(rtm_20_22$index_p, pnorm)


## Sample ----
fitdistrplus::descdist(city_trp_index$index)
fitdistrplus::plotdist(city_trp_index$index, demp = TRUE)

fit_norm_s <- fitdistrplus::fitdist(city_trp_index$index, "norm")
plot(fit_norm_s)
summary(fit_norm_s)
# Heavy tails, as expected.


## Test ----
### Mean ----
stats::t.test(
  city_trp_index$index,
  mu = rtm_index$mean_p
  )

# Two sample Kolmogorov-Smirnov
stats::ks.test(
  city_trp_index$index,
  rtm_20_22$index_p
)

stats::wilcox.test(
  city_trp_index$index,
  rtm_20_22$index_p
)

stats::qqplot(
  city_trp_index$index,
  rtm_20_22$index_p
)

# TODO: Wasserstein metric
# transport::wasserstein
# emdist::emd
# TODO: Anderson-Darling
# TODO: Cramer-von Mises

# Possible distance metrics:
# Hellinger distance
# Wasserstein metric (earth mover) beware of order of categories - if it shouldn't matter, use another metric
# Kullback–Leibler divergence (relative entropy)
# Kolmogorov-Smirnov Test
# Cramer-von Mises test
# Jensen-Shannon Divergence
# Population Stability Index

### Variance ----
# F-test, but then distributions must be normal!
stats::var.test(rtm_20_22$index_p, city_trp_index$index)

# Non-parametric tests
#Levene’s test: A robust alternative to the Bartlett’s test that is less sensitive to departures from normality.
#Fligner-Killeen’s test: a non-parametric test which is very robust against departures from normality.
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/

stats::fligner.test(list(rtm_20_22$index_p, city_trp_index$index))

# CI for SD
base::sqrt(
  base::nrow(city_trp_index) * stats::var(city_trp_index$index) /
    stats::qchisq(c(0.975, 0.025), nrow(city_trp_index))
  )

# TODO: log transformation? Box-Cox?


# Look at RTM at position of TRP

# TODO: Make a Quarto report on this. Important to mention assumptions and prerquisites as well as weaknesses.
# TODO: Lag en felles presentasjon med May-Berit og Jonas.
# TODO: Presenter i ett av arbeidsgruppemøtene for Trafikkdata by.
# TODO: Finn andel av alle turer som fanges opp av minst ett punkt. Hva da med overrepresentasjon?


# Traffic links ----
# 1. Look for prediction variables when the point/link index is the target variable.
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
  )|>
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
    associatedTollStationIds,
    length,
    minLanes,
    highestSpeedLimit,
    functional_class,
    function_class,
    aadt = trafficVolumeValue,
    urbanRatio,
    numberOfEstablishments,
    numberOfEmployees,
    numberOfInhabitants
  ) |>
  tidyr::unnest(
    municipalityIds
  ) |>
  dplyr::mutate(
    city =
      dplyr::case_when(
        municipalityIds %in% c(5001, 5028, 5029, 5059, 5031, 5035) ~ "Trondheim",
        municipalityIds %in% c(4601, 4626, 4627, 4631, 4624) ~ "Bergen",
        municipalityIds %in% c(1127, 1103, 1124, 1108) ~ "Nord-Jæren", # should Stavanger and Sandnes be limited?
        municipalityIds %in% c(301, 3205, 3207, 3201, 3203, 3218, 3222, 3224, 3232) ~ "Oslo" # Which should be included?
      )
  ) |>
  dplyr::select(
    -municipalityIds
  ) |>
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
  tidyr::unnest(
    # Gives no duplicates
    associatedTollStationIds,
    keep_empty = TRUE
  ) |>
  dplyr::mutate(
    associatedTollStationIds = as.character(associatedTollStationIds),
    road_category = factor(road_category),
    minLanes = factor(minLanes),
    highestSpeedLimit = factor(highestSpeedLimit),
    functional_class = factor(functional_class),
    function_class = factor(function_class)
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

# Putting toll ids in same column as trp_id
all_links_tidy_2 <-
  dplyr::bind_rows(
    all_links_tidy |>
      dplyr::select(
        -associatedTollStationIds
      ) |>
      dplyr::rename(
        p_id = associatedTrpIds
      ),
    all_links_tidy |>
      dplyr::select(
        -associatedTrpIds
      ) |>
      dplyr::rename(
        p_id = associatedTollStationIds
      ) |>
      dplyr::filter(
        !is.na(p_id)
      )
  )


# TODO: reduce area to true interest area (some municipalities stretch far inland to remote places, e.g. Sandnes)
# TODO: links on municipality roads


## Point index ----
# TODO: new method for point (link) index, based on traffic work?

# Choosing year index 2023
# TRD
# Need toll station nvdb id to join with links
trd_toll_ids <-
  readr::read_rds(
    file = "bomdata_trondheim/trd_toll_stations.rds"
  ) |>
  dplyr::select(
    trp_id,
    p_id = nvdb_id
  ) |>
  dplyr::mutate(
    p_id = as.character(p_id)
  )

trd_trp_index <-
  readr::read_rds(
    file = paste0("data_indexpoints_tidy/indekspunkt_", 960, ".rds")
  ) |>
  dplyr::select(
    trp_id,
    index = index_2023
  ) |>
  # Since Ranheim has change number form 72 to 1
  dplyr::mutate(
    trp_id = dplyr::case_when(
      trp_id == "72" ~ "1",
      TRUE ~ trp_id
    ),
    p_id = trp_id
  ) |>
  dplyr::rows_update(
    trd_toll_ids,
    by = "trp_id"
  ) |>
  dplyr::filter(
    # Removing toll station "Nord for Sluppen bru" which has been moved
    !(p_id == "1017875672")
  ) |>
  dplyr::select(
    -trp_id
  )

# Other cities
trp_index_raw <-
  purrr::map(
    c(952, 8952, 959),
    ~ get_published_pointindex_for_months(.x, 2023, 12)[[2]]
  ) |>
  purrr::list_rbind()

trp_index <-
  trp_index_raw |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "year_to_date"
  ) |>
  dplyr::slice_max(
    order_by = month,
    by = c(trp_id, year)
  ) |>
  dplyr::select(
    p_id = trp_id,
    index = index_short
  ) |>
  dplyr::bind_rows(
    trd_trp_index
  ) |>
  # remove outliers
  dplyr::filter(
    index < 15
  )

## City TRPs ----
city_ids <-
  c(960, 952, 8952, 959)

city_trp_info <-
  purrr::map(
    city_ids,
    ~ readr::read_rds(
      file = paste0(
        "index_trp_metadata/trp_",
        .x,
        ".rds"
      )
    )
  ) |>
  dplyr::bind_rows() |>
  dplyr::select(
    trp_id,
    name
  ) |>
  dplyr::mutate(
    trp_id = dplyr::case_when(
      trp_id == "72" ~ "1",
      TRUE ~ trp_id
    ),
    p_id = trp_id
  ) |>
  dplyr::rows_update(
    trd_toll_ids,
    by = "trp_id"
  ) |>
  dplyr::filter(
    # Removing toll station "Nord for Sluppen bru" which has been moved
    !(p_id == "1017875672")
  ) |>
  dplyr::select(
    -trp_id
  )


## Links and point index ----
city_links <-
  all_links_tidy_2 |>
  dplyr::left_join(
    trp_index,
    by = dplyr::join_by(p_id)
  ) |>
  # Joining city TRPs, whence "name" becomes the tag for it
  dplyr::left_join(
    city_trp_info,
    by = dplyr::join_by(p_id)
  )

# TODO: remove duplicate rows
# 1. All rows with an index must have a name, so we keep all rows with name
# 2. Then take all other rows that does not have id in 1
#dups <- city_links[duplicated(city_links$id),]

city_links_with_trp <-
  city_links |>
  dplyr::filter(
    !is.na(name)
  ) |>
  dplyr::select(
    -p_id
  ) |>
  dplyr::distinct()
#dups <- city_links_with_trp[duplicated(city_links_with_trp$id),]

city_links_without_trp <-
  city_links |>
  dplyr::filter(
    !(id %in% city_links_with_trp$id)
  ) |>
  dplyr::select(
    -p_id
  ) |>
  dplyr::distinct()
#dups <- city_links_without_trp[duplicated(city_links_without_trp$id),]

city_links_tidy <-
  dplyr::bind_rows(
    city_links_with_trp,
    city_links_without_trp
  )
dups <- city_links_tidy[duplicated(city_links_tidy$id),]


## Categorical variables ----
city_links_tidy |>
  purrr::keep(is.factor) |>
  names()

# city_links_tidy |>
#   ggplot(aes(x = road_category)) +
#   geom_histogram(stat = "count") +
#   theme_minimal() +
#   facet_wrap(~ city)

city_links_tidy |>
  ggplot(
    aes(
      x = road_category,
      y = index,
      color = city
    )
  ) +
  geom_jitter()
# No obvious grouping per road_category

city_links_tidy |>
  ggplot(
    aes(
      x = road_category,
      y = numberOfEstablishments,
      color = city
    )
  ) +
  geom_jitter()

city_links_tidy |>
  ggplot(
    aes(
      x = minLanes,
      y = index,
      color = city
    )
  ) +
  geom_jitter()
# No obvous trend.

city_links_tidy |>
  ggplot(
    aes(
      x = highestSpeedLimit,
      y = index,
      color = city
    )
  ) +
  geom_jitter()
# No obvous trend.

city_links_tidy |>
  dplyr::select(
    highestSpeedLimit,
    minLanes
  ) |>
  table() |>
  t() |>
  prop.table(margin = 2) |>
  barplot(
    legend.text = TRUE
  )

city_links_tidy |>
  ggplot(
    aes(
      x = functional_class,
      y = index
    )
  ) +
  geom_jitter()

city_links_tidy |>
  ggplot(
    aes(
      x = function_class,
      y = index
    )
  ) +
  geom_jitter()

city_links_tidy |>
  ggplot(
    aes(
      x = urbanRatio,
      y = index
    )
  ) +
  geom_jitter()

# ANOVA
urban_aov <-
  stats::aov(
    index ~ urbanRatio,
    data = city_links_tidy
  )

summary(urban_aov)

hist(urban_aov$residuals)
shapiro.test(urban_aov$residuals)
# fairly normal

boxplot(
  index ~ urbanRatio,
  data = city_links_tidy
  )

# Equal variances?
car::leveneTest(
  index ~ urbanRatio,
  data = city_links_tidy
)
# No
stats::oneway.test(
  index ~ urbanRatio,
  data = city_links_tidy,
  var.equal = FALSE
)
# Groups do not have significantly different means


# TODO: Select features as is? Or transform?
# How to rank variables?
# Should set up a prediction model in order to be able to really select the most important features?


## Numeric variables ----
# Correlations among variables
city_links_tidy |>
  purrr::keep(is.numeric) |>
  stats::cor(
    method = "spearman"
  ) |>
  corrplot::corrplot(
    type = "lower"
  )

# High correlation between
# between all numberOf...
# urban ratio and any of numberOf...


# Correlation with index variable
city_links_tidy |>
  purrr::keep(is.numeric) |>
  dplyr::filter(
    !is.na(index)
  ) |>
  stats::cor(
    method = "spearman"
  ) |>
  corrplot::corrplot(
    type = "lower"
  )

# Possible predictors:
# numberOfEstablishments

city_links_tidy |>
  ggplot(
    aes(
      x = aadt,
      y = index
    )
  ) +
  geom_point()
# No trend to see, but here I suspect something should be hiding...

city_links_tidy |>
  ggplot(
    aes(
      x = numberOfEstablishments,
      y = index
    )
  ) +
  geom_jitter()


# TODO: dig more after predictors, cuz i need some!

# Final set of predictors
city_links_tidy_select <-
  city_links_tidy |>
  dplyr::select(
    road_category,
    function_class,
    functional_class,
    aadt,
    urbanRatio,
    index
  )


## Distance metrics ----
# Comparing sample and population - do they look alike?

