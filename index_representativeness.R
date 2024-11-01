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
  source("get_from_trafficdata_api.R")
}

# Steps ----
# 1. Define the population
# 2. Find relevant characteristics of the population and describe these
# 3. Describe the sampling process
# 4. Compare sample characteristics with the population using measures of fit
# 5. Decide on acceptance criteria for similarity


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

read_rtm_data_purpose <- function(file_path) {

  readxl::read_excel(file_path) |>
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
      #starts_with("CD_")
      ndt = CD_ADT, # Car driver AADT per direction (normalvirkedøgn)
      ndt_apbasert = CD_APBASERT,
      ndt_arbeid = CD_ARBEID,
      ndt_flyplass = CD_FLYPLASS,
      ndt_fritid = CD_FRITID,
      ndt_hentlev = CD_HENTLEV,
      ndt_ntm6 = CD_NTM6,
      ndt_privat = CD_PRIVAT,
      ndt_skole = CD_SKOLE,
      ndt_sverige = CD_SVERIGE,
      ndt_tjeneste = CD_TJENESTE
      # TODO: calculate work for each intention
    ) |>
    dplyr::filter(
      ndt > 0,
      road_category_id %in% c(1, 2, 3),
      delstrekning < 100, # keep only roads for motor vehicles
      #VS %in% c("V"), # only existing roads
      VEGTYPE != 19290 # no roundabouts
    ) |>
    # Summing directions
    dplyr::rowwise() |>
    dplyr::mutate(
      id = list(stringr::str_sort(c(start, end))) |>
        purrr::map_chr(paste, collapse = "_"),
      meters = list(stringr::str_sort(c(from_meter, to_meter), numeric = TRUE)) |>
        purrr::map_chr(paste, collapse = "_"),
      ndt_privat = ndt_privat + ndt_apbasert + ndt_flyplass + ndt_fritid + ndt_hentlev + ndt_skole + ndt_sverige + ndt_ntm6,
      # Including ntm6 here, which is data from national model. It includes some work related trips.
      ndt_arbeid = ndt_arbeid + ndt_tjeneste,
      ndt_total = ndt_privat + ndt_arbeid,
      traffic_work_total = distance * ndt_total,
      traffic_work_privat = distance * ndt_privat,
      traffic_work_arbeid = distance * ndt_arbeid
    ) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      #ndt_privat = sum(ndt_privat),
      #ndt_arbeid = sum(ndt_arbeid),
      ndt_total = sum(ndt_total),
      traffic_work_privat = sum(traffic_work_privat),
      traffic_work_arbeid = sum(traffic_work_arbeid),
      traffic_work_total = sum(traffic_work_total),
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
      ndt_total,
      starts_with("traffic_work")
    ) |>
    dplyr::arrange(
      road_ref,
      from_meter
    ) |>
    dplyr::summarise(
      from_id = dplyr::first(from_id),
      to_id = dplyr::last(to_id),
      traffic_work_privat = sum(traffic_work_privat) / sum(traffic_work_total),
      #traffic_work_arbeid = sum(traffic_work_arbeid) / sum(traffic_work_total),
      traffic_work_total = sum(traffic_work_total),
      from_meter = min(from_meter),
      to_meter = max(to_meter),
      .by = c(road_ref, ndt_total)
    ) |>
    dplyr::mutate(
      ndt_total = round(ndt_total)
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

# TODO: Make a Quarto report on this. Important to mention assumptions and prerequisites as well as weaknesses.
# TODO: Lag en felles presentasjon med May-Berit og Jonas.
# TODO: Presenter i ett av arbeidsgruppemøtene for Trafikkdata by.
# TODO: Finn andel av alle turer som fanges opp av minst ett punkt. Hva da med overrepresentasjon?


# RTM travel purpose ----
rtm_trd_20_purpose <- read_rtm_data_purpose("rtm/rtm_trondheim_2020_ny.xlsx")
rtm_trd_22_purpose <- read_rtm_data_purpose("rtm/rtm_trondheim_2022_ny.xlsx")

rtm_20_22_purpose <-
  dplyr::inner_join(
    rtm_trd_20_purpose,
    rtm_trd_22_purpose,
    by = dplyr::join_by(from_id, to_id, road_ref, from_meter, to_meter),
    suffix = c("_20", "_22")
  ) |>
  dplyr::mutate(
    F_metric = 2 * (sqrt(traffic_work_total_22) - sqrt(traffic_work_total_20)),
    index_i = traffic_work_total_22 / traffic_work_total_20,
    index_p = 100 * (index_i - 1)
  ) |>
  dplyr::select(
    from_id, to_id, road_ref, from_meter, to_meter,
    starts_with("ndt"),
    starts_with("traffic_work"),
    everything()
  ) |>
  dplyr::filter(
    ndt_total_20 > 99,
    ndt_total_22 > 99,
    index_i > 0.85,
    index_i < 1.75
  ) |>
  dplyr::mutate(
    index_i_total = sum(traffic_work_total_22) / sum(traffic_work_total_20),
    private_ratio = traffic_work_privat_22 / traffic_work_privat_20
  )

summary(rtm_20_22_purpose$index_p)
sd(rtm_20_22_purpose$index_p)
# TODO: weighted SD

# See the distributions
rtm_20_22_purpose |>
  ggplot() +
  geom_boxplot(aes(index_i))
# Many outliers

rtm_20_22_purpose |>
  ggplot() +
  geom_histogram(aes(index_i))


rtm_20_22_purpose |>
  ggplot() +
  geom_boxplot(aes(traffic_work_privat_20))

rtm_20_22_purpose |>
  ggplot() +
  geom_histogram(aes(traffic_work_privat_20))
# Looks normal with heavy tails

rtm_20_22_purpose |>
  ggplot() +
  geom_boxplot(aes(private_ratio))

rtm_20_22_purpose |>
  ggplot() +
  geom_histogram(aes(private_ratio))





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
    maxLanes,
    highestSpeedLimit,
    functional_class,
    function_class,
    aadt = trafficVolumeValue,
    traffic_work_km = trafficWorkValue,
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
        #municipalityIds %in% c(5001, 5028, 5029, 5059, 5031, 5035) ~ "Trondheim",
        municipalityIds %in% c(4601, 4626, 4627, 4631, 4624) ~ "Bergen",
        municipalityIds %in% c(1127, 1103, 1124, 1108) ~ "Nord-Jæren", # should Stavanger and Sandnes be limited?
        municipalityIds %in% c(301, 3205, 3207, 3201, 3203, 3209, 3240, 3216, 3228, 3226, 3220) ~ "Oslo",
        municipalityIds %in% c(3301, 3312, 3314, 3303) ~ "Buskerudbyen",
        municipalityIds %in% c(4001, 4003, 4010, 4012) ~ "Grenland",
        municipalityIds %in% c(4204, 4223, 4218, 4216, 4215) ~ "Kristiansand"
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
    function_class = factor(function_class),
    traffic_work_Mkm = traffic_work_km * 1e-6,
    log_traffic_work_Mkm = log(traffic_work_Mkm + 1),
    log_log_traffic_work_Mkm = log(log_traffic_work_Mkm + 1),
    sqrt_traffic_work_Mkm = sqrt(traffic_work_Mkm + 0.5)
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
# Will duplicate those with both TRP and toll station
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

all_links_tidy_no_duplicates <-
  all_links_tidy_2 |>
  dplyr::select(
    -p_id
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


## AADT and traffic work
all_links_tidy_no_duplicates |>
  ggplot(aes(aadt)) +
  geom_histogram() +
  facet_wrap(facets = "city")

all_links_tidy_no_duplicates |>
  ggplot(aes(log_traffic_work_Mkm)) +
  geom_histogram() +
  facet_wrap(facets = "city")

all_links_tidy_no_duplicates |>
  ggplot(aes(log_traffic_work_Mkm)) +
  geom_boxplot() +
  facet_wrap(facets = "city")

all_links_tidy_no_duplicates |>
  ggplot(aes(sqrt_traffic_work_Mkm)) +
  geom_histogram() +
  facet_wrap(facets = "city")

all_links_tidy_no_duplicates |>
  ggplot(aes(log_log_traffic_work_Mkm)) +
  geom_histogram() +
  facet_wrap(facets = "city")

# Poisson? Negative binomial?

all_links_tidy_no_duplicates$traffic_work_Mkm |> base::summary()


## Categorical
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
city_id <-
  c(960, 952, 8952, 959, 1952, 955, 957)

city_names <-
  c("Trondheim", "Nord-Jæren", "Bergen", "Oslo", "Buskerudbyen", "Grenland", "Kristiansand")

city_names_and_ids <-
  tibble::tibble(
    city_id,
    city_names
  )

city_trp_info <-
  purrr::map(
    city_id,
    ~ readr::read_rds(
      file = paste0(
        "index_trp_metadata/trp_",
        .x,
        ".rds"
      )
    ) |>
      dplyr::mutate(
        city_id = .x
      )
  ) |>
  dplyr::bind_rows() |>
  dplyr::select(
    city_id,
    p_id = trp_id,
    name
  ) |>
  dplyr::filter(
    # Removing toll station "Nord for Sluppen bru" which has been moved
    !(p_id == "1017875672")
  ) |>
  dplyr::left_join(
    city_names_and_ids,
    by = join_by(city_id)
  )

# TODO: reduce area to true interest area (some municipalities stretch far inland to remote places, e.g. Sandnes)
# TODO: links on municipality roads
# TODO: new method for point (link) index, based on traffic work?

## Yearly index 2023 ----
# TRD
trd_trp_index <-
  readr::read_rds(
    file = paste0("data_indexpoints_tidy/indekspunkt_", 960, ".rds")
  ) |>
  dplyr::select(
    trp_id,
    index = index_2023
  ) |>
  dplyr::filter(
    # Removing toll station "Nord for Sluppen bru" which has been moved
    !(trp_id == "1017875672")
  ) |>
  dplyr::select(
    p_id = trp_id,
    index
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


### Links and point index
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


### Categorical variables
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


### Numeric variables
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


## Monthly indexes 2023 ----
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

## Distance metrics ----
# Comparing sample and population - do they look alike?



# Power analysis ----
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


# Prepare traffic graph for Quarto ----
# Traffic links from Adm

#layers <- sf::st_layers("C:/Users/snohan/Desktop/traffic_links_2023_2024-10-08.geojson")
#names(links)

links <-
  sf::st_read(
    "C:/Users/snohan/Desktop/traffic_links_2023_2024-10-11.geojson",
    as_tibble = TRUE
    #query = "SELECT * FROM \"traffic_links_2023_2024-10-11\" LIMIT 150"
  ) |>
  dplyr::select(
    link_id = id,
    roadSystemReferences,
    startTrafficNodeId,
    endTrafficNodeId,
    municipalityIds,
    associatedTrpIds,
    hasOnlyPublicTransportLanes,
    functionalRoadClass,
    length,
    trafficVolumes
  ) |>
  dplyr::rename(
    from = startTrafficNodeId,
    to = endTrafficNodeId,
    functional_road_class = functionalRoadClass
  ) |>
  dplyr::mutate(
    functional_road_class =
      dplyr::case_when(
        functional_road_class > 5 ~ 5,
        TRUE ~ functional_road_class
      ),
    functional_road_class = as.factor(functional_road_class)
  ) |>
  dplyr::filter(
    hasOnlyPublicTransportLanes == FALSE
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    trp_id = stringr::str_extract_all(associatedTrpIds, "(?<=\")[:alnum:]+(?=\")")
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    -hasOnlyPublicTransportLanes,
    -associatedTrpIds
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
  dplyr::select(
    link_id,
    trafficVolumeValue,
    year,
    coverage,
    trafficWorkValue,
    correctedStandardError,
    trafficVolumeType,
    sourceType,
    registrationFrequency
  ) |>
  dplyr::summarise(
    traffic_volume = mean(trafficVolumeValue) |> round(),
    traffic_work = mean(trafficWorkValue),
    .by = "link_id"
  )


## Graph for selected cities ----
## Nodes ----
nodes <-
  sf::st_read("C:/Users/snohan/Desktop/traffic-nodes-2023_2024-10-11.geojson") |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    #numberOfUndirectedLinks
  ) |>
  tibble::as_tibble()


# NB! Need to remove duplicate links (crossing municipality boundaries)
municipality_ids_nj <- c(1127, 1103, 1124, 1108)

links_nj <-
  links |>
  tidyr::unnest_longer(
    municipalityIds,
    values_to = "municipality_id"
  ) |>
  dplyr::filter(
    municipality_id %in% municipality_ids_nj
  ) |>
  dplyr::select(
    -municipality_id,
    -trafficVolumes
  ) |>
  dplyr::distinct() |>
  # Removing one disconnected link
  dplyr::filter(
    !(link_id == "0.13812205@320420-1.0@320728")
  )

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

links_nj_final |>
  ggplot(aes(color = functional_road_class)) +
  geom_sf()


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

# NB! Node ids are being reset 1:n
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
