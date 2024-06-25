# Is the selection of city index points representative for the area?
# Is the observed TRP indexes coming from the same distribution as the population of traffic links?
# Using RTM data as population.

# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  library(tidyverse)
  library(readxl)
  library(fitdistrplus)
  library(gamlss)
  library(moments)
  source("get_from_trafficdata_api.R")
}


# Population: RTM links ----
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
      aadt = CD_ADT,
      direction = NVDBRETNING
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


# Sample: TRP links ----
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

# Sample: City TRP index ----
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


# Comparison ----
# So we have a population and a sample. This may be seen as two samples.
# We need to know if this sample can be considered to come from the population.
# Thus we need to compare their distributions. Is there any assumptions in methods for comparing?
# If they do not resemble any known distribution, they may well need a transformation.
# https://book.stat420.org/transformations.html
# https://www.r-bloggers.com/2020/01/a-guide-to-data-transformation/
# http://fmwww.bc.edu/repec/bocode/t/transint.html

# Yeo-Johnson Transformation?

# Population ----
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


# Sample ----
fitdistrplus::descdist(city_trp_index$index)
fitdistrplus::plotdist(city_trp_index$index, demp = TRUE)

fit_norm_s <- fitdistrplus::fitdist(city_trp_index$index, "norm")
plot(fit_norm_s)
summary(fit_norm_s)
# Heavy tails, as expected.


# Test ----
## Mean ----
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

## Variance ----
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