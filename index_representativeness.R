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
  source("get_from_trafficdata_api.R")
}


# Data from RTM ----
read_rtm_data <- function(file_path) {

  readxl::read_excel(file_path) |>
    dplyr::select(
      start = A,
      end = B,
      distance = DISTANCE,
      road_category_id = LINKTYPE,
      road_ref = VEGREFERAN,
      VS,
      VEGTYPE,
      aadt = CD_ADT,
      #CD_ADT2,
      direction = NVDBRETNIN
      #TEST_ID,
      #TEST2
    ) |>
    dplyr::filter(
      aadt > 0,
      road_category_id %in% c(1, 2, 3),
      VS == "V",
      VEGTYPE != 19290 # roundabouts
    ) |>
    # Adding directions
    dplyr::rowwise() |>
    dplyr::mutate(
      id = list(stringr::str_sort(c(start, end))) |>
        purrr::map_chr(paste, collapse = "_")
    ) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      aadt = sum(aadt),
      .by = c(id, road_ref)
    )

}

rtm_trd_20 <- read_rtm_data("rtm/rtm_trondheim_2020.xlsx")
rtm_trd_22 <- read_rtm_data("rtm/rtm_trondheim_2022.xlsx")

rtm_20_22 <-
  dplyr::inner_join(
    rtm_trd_20,
    rtm_trd_22,
    by = dplyr::join_by(id, road_ref),
    suffix = c("_20", "_22")
  ) |>
  dplyr::mutate(
    index_i = aadt_22 / aadt_20,
    index_p = 100 * (index_i - 1)
  ) |>
  # Some links have the exact same values, including road_ref
  # Consider these as the same traffic link
  dplyr::distinct(
    dplyr::pick(-id),
    .keep_all = TRUE
  )

rtm_index <-
  rtm_20_22 |>
  dplyr::summarise(
    area_index_i = sum(aadt_22) / sum(aadt_20),
    area_index_p = 100 * (area_index_i - 1),
    mean_p = mean(index_p),
    sd_p = sd(index_p)
  )


# City index ----
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


# Population ----
# 1500 data samples should ensure normality assumptions to be safe
fitdistrplus::descdist(rtm_20_22$index_p)
fitdistrplus::plotdist(rtm_20_22$index_p, demp = TRUE)

fit_norm <- fitdistrplus::fitdist(rtm_20_22$index_p, "norm")
plot(fit_norm)
summary(fit_norm)
# Looks OK

# Trying
#fit_norm_2 <- gamlss::fitDist(rtm_20_22$index_p, trace = FALSE, try.gamlss = TRUE)
#summary(fit_norm_2)
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


# Look at RTM at position of TRP
# Make a Quarto report on this. Important to mention assumptions and prerquisites as well as weaknesses.
# Liste til May-Berit med byindekspunktene med UTM33-koordinater.
# Lag en felles presentasjon med May-Berit og Jonas. Presenter i ett av arbeidsgruppemøtene for Trafikkdata by.
# Lenketype 30 er sonetilknytning og cd_tot angir antall turer.
# Finn andel av alle turer som fanges opp av minst ett punkt. Hva da med overrepresentasjon?