# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  library(tidyverse)
  library(readxl)
  library(fitdistrplus)
  library(gamlss)
  library(moments)
  library(car)
  library(tidygraph)
  library(ggraph)
  library(paletteer)
}


# RTM data ----
# Steps for describing representativeness
# 1. Define the population
# 2. Find relevant characteristics of the population and describe these
# 3. Describe the sampling process
# 4. Compare sample characteristics with the population using measures of fit
# 5. Decide on acceptance criteria for similarity


test <-
  readxl::read_excel("rtm/rtm_trondheim_2022_ny.xlsx") |>
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
  # Some "supersections" seem like duplicates and are disclosed by large deviation between distance and diff in meter values.
  # Also some obviously wrong values in distance, Removing both of these.
  dplyr::mutate(
    diff_meters = abs(to_meter - from_meter) * 1e-3,
    ratio_distance_and_diff_meters = distance / diff_meters,
    F_pi_i_lambda_half = 2 * (sqrt(distance) - sqrt(diff_meters)) |> abs() |> round(1)
  ) |>
  dplyr::filter(
    aadt > 0,
    F_pi_i_lambda_half == 0,
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
    traffic_work = distance * aadt # * 365
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
  )


# RTM links ----
## RTM populationlinks ----
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
    # Some "supersections" seem like duplicates and are disclosed by large deviation between distance and diff in meter values.
    # Also some obviously wrong values in distance, Removing both of these.
    dplyr::mutate(
      diff_meters = abs(to_meter - from_meter) * 1e-3,
      ratio_distance_and_diff_meters = distance / diff_meters,
      F_pi_i_lambda_half = 2 * (sqrt(distance) - sqrt(diff_meters)) |> abs() |> round(1)
    ) |>
    dplyr::filter(
      aadt > 0,
      F_pi_i_lambda_half == 0,
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
      traffic_work = distance * aadt # * 365
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
    ) |>
    dplyr::mutate(
      id = paste0(from_id, "_", to_id)
    ) |>
    dplyr::relocate(id) |>
    dplyr::select(
      -from_id,
      -to_id
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

# Point index
rtm_20_22 <-
  dplyr::inner_join(
    rtm_trd_20,
    rtm_trd_22,
    by = dplyr::join_by(id, road_ref, from_meter, to_meter),
    suffix = c("_20", "_22")
  ) |>
  dplyr::mutate(
    index_i = aadt_22 / aadt_20,
    index_p = 100 * (index_i - 1),
    weight = traffic_work_20 / sum(traffic_work_20),
    city_index = (sum(traffic_work_22) / sum(traffic_work_20) - 1 ) * 100,
    deviation = weight * (index_p - city_index)^2
  ) |>
  dplyr::filter(
    !is.na(road_ref)
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
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    id,
    from_meter, to_meter
  ) |>
  # sort out multiple matches caused by missing intersection info in RTM data
  dplyr::filter(
    !(trp_id == "920506370" & id != "284593_284622"),
    !(trp_id == "20570V72811" & id != "284786_284862"),
    !(trp_id == "94210V2411536" & id != "284790_424280"),
    !(trp_id == "264832263" & id != "284919_344828"),
    !(trp_id == "40649V2411511" & id != "284902_284903"),
    !(trp_id == "44210V2411509" & id != "284791_284898"),
    !(trp_id == "75341V41863" & id != "286342_286350"),
    !(trp_id == "18672V578623" & id != "354556_352564")
  )

# Municipality roads are not included
city_trp_unmatched <-
  city_trp |>
  dplyr::filter(
    !(trp_id %in% rtm_trp$trp_id)
  )
# Also Brattørbrua without match because RTM has this road set as GS (which is wrong)

rtm_20_22_trp <-
  rtm_20_22 |>
  dplyr::mutate(
    city_trp = id %in% rtm_trp$id
  )

readr::write_rds(
  rtm_20_22_trp,
  "representativity/rtm_20_22_trondheim.rds"
)


## RTM population and sample index ----
pop_tw <- sum(rtm_20_22$traffic_work_20)

rtm_pop_index <-
  rtm_20_22_trp |>
  dplyr::summarise(
    area_index_i = sum(aadt_22) / sum(aadt_20),
    area_index_p = (100 * (area_index_i - 1)) |> round(1),
    tw_area_index_i = sum(traffic_work_22) / sum(traffic_work_20),
    tw_area_index_p = (100 * (tw_area_index_i - 1)) |> round(2),
    n_trp = n(),
    sum_tw = sum(traffic_work_20),
    sum_weights = sum(weight),
    sum_squared_weight = sum(weight^2),
    n_eff = 1 / sum_squared_weight,
    weighted_variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
    weighted_sd_p = sqrt(weighted_variance_p),
    unweighted_mean_p = mean(index_p),
    unweighted_sd_p = sd(index_p)
  ) |>
  dplyr::mutate(
    # Not including the finite population correction here effectively says the this is a sample from an infinitely large population.
    standard_error_without_fpc = sqrt(sum_squared_weight * weighted_variance_p),
    standard_error_with_fpc = sqrt(sum_squared_weight * weighted_variance_p) * sqrt((pop_tw - sum_tw) / (pop_tw - 1)),
    ci_lower = round(tw_area_index_p + stats::qt(0.025, n_trp - 1) * standard_error_without_fpc, 1),
    ci_upper = round(tw_area_index_p - stats::qt(0.025, n_trp - 1) * standard_error_without_fpc, 1),
    ci_lower_fpc = round(tw_area_index_p + stats::qt(0.025, n_trp - 1) * standard_error_with_fpc, 1),
    ci_upper_fpc = round(tw_area_index_p - stats::qt(0.025, n_trp - 1) * standard_error_with_fpc, 1)
  )


# For area index by sample, need to recalculate the weights
rtm_20_22_trp_sam <-
  rtm_20_22_trp |>
  dplyr::filter(
    city_trp == TRUE
  ) |>
  dplyr::mutate(
    weight = traffic_work_20 / sum(traffic_work_20),
    city_index = (sum(traffic_work_22) / sum(traffic_work_20) - 1 ) * 100,
    deviation = weight * (index_p - city_index)^2
  )

rtm_sam_index <-
  rtm_20_22_trp_sam |>
  dplyr::summarise(
    area_index_i = sum(aadt_22) / sum(aadt_20),
    area_index_p = (100 * (area_index_i - 1)) |> round(1),
    tw_area_index_i = sum(traffic_work_22) / sum(traffic_work_20),
    tw_area_index_p = (100 * (tw_area_index_i - 1)) |> round(2),
    n_trp = n(),
    sum_tw = sum(traffic_work_20),
    sum_weights = sum(weight),
    sum_squared_weight = sum(weight^2),
    n_eff = 1 / sum_squared_weight,
    weighted_variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
    weighted_sd_p = sqrt(weighted_variance_p),
    unweighted_mean_p = mean(index_p),
    unweighted_sd_p = sd(index_p)
  ) |>
  dplyr::mutate(
    # Not including the finite population correction here effectively says the this is a sample from an infinitely large population.
    standard_error_without_fpc = sqrt(sum_squared_weight * weighted_variance_p),
    standard_error_with_fpc = sqrt(sum_squared_weight * weighted_variance_p) * sqrt((pop_tw - sum_tw) / (pop_tw - 1)),
    ci_lower = round(tw_area_index_p + stats::qt(0.025, n_trp - 1) * standard_error_without_fpc, 1),
    ci_upper = round(tw_area_index_p - stats::qt(0.025, n_trp - 1) * standard_error_without_fpc, 1),
    ci_lower_fpc = round(tw_area_index_p + stats::qt(0.025, n_trp - 1) * standard_error_with_fpc, 1),
    ci_upper_fpc = round(tw_area_index_p - stats::qt(0.025, n_trp - 1) * standard_error_with_fpc, 1)
  )

rtm_index <-
  dplyr::bind_rows(
    rtm_pop_index,
    rtm_sam_index
  )

readr::write_rds(
  rtm_index,
  "representativity/rtm_20_22_trondheim_index.rds"
)

# TODO: HTML-doc insert: compare results when calculating area index with weights as AADT and TW.
# TODO: HTML-doc insert: make a case for the weighted sd and fpc


## RTM simulated samples ----
rtm_trd <- readr::read_rds("representativity/rtm_20_22_trondheim.rds")


calculate_random_sample_index <- function(){

  #set.seed(1845)
  rtm_random_sample_ids <-
    rtm_trd$id |>
    base::sample(42)

  rtm_random_sample <-
    rtm_trd |>
    dplyr::filter(
      id %in% rtm_random_sample_ids
    ) |>
    dplyr::mutate(
      weight = traffic_work_20 / sum(traffic_work_20),
      city_index = (sum(traffic_work_22) / sum(traffic_work_20) - 1 ) * 100,
      deviation = weight * (index_p - city_index)^2
    )

  rtm_random_sample_index <-
    rtm_random_sample |>
    dplyr::summarise(
      #area_index_i = sum(aadt_22) / sum(aadt_20),
      #area_index_p = (100 * (area_index_i - 1)) |> round(1),
      tw_area_index_i = sum(traffic_work_22) / sum(traffic_work_20),
      tw_area_index_p = (100 * (tw_area_index_i - 1)) |> round(2),
      n_trp = n(),
      sum_tw = sum(traffic_work_20),
      sum_weights = sum(weight),
      sum_squared_weight = sum(weight^2),
      n_eff = 1 / sum_squared_weight,
      weighted_variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
      weighted_sd_p = sqrt(weighted_variance_p)#,
      #unweighted_mean_p = mean(index_p),
      #unweighted_sd_p = sd(index_p)
    ) #|>
  #dplyr::mutate(
  # Not including the finite population correction here effectively says the this is a sample from an infinitely large population.
  #standard_error_without_fpc = sqrt(sum_squared_weight * weighted_variance_p),
  #standard_error_with_fpc = sqrt(sum_squared_weight * weighted_variance_p) * sqrt((pop_tw - sum_tw) / (pop_tw - 1)),
  #ci_lower = round(tw_area_index_p + stats::qt(0.025, n_trp - 1) * standard_error_without_fpc, 1),
  #ci_upper = round(tw_area_index_p - stats::qt(0.025, n_trp - 1) * standard_error_without_fpc, 1),
  #ci_lower_fpc = round(tw_area_index_p + stats::qt(0.025, n_trp - 1) * standard_error_with_fpc, 1),
  #ci_upper_fpc = round(tw_area_index_p - stats::qt(0.025, n_trp - 1) * standard_error_with_fpc, 1)
  #)

  return(rtm_random_sample_index)
}


#random_samples <- tibble::tibble()
for (n in 1:1000) {

  result <- calculate_random_sample_index()

  random_samples <-
    dplyr::bind_rows(
      random_samples,
      result
    )

}

readr::write_rds(
  random_samples,
  "representativity/rtm_random_samples.rds"
)

random_samples_stats <-
  random_samples |>
  dplyr::summarise(
    n = n(),
    mean_index_p = mean(tw_area_index_p),
    median_index_p = median(tw_area_index_p),
    min_index_p = min(tw_area_index_p),
    max_index_p = max(tw_area_index_p),
    mean_sd = mean(weighted_sd_p),
    sd = sd(tw_area_index_p),
    sd_test = sqrt((1/999) * sum((tw_area_index_p - mean(tw_area_index_p))^2)),
    ci_lower = mean_index_p - 1.96 * sd,
    ci_upper = mean_index_p + 1.96 * sd
  )


## RTM sample: City TRP index ----
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


# TODO: simulate samples of different sizes, does it stabilize the index result as sample size increases?



# RTM comparison ----
# So we have a population and a sample. This may be seen as two samples.
# We need to know if this sample can be considered to come from the population.
# Thus we need to compare their distributions. Is there any assumptions in methods for comparing?
# If they do not resemble any known distribution, they may well need a transformation.
# https://book.stat420.org/transformations.html
# https://www.r-bloggers.com/2020/01/a-guide-to-data-transformation/
# http://fmwww.bc.edu/repec/bocode/t/transint.html

# Yeo-Johnson Transformation?


## RTM population ----
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


## RTM sample ----
fitdistrplus::descdist(city_trp_index$index)
fitdistrplus::plotdist(city_trp_index$index, demp = TRUE)

fit_norm_s <- fitdistrplus::fitdist(city_trp_index$index, "norm")
plot(fit_norm_s)
summary(fit_norm_s)
# Heavy tails, as expected.


## RTM test ----
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
