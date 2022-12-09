# Prepare data in RDS files for QMD document.
source("H:/Programmering/R/byindeks/rmd_setup.R")
source("trp_toll_comparison_functions.R")
source("get_from_trafficdata_api.R")

# Moholt KD3 ----
moholt_kd3_toll <-
  read_apar_csv(
    "toll_comparison_data/moholt_kd3.csv"
  )

moholt_kd3_trp <-
  get_trp_dt(
    trp_id = "63028V72219",
    from = min(moholt_kd3_toll$date),
    to = max(moholt_kd3_toll$date) |> clock::add_days(1)
  ) |>
  tibble::as_tibble()

moholt <-
  dplyr::bind_rows(
    moholt_kd3_toll,
    moholt_kd3_trp
  ) |>
  dplyr::group_by(
    date,
    source
  ) |>
  dplyr::summarise(
    traffic_volume = sum(traffic_volume),
    .groups = "drop"
  )

moholt_diff <-
  moholt |>
  tidyr::pivot_wider(
    names_from = source,
    names_prefix = "traffic_volume_",
    values_from = traffic_volume
  ) |>
  dplyr::summarise(
    #rmse = sqrt(mean((traffic_volume_TRP - traffic_volume_Toll)^2)),
    mean_diff =
      mean(traffic_volume_AutoPASS - traffic_volume_TRP) |>
      round()
    #median_diff = median(traffic_volume_TRP - traffic_volume_Toll)
  ) |>
  purrr::pluck(1)

moholt_info <-
  list(
    moholt,
    moholt_diff
  ) |>
  readr::write_rds(
    file = "toll_comparison_data/moholt.rds"
  )


# TODO: compare vehicle classes (light and heavy)
# TODO: MC?

# Storlersbakken ----
storlersbakken_toll <-
  read_apar_csv(
    "toll_comparison_data/storlersbakken.csv"
  )

storlersbakken_trp <-
  get_trp_dt(
    trp_id = "66126V3112188",
    from = min(storlersbakken_toll$date),
    to = max(storlersbakken_toll$date) |> clock::add_days(1)
  ) |>
  tibble::as_tibble()

storlersbakken <-
  dplyr::bind_rows(
    storlersbakken_toll,
    storlersbakken_trp
  ) |>
  dplyr::group_by(
    date,
    source
  ) |>
  dplyr::summarise(
    traffic_volume = sum(traffic_volume),
    .groups = "drop"
  )

storlersbakken_diff <-
  storlersbakken |>
  tidyr::pivot_wider(
    names_from = source,
    names_prefix = "traffic_volume_",
    values_from = traffic_volume
  ) |>
  dplyr::summarise(
    mean_diff =
      mean(traffic_volume_AutoPASS - traffic_volume_TRP) |>
      round()
  ) |>
  purrr::pluck(1)

storlersbakken_info <-
  list(
    storlersbakken,
    storlersbakken_diff
  ) |>
  readr::write_rds(
    file = "toll_comparison_data/storlersbakken.rds"
  )


# Rotvoll ----
rotvoll_toll <-
  read_apar_csv(
    "toll_comparison_data/rotvoll.csv"
  )

rotvoll_trp <-
  get_trp_dt(
    trp_id = "78492V2394249",
    from = min(rotvoll_toll$date),
    to = max(rotvoll_toll$date) |> clock::add_days(1)
  ) |>
  tibble::as_tibble()

rotvoll <-
  dplyr::bind_rows(
    rotvoll_toll,
    rotvoll_trp
  ) |>
  dplyr::group_by(
    date,
    source
  ) |>
  dplyr::summarise(
    traffic_volume = sum(traffic_volume),
    .groups = "drop"
  )

rotvoll_diff <-
  rotvoll |>
  tidyr::pivot_wider(
    names_from = source,
    names_prefix = "traffic_volume_",
    values_from = traffic_volume
  ) |>
  dplyr::summarise(
    mean_diff =
      mean(traffic_volume_AutoPASS - traffic_volume_TRP) |>
      round()
  ) |>
  purrr::pluck(1)

rotvoll_info <-
  list(
    rotvoll,
    rotvoll_diff
  ) |>
  readr::write_rds(
    file = "toll_comparison_data/rotvoll.rds"
  )


# Langmoåsen ----
langmoas_toll <-
  read_apar_csv(
    "toll_comparison_data/langmoas.csv"
  )

langmoas_trp <-
  get_trp_dt(
    trp_id = "72936V3118172",
    from = min(langmoas_toll$date),
    to = max(langmoas_toll$date) |> clock::add_days(1)
  ) |>
  tibble::as_tibble()

langmoas <-
  dplyr::bind_rows(
    langmoas_toll,
    langmoas_trp
  ) |>
  dplyr::group_by(
    date,
    source
  ) |>
  dplyr::summarise(
    traffic_volume = sum(traffic_volume),
    .groups = "drop"
  )

langmoas_diff <-
  langmoas |>
  tidyr::pivot_wider(
    names_from = source,
    names_prefix = "traffic_volume_",
    values_from = traffic_volume
  ) |>
  dplyr::summarise(
    mean_diff =
      mean(traffic_volume_AutoPASS - traffic_volume_TRP) |>
      round()
  ) |>
  purrr::pluck(1)

langmoas_info <-
  list(
    langmoas,
    langmoas_diff
  ) |>
  readr::write_rds(
    file = "toll_comparison_data/langmoas.rds"
  )


# Jernbaneveien ----
jernbaneveien_toll <-
  read_apar_csv(
    "toll_comparison_data/jernbaneveien.csv"
  )

jernbaneveien_trp <-
  get_trp_dt(
    trp_id = "86957V885288",
    from = min(jernbaneveien_toll$date),
    to = max(jernbaneveien_toll$date) |> clock::add_days(1)
  ) |>
  tibble::as_tibble()

jernbaneveien <-
  dplyr::bind_rows(
    jernbaneveien_toll,
    jernbaneveien_trp
  ) |>
  dplyr::group_by(
    date,
    source
  ) |>
  dplyr::summarise(
    traffic_volume = sum(traffic_volume),
    .groups = "drop"
  )

jernbaneveien_diff <-
  jernbaneveien |>
  tidyr::pivot_wider(
    names_from = source,
    names_prefix = "traffic_volume_",
    values_from = traffic_volume
  ) |>
  dplyr::summarise(
    mean_diff =
      mean(traffic_volume_AutoPASS - traffic_volume_TRP) |>
      round()
  ) |>
  purrr::pluck(1)

jernbaneveien_info <-
  list(
    jernbaneveien,
    jernbaneveien_diff
  ) |>
  readr::write_rds(
    file = "toll_comparison_data/jernbaneveien.rds"
  )


# Skjæringa ----
skjeringa_toll <-
  read_apar_csv(
    "toll_comparison_data/skjeringa.csv"
  )

skjeringa_trp <-
  get_trp_dt(
    trp_id = "91229V886007",
    from = min(skjeringa_toll$date),
    to = max(skjeringa_toll$date) |> clock::add_days(1)
  ) |>
  tibble::as_tibble()

skjeringa <-
  dplyr::bind_rows(
    skjeringa_toll,
    skjeringa_trp
  ) |>
  dplyr::group_by(
    date,
    source
  ) |>
  dplyr::summarise(
    traffic_volume = sum(traffic_volume),
    .groups = "drop"
  )

skjeringa_diff <-
  skjeringa |>
  tidyr::pivot_wider(
    names_from = source,
    names_prefix = "traffic_volume_",
    values_from = traffic_volume
  ) |>
  dplyr::summarise(
    mean_diff =
      mean(traffic_volume_AutoPASS - traffic_volume_TRP) |>
      round()
  ) |>
  purrr::pluck(1)

skjeringa_info <-
  list(
    skjeringa,
    skjeringa_diff
  ) |>
  readr::write_rds(
    file = "toll_comparison_data/skjeringa.rds"
  )


# Svartdalstunnelen ----
svartdalstunnelen_toll <-
  read_apar_csv(
    "toll_comparison_data/svartdalstunnelen.csv"
  )

svartdalstunnelen_trp_ryen <-
  get_trp_dt(
    trp_id = "89094V3154514",
    from = min(svartdalstunnelen_toll$date),
    to = max(svartdalstunnelen_toll$date) |> clock::add_days(1)
  ) |>
  tibble::as_tibble()

svartdalstunnelen_trp_sentrum <-
  get_trp_dt(
    trp_id = "19702V625216",
    from = min(svartdalstunnelen_toll$date),
    to = max(svartdalstunnelen_toll$date) |> clock::add_days(1)
  ) |>
  tibble::as_tibble()

svartdalstunnelen <-
  dplyr::bind_rows(
    svartdalstunnelen_trp_ryen,
    svartdalstunnelen_trp_sentrum
  ) |>
  dplyr::group_by(
    date,
    vehicle_class,
    source
  ) |>
  dplyr::summarise(
    traffic_volume = sum(traffic_volume),
    .groups = "drop"
  ) |>
  dplyr::bind_rows(
    svartdalstunnelen_toll
  ) |>
  dplyr::group_by(
    date,
    source
  ) |>
  dplyr::summarise(
    traffic_volume = sum(traffic_volume),
    .groups = "drop"
  )

svartdalstunnelen_diff <-
  svartdalstunnelen |>
  tidyr::pivot_wider(
    names_from = source,
    names_prefix = "traffic_volume_",
    values_from = traffic_volume
  ) |>
  dplyr::summarise(
    mean_diff =
      mean(traffic_volume_AutoPASS - traffic_volume_TRP) |>
      round()
  ) |>
  purrr::pluck(1)

svartdalstunnelen_info <-
  list(
    svartdalstunnelen,
    svartdalstunnelen_diff
  ) |>
  readr::write_rds(
    file = "toll_comparison_data/svartdalstunnelen.rds"
  )
