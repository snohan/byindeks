# Setup ----
{
  source("rmd_setup.R")
  source("get_from_nvdb_api.R")
  library(stringi)
}


# Get AADT-links ----
# 3  Oslo
# 30 Viken
# 34 Innlandet
# 38 Vestfold og Telemark
# 42 Agder
# 11 Rogaland
# 46 Vestland
# 15 Møre og Romsdal
# 50 Trøndelag
# 18 Nordland
# 54 Troms og Finnmark

last_day_of_year <- "2022-12-31"

t_03 <- get_aadt_by_area(3, "true", last_day_of_year)
t_30 <- get_aadt_by_area(30, "true", last_day_of_year)
t_34 <- get_aadt_by_area(34, "true", last_day_of_year)
t_38 <- get_aadt_by_area(38, "true", last_day_of_year)
t_42 <- get_aadt_by_area(42, "true", last_day_of_year)
t_11 <- get_aadt_by_area(11, "true", last_day_of_year)
t_46 <- get_aadt_by_area(46, "true", last_day_of_year)
t_15 <- get_aadt_by_area(15, "true", last_day_of_year)
t_50 <- get_aadt_by_area(50, "true", last_day_of_year)
t_18 <- get_aadt_by_area(18, "true", last_day_of_year)
t_54 <- get_aadt_by_area(54, "true", last_day_of_year)

aadt_link_raw <-
  dplyr::bind_rows(
    t_03,
    t_30,
    t_34,
    t_38,
    t_42,
    t_11,
    t_46,
    t_15,
    t_50,
    t_18,
    t_54
  )

readr::write_rds(
  aadt_link_raw,
  file = "aadt_link_raw_2021.rds"
)

aadt_link_raw <-
  readr::read_rds(
    file = "aadt_link_raw_2021.rds"
  )


# Calculate traffic work ----
traffic_work <-
  aadt_link_raw |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    road_category =
      dplyr::case_when(
        road_category == "E" ~ "E+R",
        road_category == "R" ~ "E+R",
        TRUE ~ road_category
      )
  ) |>
  dplyr::group_by(
    county_numbers,
    road_category
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(aadt_total * 365 * length) / 1e9,
    .groups = "drop"
  ) |>
  dplyr::select(
    Fylkenr = county_numbers,
    Vegkategori = road_category,
    trafikkarbeid = traffic_work_mill_km
  )

readr::write_rds(
  traffic_work,
  file = "traffic_work_2021.rds"
)


# For weighting in VTI ----
jsonlite::write_json(
  traffic_work,
  path = "trafikkarbeid_2021.json",
  prettify = TRUE
)


# Traffic work per use class ----

## AADT on links ----
# Will use road reference as a very simplified geometry, as full geometry is too heavy computationally.

last_day_of_year <- "2022-12-31"

trf_03 <- get_aadt_by_area_with_all_road_references(3, "true", last_day_of_year)
trf_30 <- get_aadt_by_area_with_all_road_references(30, "true", last_day_of_year)
trf_34 <- get_aadt_by_area_with_all_road_references(34, "true", last_day_of_year)
trf_38 <- get_aadt_by_area_with_all_road_references(38, "true", last_day_of_year)
trf_42 <- get_aadt_by_area_with_all_road_references(42, "true", last_day_of_year)
trf_11 <- get_aadt_by_area_with_all_road_references(11, "true", last_day_of_year)
trf_46 <- get_aadt_by_area_with_all_road_references(46, "true", last_day_of_year)
trf_15 <- get_aadt_by_area_with_all_road_references(15, "true", last_day_of_year)
trf_50 <- get_aadt_by_area_with_all_road_references(50, "true", last_day_of_year)
trf_18 <- get_aadt_by_area_with_all_road_references(18, "true", last_day_of_year)
trf_54 <- get_aadt_by_area_with_all_road_references(54, "true", last_day_of_year)

aadt_link_raw_rf <-
  dplyr::bind_rows(
    trf_03,
    trf_30,
    trf_34,
    trf_38,
    trf_42,
    trf_11,
    trf_46,
    trf_15,
    trf_50,
    trf_18,
    trf_54
  )

# Test segmentation at county border
# test <-
#   aadt_link_raw_rf |>
#   dplyr::filter(nvdb_objekt_id == 1014963344)
#
# test_2 <-
#   aadt_link_raw |>
#   dplyr::filter(nvdb_objekt_id == 1014963344)

# Conclusion: after checking overlap with other objects, keep unique combinations of id and county_number!

readr::write_rds(
  aadt_link_raw_rf,
  file = "aadt_link_raw_2021_with_all_road_references.rds"
)

aadt_link_raw_rf_tidy <-
  readr::read_rds(
    file = "aadt_link_raw_2021_with_all_road_references.rds"
  ) |>
  dplyr::select(
    nvdb_objekt_id,
    aadt_total,
    heavy_percentage,
    road_reference_section = shortform,
    length,
    county_id = county_numbers
  ) |>
  split_road_system_reference_section() |>
  dplyr::filter(
    !is.na(road_system)
  ) |>
  dplyr::arrange(
    road_system,
    meter_start,
    meter_end
  ) |>
  dplyr::mutate(
    nvdb_objekt_id = as.character(nvdb_objekt_id)
  )


## Use classes ----
# Read CSV fetched from vegkart.no
# Roadnet selection: not walking and cycling, not roundabouts, just ERF
# qury not based on date to ensure that all objects are included.
read_use_class_file <- function(file_name) {

  readr::read_csv2(
    file_name,
    col_select =
      c(
        `VEGOBJEKT-ID`,
        LOK.VEGSYSTEMREFERANSE
      )
  ) |>
  dplyr::rename(
    id = `VEGOBJEKT-ID`,
    road_reference_section = LOK.VEGSYSTEMREFERANSE
  ) |>
  dplyr::filter(
    !is.na(road_reference_section)
  )
}


# Reduce overlapping objects
reduce_road_system_sections <- function(df) {

  df_tidy <-
    df |>
    dplyr::arrange(
      road_system,
      meter_start
    ) |>
    dplyr::slice_max(
      order_by = meter_end,
      by = c(road_system, meter_start)
    ) |>
    dplyr::slice_min(
      order_by = meter_start,
      by = c(road_system, meter_end)
    ) |>
    dplyr::mutate(
      end_less = TRUE,
      contained_within = FALSE
    )

  any_end_less = base::any(df_tidy$end_less)

  while (any_end_less) {

    df_tidy <-
      df_tidy |>
      dplyr::filter(
        contained_within == FALSE
      ) |>
      dplyr::mutate(
        same_road_system_lag = road_system == dplyr::lag(road_system, default = "TRUE"),
        same_road_system_lead = road_system == dplyr::lead(road_system, default = "TRUE"),
        overlap_backwards = meter_start <= dplyr::lag(meter_end) & same_road_system_lag,
        end_less = meter_end <= dplyr::lag(meter_end) & same_road_system_lag
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        contained_within = base::all(overlap_backwards, end_less)
      ) |>
      dplyr::ungroup()

    any_end_less = base::any(df_tidy$end_less)

  }

  df_reduced <-
    df_tidy |>
    dplyr::select(
      road_system,
      meter_start,
      meter_end
    ) |>
    tidyr::pivot_longer(
      cols = c(meter_start, meter_end),
      names_to = "extreme",
      values_to = "meter"
    ) |>
    dplyr::mutate(
      same_road_system_lag = road_system == dplyr::lag(road_system, default = "TRUE"),
      same_road_system_lead = road_system == dplyr::lead(road_system, default = "TRUE"),
      overlap_backwards = meter <= dplyr::lag(meter) & same_road_system_lag,
      overlap_forwards = meter >= dplyr::lead(meter) & same_road_system_lead
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      connecting = base::any(overlap_backwards, overlap_forwards)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      connecting == FALSE
    ) |>
    dplyr::select(
      road_system,
      extreme,
      meter
    ) |>
    tidyr::pivot_wider(
      names_from = "extreme",
      values_from = "meter",
      values_fn = list
    ) |>
    tidyr::unnest(cols = everything())

  return(df_reduced)
}

#bk_file <- bk10_50_files[1]
read_tidy_and_reduce_bk <- function(bk_file) {

  read_use_class_file(bk_file) |>
  dplyr::select(
    road_reference_section
  ) |>
  dplyr::distinct() |>
  split_road_system_reference_section() |>
  dplyr::filter(
    !is.na(road_system)
  ) |>
  dplyr::select(
    -road_reference_section
  ) |>
  reduce_road_system_sections()
}


### Bk10_50 ----
bk10_50_files <-
  base::list.files(
    "spesialuttak",
    pattern = "^bk10_50.+",
    full.names = TRUE
  )

# Read and reduce each flavour of bk
bk10_50_reduced_by_each_type <-
  purrr::map(bk10_50_files, ~ read_tidy_and_reduce_bk(.x)) |>
  purrr::list_rbind()

bk10_50_reduced <-
  bk10_50_reduced_by_each_type |>
  dplyr::distinct() |>
  dplyr::arrange(
    stringr::str_rank(road_system, numeric = TRUE),
    meter_start
  ) |>
  # Reduce between flavours of bk
  reduce_road_system_sections() |>
  dplyr::mutate(
    bk10_50 = TRUE
  ) |>
  dplyr::rename(
    meter_start_bk = meter_start,
    meter_end_bk = meter_end
  )

aadt_link_bk10_50 <-
  aadt_link_raw_rf_tidy |>
  dplyr::left_join(
    bk10_50_reduced,
    by = dplyr::join_by(
      road_system,
      overlaps(meter_start, meter_end, meter_start_bk, meter_end_bk, bounds = "()")
    )
  ) |>
  dplyr::mutate(
    bk10_50 = dplyr::case_when(
      is.na(bk10_50) ~ FALSE,
      TRUE ~ TRUE
    ),
    metering_length = meter_end - meter_start,
    lack_of_overlap_start =
      dplyr::if_else(
        meter_start <= meter_start_bk,
        meter_start_bk - meter_start,
        0),
    lack_of_overlap_end =
      dplyr::if_else(
        meter_end >= meter_end_bk,
        meter_end - meter_end_bk,
        0)
  ) |>
  dplyr::filter(
    metering_length > 0
  ) |>
  dplyr::mutate(
    ratio_overlap = 1 - (lack_of_overlap_start + lack_of_overlap_end) / metering_length,
    length_overlap = metering_length * ratio_overlap
  ) |>
  dplyr::arrange(
    road_system,
    meter_start
  )

# is metering_length same as geometric length?
# test_length <-
#   aadt_link_bk10_50 |>
#   dplyr::select(
#     nvdb_objekt_id,
#     length
#   ) |>
#   dplyr::distinct()
#
# sum(test_length$length)
# sum(aadt_link_bk10_50$metering_length, na.rm = TRUE)
# Not quite the same, but this is dealt with by ratio overlap.

# test_length_per_object <-
#   aadt_link_bk10_50 |>
#   dplyr::select(
#     nvdb_objekt_id,
#     length,
#     metering_length
#   ) |>
#   dplyr::summarise(
#     sum_metering = sum(metering_length),
#     .by = c(nvdb_objekt_id, length)
#   ) |>
#   dplyr::mutate(
#     diff = length - sum_metering
#   ) |>
#   dplyr::arrange(
#     diff
#   )

#### Test ----
# En trafikkmengdelenke ved Jonsvatnet som overlapper kun i krysset med en bk10_50-lenke
# 1015060830

# En trafikkmengdelenke ved Jonsvatnet som ikke overlapper med en bk10_50-lenke
# 1015060829

# En trafikkmengdelenke ved Jonsvatnet som helt overlapper med en bk10_50-lenke
# 1015060781

aadt_link_test <-
  aadt_link_bk10_50 |>
  dplyr::filter(
    nvdb_objekt_id %in% c("1015060830", "1015060829", "1015060781")
  )

# PASS! :)


### Bk10_60 ----
bk10_60_files <-
  base::list.files(
    "spesialuttak",
    pattern = "^bk10_60.+",
    full.names = TRUE
  )

bk10_60_reduced_by_each_type <-
  purrr::map(bk10_60_files, ~ read_tidy_and_reduce_bk(.x)) |>
  purrr::list_rbind()

bk10_60_reduced <-
  bk10_60_reduced_by_each_type |>
  dplyr::distinct() |>
  dplyr::arrange(
    stringr::str_rank(road_system, numeric = TRUE),
    meter_start
  ) |>
  # Reduce between flavours of bk
  reduce_road_system_sections() |>
  dplyr::mutate(
    bk10_60 = TRUE
  ) |>
  dplyr::rename(
    meter_start_bk = meter_start,
    meter_end_bk = meter_end
  )

aadt_link_bk10_60 <-
  aadt_link_raw_rf_tidy |>
  dplyr::left_join(
    bk10_60_reduced,
    by = dplyr::join_by(
      road_system,
      overlaps(meter_start, meter_end, meter_start_bk, meter_end_bk, bounds = "()")
    )
  ) |>
  dplyr::mutate(
    bk10_60 = dplyr::case_when(
      is.na(bk10_60) ~ FALSE,
      TRUE ~ TRUE
    ),
    metering_length = meter_end - meter_start,
    lack_of_overlap_start =
      dplyr::if_else(
        meter_start <= meter_start_bk,
        meter_start_bk - meter_start,
        0),
    lack_of_overlap_end =
      dplyr::if_else(
        meter_end >= meter_end_bk,
        meter_end - meter_end_bk,
        0)
  ) |>
  dplyr::filter(
    metering_length > 0
  ) |>
  dplyr::mutate(
    ratio_overlap = 1 - (lack_of_overlap_start + lack_of_overlap_end) / metering_length,
    length_overlap = metering_length * ratio_overlap
  ) |>
  dplyr::arrange(
    road_system,
    meter_start
  )


## Traffic work ----
### Heavy total ----
traffic_work_heavy <-
  aadt_link_raw_rf_tidy |>
  dplyr::select(
    nvdb_objekt_id,
    aadt_total,
    heavy_percentage,
    length
  ) |>
  # Removing duplicates, but links that cross county borders is split in two
  dplyr::distinct() |>
  dplyr::mutate(
    traffic_work = 0.01 * heavy_percentage * aadt_total * 365 * length
  ) |>
  dplyr::summarise(
    traffic_work_heavy_mill_km = sum(traffic_work) / 1e9
  )

#length(unique(aadt_link_raw_rf_tidy$nvdb_objekt_id))
#n <- table(traffic_work_heavy$nvdb_objekt_id) |> as.data.frame()


### Bk10_50 ----
traffic_work_bk10_50 <-
  aadt_link_bk10_50 |>
  dplyr::select(
    nvdb_objekt_id,
    county_id,
    length_overlap,
    aadt_total,
    heavy_percentage,
    bk10_50
  ) |>
  dplyr::filter(
    bk10_50 == TRUE
  ) |>
  dplyr::mutate(
    traffic_work = 0.01 * heavy_percentage * aadt_total * 365 * length_overlap
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(traffic_work) / 1e9
  )

ratio_bk10_50 <-
  traffic_work_bk10_50$traffic_work_mill_km / traffic_work_heavy$traffic_work_heavy_mill_km


### Bk10_60 ----
traffic_work_bk10_60 <-
  aadt_link_bk10_60 |>
  dplyr::select(
    nvdb_objekt_id,
    county_id,
    length_overlap,
    aadt_total,
    heavy_percentage,
    bk10_60
  ) |>
  dplyr::filter(
    bk10_60 == TRUE
  ) |>
  dplyr::mutate(
    traffic_work = 0.01 * heavy_percentage * aadt_total * 365 * length_overlap
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(traffic_work) / 1e9
  )

ratio_bk10_60 <-
  traffic_work_bk10_60$traffic_work_mill_km / traffic_work_heavy$traffic_work_heavy_mill_km
