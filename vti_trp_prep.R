{
source("H:/Programmering/R/byindeks/rmd_setup.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
library(writexl)
}

# TRP ----
trp_for_vti <-
  get_points() |>
  dplyr::group_by(trp_id) |>
  dplyr::slice(which.min(validFrom)) |>
  dplyr::ungroup()

trp_latest_data <- get_trps_latest_data()

trp_for_vti_tidy <-
  trp_for_vti %>%
  dplyr::select(
    trp_id,
    name,
    traffic_type,
    registration_frequency,
    county_geono,
    county_no,
    county_name,
    municipality_name,
    road_reference,
    road_link_position,
    lat, lon,
    valid_from = validFrom,
    operational_status
  ) %>%
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    operational_status == "OPERATIONAL"
  ) %>%
  split_road_system_reference() %>%
  dplyr::left_join(
    trp_latest_data,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    latest_day = lubridate::floor_date(latest_data_by_hour)
  ) %>%
  dplyr::filter(
    road_category != "K",
    valid_from < "2023-01-01",
    latest_day > "2024-01-26"
  ) %>%
  dplyr::mutate(
    road_category =
      dplyr::case_when(
        road_category == "E" ~ "R",
        road_category == "R" ~ "R",
        road_category == "F" ~ "F"
      ),
    name = stringr::str_to_title(name, locale = "no")
  ) %>%
  dplyr::select(
    trp_id,
    name,
    county_geono,
    county_no,
    county_name,
    municipality_name,
    road_category,
    road_category_and_number,
    road_reference,
    road_link_position,
    lat, lon,
    latest_day
  ) |>
  dplyr::arrange(
    county_geono,
    road_category
  )

#remove(trp_for_vti)

# Traffic links ----
# Geojson from ADM
#links <- sf::st_read("H:/Programmering/R/aadt_model/traffic-links-2021.geojson")
links <- sf::st_read("C:/Users/snohan/Desktop/traffic_links_2023.geojson")

links_tidy <-
  links |>
  dplyr::select(
    id,
    length,
    trp_id = primaryTrpId,
    isInvalid,
    hasOnlyPublicTransportLanes,
    blocked,
    trafficVolumes
  ) |>
  dplyr::filter(
    !is.na(trp_id),
    isInvalid == FALSE,
    hasOnlyPublicTransportLanes == FALSE,
    blocked == FALSE,
    # Remove periodic
    trp_id %in% trp_for_vti_tidy$trp_id
  )

# Unnest traffic volume column
test <-
  links_tidy |>
  dplyr::slice(1) |>
  jsonlite::flatten()


# Check
# trp_without_link <-
#   trp_for_vti_tidy |>
#   dplyr::anti_join(
#     links_tidy,
#     by = "trp_id"
#   )
# These are on same link as others (among them are the ones on only half the road)

remove(links)


# AADT ----
# coverage and length_quality previous year
trp_aadt_raw <-
  trp_for_vti_tidy$trp_id %>%
  get_aadt_for_trp_list()

readr::write_rds(
  trp_aadt_raw,
  file = "trp_aadt.rds"
)

trp_aadts <-
  readr::read_rds("trp_aadt.rds") |>
  dplyr::filter(year == 2022)


# Exclude trps with low index coverage last year
# CAUTION: Problem could lie in year before last year...
# pointindex <- get_published_pointindex_paginated(962, 2020, 12)
#
# pointindices <- pointindex[[2]] %>%
#   dplyr::filter(day_type == "ALL",
#                 period == "year_to_date") %>%
#   dplyr::select(trp_id, index_total_coverage)

trp_for_vti_aadt_candidates <-
  trp_for_vti_tidy %>%
  dplyr::inner_join(
    trp_aadts,
    by = "trp_id"
  ) %>%
  #dplyr::left_join(pointindices) %>%
  dplyr::mutate(
    valid_length_percent = round(valid_length_volume / adt * 100, digits = 1)#,
    #index_ok = dplyr::case_when(is.na(index_total_coverage) ~ TRUE,
    #                                        index_total_coverage > 90 ~ TRUE,
    #                                        TRUE ~ FALSE)
  ) %>%
  dplyr::filter(
    trp_id %in% links_tidy$trp_id,
    coverage > 95,
    #adt >= 200,
    valid_length_percent > 99#,
    #index_ok == TRUE
  )


# Manual log
# Write a file to manually update if the trp is chosen
# trp_for_vti_aadt %>%
#   dplyr::select(
#     trp_id,
#     name,
#     county_geono,
#     county_name,
#     municipality_name,
#     road_category,
#     road_reference,
#     adt
#   ) %>%
#   dplyr::mutate(chosen = NA) %>%
#   write.csv2(
#     file = "trp_for_vti_2022.csv",
#     row.names = F
#   )

# Read back in the result of chosen trps
# not_chosen <-
#   readxl::read_xlsx("trp_for_vti_2022.xlsx") %>%
#   dplyr::filter(chosen == FALSE)

# Remove unwanted TRPs
#source("vti_trp_not_chosen_2023.R")

trp_for_vti_aadt <-
  trp_for_vti_aadt_candidates %>%
  dplyr::filter(
    !(trp_id %in% not_chosen)
  ) |>
  dplyr::mutate(
    trp_label = paste(name, road_category_and_number, adt, sep = "<br/>"),
    trp_label = lapply(trp_label, htmltools::HTML)
  )


readr::write_rds(
  trp_for_vti_aadt,
  file = "vti_trp_candidates.rds"
)


# TRPs from last years VTI ----
pointindex_last_year <- get_published_pointindex_for_months_paginated(962, 2023, 12)

trps_last_year <- pointindex_last_year[[1]]

trps_last_year_to_keep <-
  trp_for_vti_aadt |>
  dplyr::filter(
    trp_id %in% trps_last_year
  ) |>
  dplyr::mutate(
    change_status = "keep"
  )

trps_last_year_to_discard <-
  tibble::tibble(
    trp_id = trps_last_year
  ) |>
  dplyr::filter(
    !(trp_id %in% trp_for_vti_aadt$trp_id)
  ) |>
  dplyr::left_join(
    trp_for_vti,
    by = "trp_id"
  ) |>
  dplyr::select(
    county_geono,
    county_name,
    trp_id,
    name,
    road_reference
  ) |>
  dplyr::arrange(
    county_geono,
    name
  ) |>
  dplyr::mutate(
    change_status = "discard"
  )

trps_to_add <-
  trp_for_vti_aadt |>
  dplyr::filter(
    !(trp_id %in% trps_last_year)
  ) |>
  dplyr::mutate(
    change_status = "add"
  )

## Write Excel ----
dplyr::bind_rows(
  trps_last_year_to_keep,
  trps_last_year_to_discard,
  trps_to_add
) |>
  dplyr::select(
    trp_id,
    name,
    county_name,
    municipality_name,
    road_category_and_number,
    change_status
  ) |>
  dplyr::arrange(
    change_status,
    name
  ) |>
  writexl::write_xlsx(
    path = "new_vti/revise_vti_trp_2024.xlsx"
  )


# Used traffic links ----
link_candidates <-
  links_tidy |>
  dplyr::filter(
    trp_id %in% trp_for_vti_aadt$trp_id
  ) |>
  dplyr::mutate(
    to_keep = dplyr::case_when(
      trp_id %in% trps_last_year_to_keep$trp_id ~ TRUE,
      TRUE ~ FALSE
    )
  )

readr::write_rds(
  link_candidates,
  file = "vti_link_candidates.rds"
)

# Measured traffic work ----
total_traffic_work <-
  readr::read_rds("traffic_work_2021.rds") |>
  dplyr::mutate(
    road_category =
      dplyr::case_when(
        Vegkategori == "E+R" ~ "R",
        TRUE ~ Vegkategori
      ),
    county_no = base::unlist(Fylkenr)
  ) |>
  dplyr::select(
    county_no,
    road_category,
    total_traffic_work_mill_km = trafikkarbeid
  )

measured_traffic_work <-
  link_candidates |>
  sf::st_drop_geometry() |>
  dplyr::left_join(
    trp_for_vti_aadt_candidates,
    by = "trp_id"
  ) |>
  dplyr::group_by(
    county_geono,
    county_no,
    county_name,
    road_category
  ) |>
  dplyr::summarise(
    measured_traffic_work_mill_km = sum(adt * 365 * length) / 1e9,
    .groups = "drop"
  ) |>
  dplyr::filter(
    !is.na(county_geono)
  ) |>
  dplyr::left_join(
    total_traffic_work,
    by = c("county_no", "road_category")
  ) |>
  dplyr::mutate(
    measured_percentage_of_traffic_work = round(100 * measured_traffic_work_mill_km / total_traffic_work_mill_km, 0)
  ) |>
  dplyr::select(
    county_geono,
    #county_no,
    #county_name,
    road_category,
    measured_percentage_of_traffic_work
  )
