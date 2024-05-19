# Traffic Data API calls to get points metadata and aadt
{
source("rmd_setup.R")
source("get_from_trafficdata_api.R")
source("split_road_system_reference.R")
}


# Municipalities ----
municipalities <- get_municipalities()


# TRPs ----
points <-
  get_points() |>
  dplyr::distinct(trp_id, .keep_all = T)

trp_time_span <- get_trp_data_time_span()


# Nord-Jæren ----
## TRPs used today ----
index_year <- 2023
index_month <- 12
city_number <- 952

municipality_names <- c("Randaberg", "Sola", "Stavanger", "Sandnes")

municipality_numbers <-
  municipalities |>
  dplyr::filter(
    municipality_name %in% municipality_names
  )

city_index <- get_published_index_for_months(city_number, index_year, index_month)
pointindex <- get_published_pointindex_for_months(city_number, index_year, index_month)

city_trps <- pointindex[[1]]
city_name <- city_index$area_name[1]


## TRPs in area ----
trps_unsuited <- c(
  # TRP med samme trafikk
  "03108V320583", # Tastatorget, Finnestad i stedet
  "81631V1727485", # Asheimveien bru, Folkvord bru i stedet
  "07051V2725969", # Eiganestunnelen hovedløp fra Sandnes
  "92879V2726065", # Eiganestunnelen hovedløp fra Stavanger
  "12478V320582" # Jåtten E39
)

trps_filtered <-
  points |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    operational_status != "RETIRED"
  ) |>
  dplyr::mutate(
    operational_status = stringr::str_to_lower(operational_status, locale = "no")
  ) |>
  dplyr::filter(
    municipality_name %in% c("Randaberg", "Sola", "Stavanger", "Sandnes"),
    # øyene nord for Randabergs fastland skal ikke med
    lat < 59.05,
    # områder langt øst skal ikke være med
    lon < 5.82,
    # TODO: samme begrensning må gjøres for populasjonen av trafikklenker
    !(trp_id %in% trps_unsuited)
  ) |>
  dplyr::mutate(included = trp_id %in% city_trps) |>
  dplyr::left_join(
    trp_time_span,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(!is.na(first_data_with_quality_metrics)) |>
  split_road_system_reference() |>
  dplyr::select(
    trp_id,
    name,
    operational_status,
    road_reference,
    road_category_and_number,
    municipality_name,
    lat, lon,
    included,
    first_data, first_data_with_quality_metrics,
    latest_daily_traffic
  )


## AADT ----
trp_aadts <-
  trps_filtered$trp_id |>
  get_aadt_for_trp_list()

trps_aadt_in_period <-
  trp_aadts |>
  dplyr::filter(
    year %in% c(2017:2023)
  ) |>
  dplyr::mutate(
    valid_length_percent = round(valid_length_volume / adt * 100, digits = 1),
    # good enough coverage for index (50), sliding (84)
    good_enough = dplyr::case_when(
      (coverage > 50 & valid_length_percent > 98.5) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::filter(
    good_enough == TRUE
  )

readr::write_rds(
  trps_aadt_in_period,
  "nj_trp_aadt.rds"
)

n_trp_good_enough <-
  trps_aadt_in_period |>
  dplyr::summarise(
    n_trp = n(),
    .by = year
  ) |>
  dplyr::arrange(
    year
  )

readr::write_rds(
  n_trp_good_enough,
  "nj_n_trp.rds"
)

## 2023 ----
trp_aadt_2023 <-
  trp_aadts |>
  dplyr::filter(
    year == 2023
  ) |>
  dplyr::mutate(
    valid_length_percent = round(valid_length_volume / adt * 100, digits = 1)
  ) |>
  dplyr::mutate(
    good_enough = dplyr::case_when(
      (coverage > 50 & valid_length_percent > 99) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::select(
    trp_id,
    adt,
    good_enough,
    coverage
  )


## TRP tidy ----
trps_unsuited_2023 <- c(
  # Stengte veger 2023
  "83652V319725" # Strandgata nord
)

trp_aadt_tidy <-
  trps_filtered |>
  dplyr::left_join(
    trp_aadt_2023,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(
    operational_status == "operational",
    good_enough == TRUE,
    !(trp_id %in% trps_unsuited_2023)
  ) |>
  dplyr::mutate(
    status = dplyr::case_when(
      included == TRUE ~ "Opprinnelig",
      included == FALSE ~ "Mulig"
    ),
    trp_label = paste(name, road_category_and_number, adt, sep = "<br/>"),
    trp_label = lapply(trp_label, htmltools::HTML)
  )


## Traffic links 2023 ----
# Geojson from ADM
links <-
  sf::st_read("C:/Users/snohan/Desktop/traffic_links_2023.geojson") |>
  dplyr::rename(
    municipality_ids = municipalityIds,
    trp_id = primaryTrpId,
  ) |>
  dplyr::select(
    id,
    municipality_ids,
    trp_id,
    primaryTrpIds,
    associatedTrpIds,
    length,
    trafficVolumes
  ) |>
  tidyr::unnest(
    cols = municipality_ids
  ) |>
  dplyr::filter(
    municipality_ids %in% municipality_numbers$municipality_number
  ) |>
  dplyr::select(
    -municipality_ids
  ) |>
  # Remove duplicates (links crossing municipality borders inside city)
  dplyr::distinct() |>
  # manually update trp_id on links missing correct info
  dplyr::mutate(
    trp_id = dplyr::case_when(
      id == "1018316790" ~ "61929V2422166",
      id == "1018204697" ~ "96553V2725986",
      id == "1018200234" ~ "65096V2726170",
      id == "1018200253" ~ "94180V2726102",
      TRUE ~ trp_id
    )
  )


### Traffic work ----
traffic_work <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    length,
    trp_id,
    trafficVolumes
  ) |>
  # Some may be missing, often ferry links as per feb 24
  dplyr::filter(
    !(is.na(trafficVolumes))
  ) |>
  dplyr::mutate(
    volumes = purrr::map(trafficVolumes, ~ jsonlite::fromJSON(.x))
  ) |>
  tidyr::unnest(
    volumes
  ) |>
  dplyr::filter(
    trafficVolumeType == "GUESSTIMATED"
  ) |>
  dplyr::select(
    id,
    length,
    trp_id,
    traffic_work = trafficWorkValue
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(traffic_work) / 1e9,
    n_links = n()
  )

traffic_work_measured <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    length,
    trp_id,
    trafficVolumes
  ) |>
  # Some may be missing, often ferry links as per feb 24
  dplyr::filter(
    !(is.na(trafficVolumes)),
    trp_id %in% trp_aadt_tidy$trp_id
  ) |>
  dplyr::mutate(
    volumes = purrr::map(trafficVolumes, ~ jsonlite::fromJSON(.x))
  ) |>
  tidyr::unnest(
    volumes
  ) |>
  dplyr::filter(
    trafficVolumeType == "GUESSTIMATED"
  ) |>
  dplyr::select(
    id,
    length,
    trp_id,
    traffic_work = trafficWorkValue
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(traffic_work) / 1e9,
    n_links = n()
  )

traffic_work_measured$traffic_work_mill_km / traffic_work$traffic_work_mill_km


### Chosen links ----
links_with_chosen_trp <-
  links |>
  dplyr::inner_join(
    trp_aadt_tidy,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    id,
    trp_id,
    name,
    length,
    road_reference,
    road_category_and_number,
    municipality_name,
    lat, lon,
    included,
    adt,
    status,
    trp_label
  )

# missing_trps <-
#   trp_aadt_tidy |>
#   dplyr::filter(
#     !(trp_id %in% links_with_chosen_trp$trp_id)
#   )


readr::write_rds(
  links_with_chosen_trp,
  "chosen_links_nj_2023.rds"
)


## Index results ----
# Columns:
# Alternative number
# Period
# Subperiod (T/F)
# Index
# N TRP
# CILL
# CIUL

index_yearly_24 <-
  readr::read_rds(
    paste0("data_indexpoints_tidy/byindeks_", city_number, ".rds")
  ) |>
  dplyr::filter(
    !(year < 2023 & index_type == "chained")
  ) |>
  dplyr::select(
    index_period = year_from_to,
    index_p,
    n_trp,
    index_type,
    ci_lower,
    ci_upper
  ) |>
  dplyr::mutate(
    ci_width = ci_upper - ci_lower,
    alternative = "A5",
    complete_period =
      dplyr::case_when(
        index_type == "direct" ~ FALSE,
        TRUE ~ TRUE
      )
  )

rolling_indices_2017 <-
  readr::read_rds(
    paste0("data_indexpoints_tidy/rolling_indices_", city_number, ".rds")
  ) |>
  dplyr::bind_rows() |>
  dplyr::filter(
    month_n == 12,
    month_object == "2023-12-01"
  ) |>
  dplyr::mutate(
    ci_width = ci_upper - ci_lower,
    alternative = paste0("A1_", window),
    complete_period =
      dplyr::case_when(
        month_object == "2023-12-01" ~ TRUE,
        TRUE ~ FALSE
      )
  ) |>
  dplyr::select(
    index_period,
    index_p,
    n_trp,
    index_type = window,
    ci_lower,
    ci_upper,
    ci_width,
    alternative,
    complete_period
  )

index_direct_2017_2023 <-
  readr::read_rds("trp_index/city_index_njaeren_2017_2023.rds") |>
  dplyr::select(
    index_period = period,
    index_p,
    n_trp,
    ci_lower,
    ci_upper
  ) |>
  dplyr::mutate(
    index_type = "direct",
    ci_width = ci_upper - ci_lower,
    alternative = "A3",
    complete_period = TRUE
  )

index_direct_2017_2019_2023 <-
  readr::read_rds("trp_index/city_index_njaeren_2017_2019_2023_direct.rds") |>
  dplyr::select(
    index_period = year_from_to,
    index_p,
    n_trp,
    ci_lower,
    ci_upper,
    index_type
  ) |>
  dplyr::mutate(
    ci_width = ci_upper - ci_lower,
    alternative = "A4",
    complete_period =
      dplyr::case_when(
        index_type == "direct" ~ FALSE,
        TRUE ~ TRUE
      )
  )

index_direct_sliding_2017_2019_2023 <-
  readr::read_rds("trp_index/index_jaeren_2017_2019_2023_direct_sliding_all.rds") |>
  dplyr::mutate(
    ci_width = ci_upper - ci_lower,
    year_base = stringr::str_sub(index_period, 1, 4),
    complete_period =
      dplyr::case_when(
        year < 2023 | year_base > 2017 ~ FALSE,
        TRUE ~ TRUE
      )
  )


n_trp_prospective <-
  tibble::tibble(
    index_period = c(
      "2023-",
      "2017-2023",
      "2023-"
    ),
    n_trp = c(
      79,
      24,
      79
    ),
    index_type = c(
      "36_month",
      "direct",
      "36_month"
    ),
    alternative = c(
      "A4",
      "A3",
      "A3"
    ),
    complete_period = c(
      FALSE,
      FALSE,
      FALSE
    )
  )

# Gather
n_trp_per_chain_link <-
  dplyr::bind_rows(
    index_yearly_24,
    rolling_indices_2017,
    index_direct_2017_2023,
    index_direct_2017_2019_2023,
    index_direct_sliding_2017_2019_2023
  ) |>
  dplyr::bind_rows(
    n_trp_prospective
  ) |>
  # Need to keep only those who have more than one link in the index chain
  dplyr::filter(
    # TODO: get A2 correct here
    complete_period == FALSE
  ) |>
  dplyr::summarise(
    n_trp_string = base::toString(n_trp),
    .by = c(alternative)
  )


dplyr::bind_rows(
  index_yearly_24,
  rolling_indices_2017,
  index_direct_2017_2023,
  index_direct_2017_2019_2023,
  index_direct_sliding_2017_2019_2023
  ) |>
  dplyr::bind_rows(
    n_trp_prospective
  ) |>
  dplyr::left_join(
    n_trp_per_chain_link,
    by = join_by(alternative)
  ) |>
  dplyr::mutate(
    n_trp_string =
      dplyr::case_when(
        is.na(n_trp_string) ~ as.character(n_trp),
        TRUE ~ n_trp_string
      )
  ) |>
readr::write_rds(
  "index_alternatives_nj_2023.rds"
)


# Kristiansand 2023 ----
# Eligible TRPs when 2023 is reference year

municipality_names <-
  c(
    "Kristiansand",
    "Vennesla",
    "Iveland",
    "Birkenes",
    "Lillesand"
  )

municipality_numbers <-
  municipalities |>
  dplyr::filter(
    municipality_name %in% municipality_names
  )


# TRPs used in the 2016-index chain
trps_2016 <- get_published_pointindex_for_months(957, 2023, 1)[[1]]


# Possible TRPs
trps_filtered <-
  points |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    operational_status != "RETIRED"
  ) |>
  dplyr::mutate(
    operational_status = stringr::str_to_lower(operational_status, locale = "no")
  ) |>
  dplyr::filter(
    municipality_name %in%
      c(
        "Kristiansand",
        "Vennesla",
        "Iveland",
        "Birkenes",
        "Lillesand"
      )
  ) |>
  dplyr::mutate(included = trp_id %in% trps_2016) |>
  split_road_system_reference() |>
  dplyr::select(
    trp_id,
    name,
    operational_status,
    road_reference,
    road_category_and_number,
    municipality_name,
    lat, lon,
    included
  )


# AADT
trp_aadts <-
  trps_filtered$trp_id |>
  get_aadt_for_trp_list()

trps_aadt_2023 <-
  trp_aadts |>
  dplyr::filter(
    year %in% c(2023)
  ) |>
  dplyr::mutate(
    valid_length_percent = round(valid_length_volume / adt * 100, digits = 1),
    # good enough coverage for index (50), sliding (84)
    good_enough = dplyr::case_when(
      (coverage > 50 & valid_length_percent > 98.5) ~ TRUE,
      TRUE ~ FALSE
    ),
    reason = dplyr::case_when(
      coverage <= 50 ~ "Lav dekningsgrad",
      valid_length_percent <= 98.5 ~ "Dårlige lengdemålinger",
      TRUE ~ ""
    ),
    adt = round(adt, -1)
  ) |>
  dplyr::select(
    trp_id,
    adt,
    good_enough,
    reason
  )


trps_2023 <-
  trps_filtered |>
  dplyr::left_join(
    trps_aadt_2023,
    by = dplyr::join_by(trp_id)
    ) |>
  dplyr::mutate(
    status = dplyr::case_when(
      included == TRUE ~ "Indeks 2016",
      included == FALSE ~ "Mulig nytt"
    ),
    reason = dplyr::case_when(
      trp_id %in%
        c(
          "58700V3366122", # Lohnelier
          "09525V2399484", # Brennåsen ø
          "26577V2399484", # Brennåsen v
          "98936V121303", # Vesterbrua
          "76952V121302", # Haumyrheia
          "86748V121742", # Haumyrheia
          "00000V1702751", # Timenes
          "54653V1751052", # Løehei
          "97410V1751020", # Songefjell
          "07242V1751122", # Kvernås
          "78032V121764", # Tveit kirke
          "66163V21769", # Kvennhusbekken
          "07306V22174" # Birkeland syd
          )
      ~ "Overrepresentasjon",
      TRUE ~ reason
    ),
    final_status =
      dplyr::case_when(
      reason != "" ~ reason,
      TRUE ~ status
      ) |>
      forcats::as_factor(),
    trp_label = paste(name, road_category_and_number, adt, sep = "<br/>"),
    trp_label = lapply(trp_label, htmltools::HTML)
  )


## Traffic links 2023 ----
# Geojson from ADM
links <-
  sf::st_read("C:/Users/snohan/Desktop/traffic_links_2023_2024-04-09.geojson") |>
  dplyr::rename(
    municipality_ids = municipalityIds,
    trp_id = primaryTrpId,
  ) |>
  dplyr::select(
    id,
    municipality_ids,
    trp_id,
    primaryTrpIds,
    associatedTrpIds,
    length,
    trafficVolumes
  ) |>
  tidyr::unnest(
    cols = municipality_ids
  ) |>
  dplyr::filter(
    municipality_ids %in% municipality_numbers$municipality_number
  ) |>
  dplyr::select(
    -municipality_ids
  ) |>
  # Remove duplicates (links crossing municipality borders inside city)
  dplyr::distinct() |>
  # manually update trp_id on links missing correct info
  dplyr::mutate(
    trp_id = dplyr::case_when(
      id == "1018298064" ~ "27566V121337",
      id == "1018298020" ~ "69803V121343",
      id == "1018877462" ~ "28020V121748",
      TRUE ~ trp_id
    )
  )


### Traffic work ----
traffic_work <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    length,
    trp_id,
    trafficVolumes
  ) |>
  # Some may be missing, often ferry links as per feb 24
  dplyr::filter(
    !(is.na(trafficVolumes))
  ) |>
  dplyr::mutate(
    volumes = purrr::map(trafficVolumes, ~ jsonlite::fromJSON(.x))
  ) |>
  tidyr::unnest(
    volumes
  ) |>
  dplyr::filter(
    trafficVolumeType == "GUESSTIMATED"
  ) |>
  dplyr::select(
    id,
    length,
    trp_id,
    traffic_work = trafficWorkValue
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(traffic_work) / 1e9,
    n_links = n()
  )

traffic_work_measured <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    length,
    trp_id,
    trafficVolumes
  ) |>
  # Some may be missing, often ferry links as per feb 24
  dplyr::filter(
    !(is.na(trafficVolumes)),
    trp_id %in% trps_2023$trp_id
  ) |>
  dplyr::mutate(
    volumes = purrr::map(trafficVolumes, ~ jsonlite::fromJSON(.x))
  ) |>
  tidyr::unnest(
    volumes
  ) |>
  dplyr::filter(
    trafficVolumeType == "GUESSTIMATED"
  ) |>
  dplyr::select(
    id,
    length,
    trp_id,
    traffic_work = trafficWorkValue
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(traffic_work) / 1e9,
    n_links = n()
  )

traffic_work_measured$traffic_work_mill_km / traffic_work$traffic_work_mill_km


### Chosen links ----
links_with_chosen_trp <-
  links |>
  dplyr::right_join(
    trps_2023,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    id,
    trp_id,
    name,
    length,
    road_reference,
    road_category_and_number,
    municipality_name,
    lat, lon,
    included,
    adt,
    status,
    final_status,
    reason,
    trp_label
  )

# missing_trps <-
#   trps_2023 |>
#   dplyr::filter(
#     !(trp_id %in% links_with_chosen_trp$trp_id)
#   )


readr::write_rds(
  links_with_chosen_trp,
  "chosen_links_krs_2023.rds"
)


# Kristiansand 2023 inner ----
municipality_names <-
  c(
    "Kristiansand"
  )

municipality_numbers <-
  municipalities |>
  dplyr::filter(
    municipality_name %in% municipality_names
  )


# Possible TRPs
trps_filtered <-
  points |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    operational_status != "RETIRED"
  ) |>
  dplyr::mutate(
    operational_status = stringr::str_to_lower(operational_status, locale = "no")
  ) |>
  dplyr::filter(
    municipality_name %in% municipality_names
  ) |>
  split_road_system_reference() |>
  dplyr::select(
    trp_id,
    name,
    operational_status,
    road_reference,
    road_category_and_number,
    municipality_name,
    lat, lon
  )

trp_aadts <-
  trps_filtered$trp_id |>
  get_aadt_for_trp_list()

trps_aadt_2023 <-
  trp_aadts |>
  dplyr::filter(
    year %in% c(2023)
  ) |>
  dplyr::mutate(
    valid_length_percent = round(valid_length_volume / adt * 100, digits = 1),
    # good enough coverage for index (50), sliding (84)
    good_enough = dplyr::case_when(
      (coverage > 50 & valid_length_percent > 98.5) ~ TRUE,
      TRUE ~ FALSE
    ),
    reason = dplyr::case_when(
      coverage <= 50 ~ "Lav dekningsgrad",
      valid_length_percent <= 98.5 ~ "Dårlige lengdemålinger",
      TRUE ~ ""
    ),
    adt = round(adt, -1)
  ) |>
  dplyr::select(
    trp_id,
    adt,
    good_enough,
    reason
  )

trps_2023 <-
  trps_filtered |>
  dplyr::left_join(
    trps_aadt_2023,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::mutate(
    reason = dplyr::case_when(
      trp_id %in%
        c(
          "58700V3366122", # Lohnelier
          "09525V2399484", # Brennåsen ø
          "26577V2399484", # Brennåsen v
          "98936V121303", # Vesterbrua
          "76952V121302", # Haumyrheia
          "86748V121742", # Haumyrheia
          "00000V1702751", # Timenes
          "54653V1751052", # Løehei
          "97410V1751020", # Songefjell
          "07242V1751122", # Kvernås
          "78032V121764", # Tveit kirke
          "66163V21769", # Kvennhusbekken
          "07306V22174" # Birkeland syd
        )
      ~ "Overrepresentasjon",
      TRUE ~ reason
    ),
    final_status =
      reason |>
      forcats::as_factor(),
    trp_label = paste(name, road_category_and_number, adt, sep = "<br/>"),
    trp_label = lapply(trp_label, htmltools::HTML)
  )


## Traffic links 2023 ----
# Geojson from ADM
links <-
  sf::st_read("C:/Users/snohan/Desktop/traffic_links_2023_2024-05-19.geojson") |>
  dplyr::rename(
    municipality_ids = municipalityIds,
    trp_id = primaryTrpId,
  ) |>
  dplyr::select(
    id,
    municipality_ids,
    trp_id,
    primaryTrpIds,
    associatedTrpIds,
    length,
    trafficVolumes
  ) |>
  tidyr::unnest(
    cols = municipality_ids
  ) |>
  dplyr::filter(
    municipality_ids %in% municipality_numbers$municipality_number
  ) |>
  dplyr::select(
    -municipality_ids
  ) |>
  # Remove duplicates (links crossing municipality borders inside city)
  dplyr::distinct() |>
  # manually update trp_id on links missing correct info
  dplyr::mutate(
    trp_id = dplyr::case_when(
      id == "1018298064" ~ "27566V121337",
      id == "1018298020" ~ "69803V121343",
      id == "1018877462" ~ "28020V121748",
      TRUE ~ trp_id
    )
  )


### Chosen links ----
links_with_chosen_trp <-
  links |>
  dplyr::right_join(
    trps_2023,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    id,
    trp_id,
    name,
    length,
    road_reference,
    road_category_and_number,
    municipality_name,
    lat, lon,
    adt,
    #status,
    final_status,
    reason,
    trp_label
  ) |>
  dplyr::filter(
    trp_id %in% c(
      "47215V121446", # Flekkerøy
      "11991V121784", # Kjos
      "45924V1955584", # Vågsbygdporten
      "26071V121746", # Fjellro
      "73283V1955438", # Tinnheiveien
      "22224V121366", # Vesterveitunnelen
      "04813V121368", # Avkjørsel ved Wilhelm Krag
      #"98936V121303", # Vesterbrua ?
      "02466V121760", # Grim
      "95764V121488", # Jernbanen
      "47077V121488", # Lundsbrua
      "69803V121343", # Rampe fra Eg
      "27566V121337", # Rampe mot Eg
      "57166V121303", # Baneheia v
      "40820V121304", # Baneheia ø
      "16947V121800", # Sødal
      #"05898V121506", # Østre ringvei sør ? arbeider i 2023?
      "99454V104306", # Tretjønnveien
      "12556V121486", # Prestheia
      "73380V121327", # Rampe mot Prestheia
      "30128V121328", # Rampe fra Østre ringvei
      "68804V121330", # Rampe fra Lund
      "20911V121329", # Rampe mot Lund
      #"22540V121303", # Prestebekken ? NEI
      "02489V121427", # Vollevannet
      #"33412V121301", # Narvigbakken ?
      "59306V121493", # Odderheia
      "04110V121776", # Strømme
      "15439V121417", # Øvre Strømme
      "20024V121776", # Sukkevannet
      "01355V1702751", # Hånes
      "27139V121510", # Stokkåsen
      "06702V121764", # Bjørndalen
      "58182V1787571", # Barselvannet
      "00000V1702725" # Skibåsen ? Litt feil plassert - skulle vært på kommunalvegen sørøst for rampekrysset
    )
  )

readr::write_rds(
  links_with_chosen_trp,
  "chosen_links_krs_2023_2.rds"
)
