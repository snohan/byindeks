source("H:/Programmering/R/byindeks/rmd_setup.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")

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
    valid_from < "2022-01-01",
    latest_day > "2023-02-01"
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
  )

#remove(trp_for_vti)

# Traffic links ----
# Geojson files from ADM
links <- sf::st_read("H:/Programmering/R/aadt_model/traffic-links-2021.geojson")

# CSV from Kibana
links_info <- readr::read_csv2("H:/Programmering/R/aadt_model/links_info.csv") |>
  dplyr::select(
    nvdb_id,
    functional_class_high = functional_road_class_info.highest,
    functional_class_low = functional_road_class_info.lowest,
    ferry = is_ferry_traffic_link,
    directions = lanes_and_directions_info.direction_types,
    road_category = location.road_category,
    urban_ratio,
    lanes_max = lanes_and_directions_info.max_num_lanes,
    lanes_min = lanes_and_directions_info.min_num_lanes,
    trp = primary_trp,
    speed_high = speed_limit_info.highest,
    speed_low = speed_limit_info.lowest,
  )

links_tidy <-
  links |>
  dplyr::select(
    nvdb_id = nvdbId,
    length
  ) |>
  dplyr::left_join(
    links_info,
    by = "nvdb_id"
  ) |>
  dplyr::mutate(
    trp_id = stringr::str_replace(trp, "^-$", NA_character_)
  ) |>
  dplyr::select(
    nvdb_id,
    length,
    trp_id
  ) |>
  dplyr::filter(
    !is.na(trp_id)
  )

# Check
trp_without_link <-
  trp_for_vti_tidy |>
  dplyr::anti_join(
    links_tidy,
    by = "trp_id"
  )
# These are on same link as others (among them are the ones on only half the road)

remove(links)
remove(links_info)

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

not_chosen <-
  c(
    "33419V930316",
    "72525V930309",
    "14664V930577",
    "66883V930281",
    "09413V2801655",
    "12750V930437",
    "07072V930629",
    "08346V930598",
    "34500V930598",
    "48657V2678479",
    "80998V1125915",
    "55092V1125799",
    "31958V1125796",
    "28120V1126319",
    "56700V2674745",
    "06273V1126310",
    "42289V1126309",
    "95678V1125814",
    "39464V1126283",
    "90978V1126285",
    "14354V2486023",
    "27227V885156",
    "63025V885155",
    "31675V885942",
    "51449V885379",
    "96529V885935",
    "24290V885934",
    "31059V885934",
    "52015V885837",
    "95011V1665559",
    "01513V1665565",
    "63904V885194",
    "29852V885202",
    "08581V885541",
    "29133V1201961",
    "26549V2669516",
    "71847V886007",
    "51398V885394",
    "86717V885139",
    "71182V885135",
    "95077V885274",
    "07664V885976",
    "51478V578612",
    "91007V578610",
    "92316V578609",
    "89711V578609",
    "31504V578608",
    "99915V578630",
    "09664V2566902",
    "32773V2566902",
    "47623V578084",
    "76881V578085",
    "65732V578598",
    "50089V578151",
    "66419V578640",
    "81214V578078",
    "32599V578080",
    "83441V578079",
    "34194V578077",
    "05420V578059",
    "37345V1742835",
    "00387V578273",
    "74706V578650",
    "01622V578649",
    "16211V2390266",
    "07599V2390266",
    "42194V2506518",
    "97115V2506512",
    "84237V578097",
    "99467V578621",
    "00902V578622",
    "03614V1740869",
    "17937V578125",
    "41111V72156",
    "32375V72155",
    "78492V2394249",
    "63028V72219",
    "44660V72811",
    "04300V72813",
    "81077V72158",
    "91594V1677415",
    "36935V72359",
    "55570V3128090",
    "41020V3128088",
    "47447V72878",
    "40308V72241",
    "80360V72241",
    "06438V2413672",
    "99593V72151",
    "31770V2799092",
    "62393V72804",
    "24362V72144",
    "00430V249404",
    "63446V249644",
    "56861V249640",
    "21648V249012",
    "38053V2473660",
    "10701V249565",
    "77303V249268",
    "81047V249564",
    "04843V249564",
    "00786V249561",
    "87500V2407154",
    "81921V2407139",
    "31556V249594",
    "70361V249595",
    "74472V249451",
    "00508V249319",
    "46105V249580",
    "89787V249576",
    "19529V225821",
    "25589V249575",
    "75649V249438",
    "14626V249527",
    "10493V249543",
    "91810V2497857",
    "99999V2568304",
    "01482V248909",
    "42912V249526",
    "70836V249525",
    "97129V249051",
    "13279V248971",
    "92599V249619",
    "00311V249059",
    "21607V249612",
    "65035V248952",
    "38589V248954",
    "85956V248955",
    "58648V248949",
    "91512V248948",
    "82819V248945",
    "07607V249425",
    "25819V2511481",
    "54369V2511480",
    "00844V249651",
    "29293V249601",
    ""
  )

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
pointindex_last_year <- get_published_pointindex_for_months_paginated(962, 2022, 12)

trps_last_year <- pointindex_last_year[[1]]

trps_last_year_to_keep <-
  trp_for_vti_aadt |>
  dplyr::filter(
    trp_id %in% trps_last_year
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
  )

trps_to_add <-
  trp_for_vti_aadt |>
  dplyr::filter(
    !(trp_id %in% trps_last_year)
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
