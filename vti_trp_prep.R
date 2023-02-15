source("H:/Programmering/R/byindeks/rmd_setup.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
library(writexl)

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
    "90465V384130",
    "01058V384504",
    "97639V384489",
    "62929V384083",
    "05746V2353431",
    "74896V384121",
    "05960V384024",
    "09537V384484",
    "52148V1797330",
    "01368V384495",
    "66711V384480",
    "09390V384481",
    "84325V384020",
    "69452V384101",
    "96103V384037",
    "43761V1724813",
    "43916V384471",
    "49965V384062",
    "25641V1699273",
    "18129V804739",
    "16793V804739",
    "78019V2370741",
    "64438V2370744",
    "66206V805614",
    "62279V805789",
    "61215V2782426",
    "56658V804775",
    "56438V804776",
    "02940V805615",
    "56100V804816",
    "56190V804815",
    "29810V804818",
    "53289V804779",
    "10853V1822314",
    "83931V1822326",
    "04340V1885974",
    "90914V1885978",
    "03658V1885986",
    "20790V1885953",
    "44953V1885943",
    "95067V1885981",
    "17350V1885991",
    "74808V805815",
    "22576V805808",
    "84212V805616",
    "13093V804836",
    "78845V804838",
    "23412V804831",
    "43672V804832",
    "09194V804833",
    "79902V804834",
    "67055V804829",
    "25132V805616",
    "22439V804830",
    "20435V804835",
    "19690V804744",
    "31498V2574580",
    "81885V2574569",
    "56025V2078107",
    "43259V2078119",
    "68733V2078106",
    "18257V2078120",
    "32158V2482021",
    "70948V2482020",
    "87699V804741",
    "55153V805604",
    "97841V805708",
    "90500V805707",
    "22326V805039",
    "47932V805038",
    "57587V805035",
    "85389V805037",
    "00917V805315",
    "06872V2518838",
    "13617V2518814",
    "64982V805707",
    "54644V2518837",
    "87780V2518835",
    "51157V805707",
    "92869V805019",
    "04468V805022",
    "90132V805016",
    "14706V805707",
    "33729V805012",
    "12515V805011",
    "54608V320601",
    "51936V319817",
    "49950V319650",
    "30193V2370595",
    "74885V319528",
    "68684V319527",
    "40190V319527",
    "03108V320583",
    "61929V2422166",
    "85039V2725969",
    "10795V320297",
    "58562V320296",
    "55507V319881",
    "32842V319521",
    "68351V319882",
    "10239V2725979",
    "62464V2725991",
    "25926V2725990",
    "92743V2726085",
    "33926V2721315",
    "91556V2721320",
    "20586V2721334",
    "95626V320634",
    "99781V2303021",
    "15931V2303033",
    "89457V2303027",
    "88184V2303031",
    "13606V2303025",
    "84064V320581",
    "71798V319583",
    "13433V319582",
    "35382V1727514",
    "50741V1727509",
    "40696V1727469",
    "97275V319853",
    "00462V320124",
    "95509V319514",
    "90666V320576",
    "13000V2783697",
    "94180V2726102",
    "65096V2726170",
    "06011V2726196",
    "06390V1204620",
    "00870V1204449",
    "91602V21669",
    "11219V22151",
    "61942V2809673",
    "06443V21826",
    "72950V21826",
    "07242V1751122",
    "18321V121493",
    "01355V1702751",
    "11991V121784",
    "09525V2399484",
    "68018V121746",
    "60585V521136",
    "70643V2982045",
    "86022V521170",
    "00344V521377",
    "94845V521171",
    "14213V521376",
    "95297V2782243",
    "26489V521174",
    "79830V521202",
    "97413V521398",
    "79492V521118",
    "06938V521177",
    "33324V521399",
    "94756V521254",
    "54228V1210081",
    "12634V521208",
    "04267V1175832",
    "38613V2620349",
    "45730V1175656",
    "11446V1175840",
    "03520V1175848",
    "24127V1175846",
    "30201V1175821",
    "48148V1175464",
    "62500V2791531",
    "31429V2791530",
    "81446V625466",
    "18934V625464",
    "30017V625457",
    "29852V2460300",
    "44756V1863949",
    "29403V625517",
    "57234V2491268",
    "89040V2545188",
    "14231V2545190",
    "02924V625304",
    "26619V625212",
    "06154V625229",
    "39940V625245",
    "91334V625635",
    "27193V2516106",
    "51368V625246",
    "13447V625213",
    "19702V625216",
    "03375V625405",
    "66346V625214",
    "23392V625266",
    "67570V625213",
    "89041V625265",
    "73166V2312244",
    "31290V625405",
    "51029V625643",
    "90697V625213",
    "04477V625649",
    "12707V625214",
    "49763V2367192",
    "10254V625614",
    "37530V625530",
    "20690V625530",
    "33431V625542",
    "22325V625540",
    "11462V625377",
    "08916V625376",
    "14598V625562",
    "37866V625564",
    "83502V625530",
    "47140V625367",
    "98792V181304",
    "62909V181278",
    "53370V181307",
    "15388V181306",
    "67864V181273",
    "42437V2352346",
    "85451V2352342",
    "16140V2352342",
    "45380V2352337",
    "17036V181271",
    "72438V180781",
    "35376V181262",
    "25999V3099835",
    "89755V180856",
    "96851V180854",
    "94099V2038395",
    "39256V181280",
    "87557V180903",
    "09389V443429",
    "15135V443654",
    "61592V443890",
    "67786V443870",
    "11693V443793",
    "76407V1773686",
    "69184V2802005",
    "37754V1773669",
    "44176V444229",
    "62753V444229",
    "25219V1773688",
    "64412V443607",
    "23620V443607",
    "68557V444232",
    "53805V444232",
    "39054V444232",
    "69931V443604",
    "00222V444290",
    "17096V443647",
    "04925V444232",
    "99246V443317",
    "71728V443308",
    "74031V1857387",
    "68234V443152",
    "18959V1796996",
    "00000V1796947",
    "65345V1796950",
    "77061V444230",
    "94949V444217",
    "21405V443151",
    "71609V444217",
    "07717V2682280",
    "92246V2682281",
    "65271V443150",
    "17208V971599",
    "15322V971307",
    "03364V971801",
    "65179V1209937",
    "96583V971490",
    "58345V1719147",
    "04904V971774",
    "35229V971507",
    "82913V971429",
    "08132V1984223",
    "69356V971439",
    "35530V971496",
    "60371V1719219",
    "16035V971817",
    "72753V1896595",
    "98144V2554057",
    "88604V443293",
    "58722V2586343",
    "74943V443384",
    "02846V443400",
    "18034V444240",
    "10349V444025",
    "32088V444219",
    "34395V443402",
    "66990V444219",
    "86357V444219",
    "30552V444220",
    "18512V1665976",
    "53771V1204217",
    "35666V443378",
    "33545V1219589",
    "73894V444221",
    "96571V444221",
    "77470V2664206",
    "36944V2664207",
    "68651V2350599",
    "89862V443455",
    "49769V443454",
    "53995V443452",
    "09932V443457",
    "14775V443441",
    "93714V443439",
    "58042V1060046",
    "67263V1811577",
    "63515V1811747",
    "18788V1811746",
    "95703V2037788",
    "72111V2038086",
    "20808V2037771",
    "88780V2282345",
    "19084V704693",
    "17681V704560",
    "15329V704987",
    "14051V2499835",
    "57114V705039",
    "10662V704614",
    "04361V3106858",
    "28955V704576",
    "96661V3106834",
    "41667V3106834",
    "14709V3106834",
    "26093V704634",
    "65896V704749",
    "07511V704747",
    "62237V2823404",
    "45625V2823431",
    "54219V2823423",
    "46383V704525",
    "63816V2823438",
    "63819V2822700",
    "20796V3238279",
    "75918V3238278",
    "51612V2822714",
    "66729V3121285",
    "43983V3121286",
    "45078V2822708",
    "10538V3287376",
    "60879V3287375",
    "63825V2822711",
    "04474V1060615",
    "15967V1203619",
    "41909V2583664",
    "58272V2583663",
    "56109V1060668",
    "25754V3134152",
    "00598V1060635",
    "92496V1060637",
    "40191V1060144",
    "03556V2741763",
    "48559V2741763",
    "82976V1060642",
    "93356V2741763",
    "03080V1060138",
    "19923V1060616",
    "84996V1060616",
    "89367V1060739",
    "57449V2237119",
    "69053V1060617",
    "81103V1059995",
    "44161V2411262",
    "93565V2411267",
    "46801V2562048",
    "63808V1060623",
    "88480V2562065",
    "42465V1060624",
    "03711V1060662",
    "89879V1060627",
    "74892V1059997",
    "72532V1060043",
    "26685V704589",
    "48178V705242",
    "67004V22180",
    "14061V443595",
    "28516V444278",
    "15913V443596",
    "12556V121486",
    "19646V885990",
    "71535V319524",
    "31891V249584",
    "98655V249011",
    "70525V578254",
    "93526V805122",
    "99259V805430",
    "21376V2607269"
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
    path = "new_vti/revise_vti_trp_2023.xlsx"
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
