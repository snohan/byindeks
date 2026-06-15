# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
  library(stringi)
  library(writexl)
}

fylker <- get_counties()

traffic_work_from_traffic_links <- function(the_year) {

  traffic_links <-
    # Downloaded in Adm
    jsonlite::fromJSON(paste0("H:/my_data/traffic-links-", the_year, ".json")) |> 
    dplyr::select(
      id,
      function_class = functionClass,
      road_category = roadCategory,
      countyIds,
      trafficVolumes
    ) |> 
    tidyr::unnest(cols = countyIds) |> 
    # For links crossing county borders - which to choose?
    dplyr::slice_max(countyIds, by = id) |> 
    tidyr::unnest(cols = trafficVolumes) |> 
    dplyr::select(
      trafficLinkId,
      county_id = countyIds,
      function_class,
      road_category,    
      year,
      traffic_work = trafficWorkValue,
      trafficVolumeType
    ) |> 
    dplyr::mutate(
      road_category =
        dplyr::case_when(
          road_category %in% c("Europaveg", "Riksveg") ~ "ER",
          road_category %in% c("Fylkesveg") ~ "F",
          TRUE ~ road_category
        )
    ) |> 
    dplyr::filter(
      road_category %in% c("ER", "F")
    )

  traffic_links_overridden <-
    traffic_links |> 
    dplyr::filter(
      trafficVolumeType == "OVERRIDDEN",
      year == the_year
    ) |> 
    # Remove duplicates - which one is correct?
    dplyr::slice_max(traffic_work, by = trafficLinkId)

  traffic_links_not_overridden <-
    traffic_links |> 
    dplyr::filter(
      !(trafficLinkId %in% traffic_links_overridden$trafficLinkId),
      trafficVolumeType == "MODEL_RESULT",
      year == the_year
    )

  traffic_links_tidy <-
    dplyr::bind_rows(
      traffic_links_overridden,
      traffic_links_not_overridden
    )

  # Calculate traffic work
  traffic_work <-
    traffic_links_tidy |>
    dplyr::summarise(
      traffic_work_mill_km = sum(traffic_work) / 1e6,
      .by = c(county_id, year, road_category)
    ) |>
    dplyr::select(
      Fylkenr = county_id,
      ar = year,
      Vegkategori = road_category,
      trafikkarbeid = traffic_work_mill_km
    )

  return(traffic_work)
}


# Read traffic links ----
traffic_work_2024 <- traffic_work_from_traffic_links(2024)
traffic_work_2025 <- traffic_work_from_traffic_links(2025)

readr::write_rds(
  traffic_work_2025,
  file = "traffic_work_2025.rds"
)

## Excel
traffic_work_county_road_category <-
  dplyr::bind_rows(
    traffic_work_2024,
    traffic_work_2025
  ) |>
  tidyr::pivot_wider(
    names_from = "Vegkategori",
    names_prefix = "trafikkarbeid_",
    values_from = "trafikkarbeid"
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    trafikkarbeid_ERF = base::sum(trafikkarbeid_ER, trafikkarbeid_F, na.rm = T)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    prosentandel_ERF_landet = base::round(trafikkarbeid_ERF / base::sum(trafikkarbeid_ERF) * 100, 2),
    .by = ar
  ) |>
  dplyr::left_join(
    fylker,
    by = dplyr::join_by(Fylkenr == county_number)
  ) |>
  dplyr::arrange(geo_number) |> 
  dplyr::select(
    county_name,
    ar,
    trafikkarbeid_ER,
    trafikkarbeid_F,
    trafikkarbeid_ERF,
    prosentandel_ERF_landet
  ) |> 
  dplyr::mutate(
    dplyr::across(tidyselect::starts_with("trafikkarbeid"), ~ base::round(., 3))
  )

traffic_work_country_road_category <-
  dplyr::bind_rows(
    traffic_work_2024,
    traffic_work_2025
  ) |>
  dplyr::summarise(
    trafikkarbeid = base::sum(trafikkarbeid, na.rm = TRUE) |> base::round(3),
    .by = c(Vegkategori, ar)
  ) |> 
  dplyr::mutate(
    prosentandel = base::round(trafikkarbeid / sum(trafikkarbeid) * 100, 2),
    .by = ar
  )

traffic_work_stats_for_excel <-
  base::list(
    fylker = traffic_work_county_road_category,
    landet = traffic_work_country_road_category
  )

writexl::write_xlsx(
  traffic_work_stats_for_excel,
  "traffic_work/trafikkarbeid.xlsx"
)


## For weighting in VTI ----
jsonlite::write_json(
  traffic_work_2025,
  path = "traffic_work/trafikkarbeid_2025.json",
  prettify = TRUE
)
