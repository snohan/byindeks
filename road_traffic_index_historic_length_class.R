source("get_from_trafficdata_api.R")
source("split_road_system_reference.R")

# Per January 2022, no more than two lenght classes are available in the API
# Fetching published index from Traffic Data API
#index_2017 <- get_published_road_traffic_index_for_months(962, 2017, 12)

# Fetching from file
historic_length_indices <-
  read.csv2(
    "road_traffic_index_length_historic.csv"
  ) %>%
  # create new length classes
  dplyr::mutate(
    length_class_4 =
      dplyr::case_when(
        length_class == "Alle" ~ "Alle",
        length_class == "< 5,6m" ~ "< 5,6 m",
        length_class == "> 5,6m" ~ ">= 5,6 m",
        length_class == ">= 5,6m" ~ ">= 5,6 m",
        length_class == "5,6m - 7,6m" ~ "5,6 m - 7,6 m",
        length_class == "> 5,6m" ~ "5,6 m - 7,6 m",
        length_class == "7,6m - 12,5m" ~ "7,6 m - 12,5 m",
        length_class == "12,5m - 16,0m" ~ "12,5 m -",
        length_class == "16,0m - 24,0m" ~ "12,5 m -",
        length_class == ">= 16,0m" ~ "12,5 m -",
        length_class == "> 16,0m" ~ "12,5 m -",
        length_class == ">= 24,0m" ~ "12,5 m -",
      )
  ) %>%
  dplyr::group_by(
    year,
    length_class_4
  ) %>%
  dplyr::summarise(
    index_year_volume = sum(traffic_volume_index_year),
    base_year_volume = sum(traffic_volume_base_year),
    .groups = "drop"
  ) %>%
  dplyr::group_by(
    length_class_4
  ) %>%
  dplyr::mutate(
    index_i = index_year_volume / base_year_volume,
    cum_index_i = round(cumprod(index_i) * 100, digits = 1)
  ) %>%
  dplyr::arrange(
    length_class_4
  )

write.csv2(
  historic_length_indices,
  file = "historic_length_indices.csv",
  row.names = FALSE
)

yearly_indices <-
  index_2017 %>%
  dplyr::mutate(
    area_type =
      dplyr::case_when(
        area_type == "COUNTY" ~ "Fylke",
        area_type == "COUNTRY_PART" ~ "Landsdel",
        area_type == "COUNTRY" ~ "Hele landet"
      ),
    Lengdeklasse =
      dplyr::case_when(
        length_range == "[..,..)" ~ "Alle kjøretøy",
        length_range == "[..,5.6)" ~ "Lette kjøretøy",
        length_range == "[5.6,..)" ~ "Tunge kjøretøy"
      ),
    Dagtype =
      dplyr::case_when(
        day_type == "ALL" ~ "Alle døgn",
        day_type == "WEEKDAY" ~ "Yrkedøgn",
        day_type == "WEEKEND" ~ "Helgedøgn"
      ),
    Vegkategori =
      dplyr::case_when(
        road_category == "FYLKESVEG" ~ "Fylkesveg",
        road_category == "EUROPAVEG_RIKSVEG" ~ "Europa- og riksveg",
        road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG" ~ "Europa-, riks- og fylkesveg"
      ),
    Indeks = round(index_p, digits = 1),
    Standardavvik = round(standard_deviation, digits = 1),
    Periode =
      dplyr::case_when(
        period == "month" ~ "Måned",
        period == "year_to_date" ~ "Hittil i år"
      )
  ) %>%
  dplyr::filter(
    area_type == "COUNTRY"
  ) %>%
  dplyr::select(area_name, area_type, year, month, Vegkategori, Lengdeklasse, Dagtype,
                Indeks, Standardavvik, Periode)

