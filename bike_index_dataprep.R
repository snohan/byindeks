source("rmd_setup.R")
source("get_from_trafficdata_api.R")


# Fetching published index from Traffic Data API
index_2018_grl <- get_published_index_for_months(4953, 2018, 12)
index_2018_osl <- get_published_index_for_months(6953, 2018, 12)
index_2018_frd <- get_published_index_for_months(9952, 2018, 12)

index_2019_ngl <- get_published_index_for_months(5953, 2019, 12)
index_2019_grl <- get_published_index_for_months(4953, 2019, 12)
index_2019_brg <- get_published_index_for_months(5952, 2019, 12)
index_2019_osl <- get_published_index_for_months(6953, 2019, 12)
index_2019_frd <- get_published_index_for_months(9952, 2019, 12)

index_2020_ngl <- get_published_index_for_months(5953, 2020, 12)
index_2020_grl <- get_published_index_for_months(4953, 2020, 12)
index_2020_brg <- get_published_index_for_months(5952, 2020, 12)
index_2020_osl <- get_published_index_for_months(6953, 2020, 12)
index_2020_njr <- get_published_index_for_months(6952, 2020, 12)
index_2020_frd <- get_published_index_for_months(9952, 2020, 12)

index_2021_ngl <- get_published_index_for_months(5953, 2021, 12)
index_2021_grl <- get_published_index_for_months(4953, 2021, 12)
index_2021_brg <- get_published_index_for_months(5952, 2021, 12)
index_2021_osl <- get_published_index_for_months(6953, 2021, 12)
index_2021_njr <- get_published_index_for_months(6952, 2021, 12)
index_2021_frd <- get_published_index_for_months(9952, 2021, 12)

index_2022_ngl <- get_published_index_for_months(5953, 2022, 12)
index_2022_grl <- get_published_index_for_months(4953, 2022, 12)
index_2022_brg <- get_published_index_for_months(5952, 2022, 12)
index_2022_osl <- get_published_index_for_months(6953, 2022, 12)
index_2022_njr <- get_published_index_for_months(6952, 2022, 12)
index_2022_frd <- get_published_index_for_months(9952, 2022, 12)
index_2022_trm <- get_published_index_for_months(11952, 2022, 12)

index_all_years <- dplyr::bind_rows(
  index_2018_grl,
  index_2018_osl,
  index_2018_frd,
  #
  index_2019_ngl,
  index_2019_grl,
  index_2019_brg,
  index_2019_osl,
  index_2019_frd,
  #
  index_2020_ngl,
  index_2020_grl,
  index_2020_brg,
  index_2020_osl,
  index_2020_njr,
  index_2020_frd,
  #
  index_2021_ngl,
  index_2021_grl,
  index_2021_brg,
  index_2021_osl,
  index_2021_njr,
  index_2021_frd,
  #
  index_2022_ngl,
  index_2022_grl,
  index_2022_brg,
  index_2022_osl,
  index_2022_njr,
  index_2022_frd,
  index_2022_trm
) %>%
  dplyr::filter(
    length_range == "[..,..)",
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG"
  ) %>%
  dplyr::select(
    area_name,
    year,
    month,
    period,
    index_p,
    standard_deviation,
    coverage = volumeIndexCoverage.hours.percentage
  ) %>%
  dplyr::mutate(
    month_object = lubridate::make_date(year = year, month = month),
    month_object_2000 = lubridate::make_date(year = 2000, month = month),
    month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE),
    area_name = dplyr::case_when(
      area_name == "Oslo" ~ "Osloområdet",
      TRUE ~ area_name
    )
  )

readr::write_rds(
  index_all_years,
  file = "data_indexpoints_tidy/all_bike_indices.rds"
)
