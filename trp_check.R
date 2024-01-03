# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
  library(readxl)
  library(writexl)
}

trp_timespan <-
  get_trp_data_time_span() |>
  dplyr::select(-first_data)


trp <-
  get_points() |>
  dplyr::filter(
    traffic_type == "BICYCLE"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    municipality_name
  ) |>
  dplyr::distinct() |>
  dplyr::left_join(
    trp_timespan,
    by = dplyr::join_by(trp_id)
  )



# Oslo bike index TRP ----
oslo_trp <-
  readxl::read_excel(
    "oslo_sykkelindeks_trp.xlsx"
  ) |>
  dplyr::left_join(
    trp,
    by = dplyr::join_by(name)
  ) |>
  dplyr::relocate(trp_id)

writexl::write_xlsx(
  oslo_trp,
  path = "oslo_sykkelindeks_trp_redigert.xlsx"
)
