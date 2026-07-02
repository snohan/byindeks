#
base::Sys.setlocale(locale = "nb.utf8")
source("apar.R")
source("toll_station_functions.R")


# 2019 - 2021-04 ----
# Filer fra BFIN (Nina Lysfjord)
# Stasjonsinfo
tollstations_nj_bfin <- 
  readxl::read_excel("bomdata_nj/nj_bomstasjoner_bfin.xlsx") |> 
  dplyr::select(
    trp_id = ChargingPointID,
    navn_csn = 'Navn i CSN',
    navn_analytics = 'Navn i Analytics'
  ) |> 
  dplyr::mutate(
    trp_id = base::as.character(trp_id)
  )

# Bomdata per måned
tolldata_nj_month_bfin <-
  readxl::read_excel("bomdata_nj/nj_bomdata_bfin.xlsx") |> 
  dplyr::select(
    navn_csn = Bomstasjon,
    month_orig = Måned,
    lmv = 'Liten bil',
    hmv = 'Stor bil',
    no_class = Ukjent
  ) |> 
  dplyr::left_join(
    tollstations_nj_bfin |> 
      dplyr::select(-navn_analytics),
    by = "navn_csn"
  ) |> 
  dplyr::mutate(
    month_object = base::paste0("01.", month_orig) |> lubridate::dmy()
  ) |> 
  dplyr::select(
    trp_id, month_object, lmv, hmv, no_class
  ) |> 
  dplyr::mutate(
    month_no = lubridate::month(month_object),
    year = lubridate::year(month_object),
    sum_class = (lmv + hmv),
    lmv_ratio = lmv / sum_class,
    no_class_lmv_ratio = no_class * lmv_ratio,
    no_class_to_lmv = 
      dplyr::case_when(
        no_class_lmv_ratio - base::floor(no_class_lmv_ratio) < 0.5 ~ base::floor(no_class_lmv_ratio),
        no_class_lmv_ratio - base::floor(no_class_lmv_ratio) >= 0.5 ~ base::ceiling(no_class_lmv_ratio)
      ),
    lmv_adj = lmv + no_class_to_lmv,
    hmv_adj = hmv + (no_class - no_class_to_lmv)
    # diff_adj = sum_class + no_class - lmv_adj - hmv_adj # All zero! :)
  ) |> 
  dplyr::select(
    trp_id, month_object, month_no, year,
    lmv = lmv_adj,
    hmv = hmv_adj
  )
  
readr::write_rds(tolldata_nj_month_bfin, "bomdata_nj/nj_bomdata_bfin.rds")

# 2021-05 ----
tolldata_nj_month_2025 <-
  readr::read_csv("bomdata_nj/nj_bomdata_month_2025.csv") |> 
  dplyr::select(
    trp_id = 'toll station code',
    class = 'vehicle class ID',
    month_no = 'month no.',
    year,
    traffic = 'Accepted passages'
  ) |> 
  dplyr::mutate(
    trp_id = base::as.character(trp_id),
    month_object = base::paste0("01-", month_no, "-", year) |> lubridate::dmy(),
    class = dplyr::case_when(
      class == 1 ~ "lmv",
      class == 2 ~ "hmv"
    )
  ) |> 
  dplyr::select(
    trp_id, month_object, month_no, year, class, traffic
  ) |> 
  tidyr::pivot_wider(
    names_from = class,
    values_from = traffic
  )

readr::write_rds(tolldata_nj_month_2025, "bomdata_nj/nj_bomdata_2025.rds")


# Compare 2025 to 2019
tolldata_nj_2019_2025_month <-
  dplyr::full_join(
    readr::read_rds("bomdata_nj/nj_bomdata_bfin.rds") |> 
      dplyr::filter(year == 2019) |> 
      dplyr::select(trp_id, month_no, lmv, hmv) |> 
      dplyr::mutate(
        # Toll stations around Tananger have changed ID and direction
        trp_id = 
          dplyr::case_when(
            trp_id == "601" ~ "603",
            trp_id == "602" ~ "604",
            TRUE ~ trp_id
          )
      ),
    readr::read_rds("bomdata_nj/nj_bomdata_2025.rds") |> 
      dplyr::select(trp_id, month_no, lmv, hmv),
    by = dplyr::join_by(trp_id, month_no),
    suffix = c("_2019", "_2025")
  ) |> 
  dplyr::mutate(
    index_p = 100 * (lmv_2025 / lmv_2019 - 1)
  ) |> 
  dplyr::filter(
    !(trp_id %in% c(101, 114))
  ) |> 
  dplyr::arrange(trp_id, month_no)

readr::write_rds(tolldata_nj_2019_2025_month, "bomdata_nj/nj_bomstasjonsindeks_2019_2025.rds")

# Rare data
# 1. 109 ved sykehuset har lavere tall i nov og des 2025 - hvorfor?
# 2. Bybrua har skiftet ID fra 101 til 114. Den har byttet innkrevingsretning?
#    Men i 2025 går mye av trafikken i Hundvågtunnelen i stedet for over Bybrua. Da må bomstasjoene i tunnelen være med. 
#    Må da ta bare den ene kjøreretningen som er lik den ene som fanges opp av 114.
#    Men sammenligningen blir ikke komplett da en del av trafikken som i dag går i tunnelen, gikk med ferje i 2019.
#    Ergo er det best å utelate bomstasjonen på Bybrua fra sammenligningen.
