# Compare exclusions of MDT and index ----
# Check that the "same" exclusions are used on PI as MDT
# TRD toll station MDTs already have the same exclusions
# if(city_number == 960){
#
#   mdt_and_pi <-
#     mdt_validated |>
#     dplyr::filter(
#       coverage >= 50,
#       length_quality >= 98.5
#     ) |>
#     dplyr::left_join(
#       dplyr::select(trp_toll_index_monthly, -year, -month, -length_range), # TRD
#       by = c("trp_id", "year_month" = "month_object") # TRD
#     ) |>
#     dplyr::left_join(
#       trp_names,
#       by = "trp_id"
#     ) |>
#     dplyr::select(
#       trp_id,
#       name,
#       year,
#       month,
#       mdt_coverage = coverage,
#       length_quality,
#       mdt,
#       index
#     ) |>
#     dplyr::arrange(
#       name,
#       trp_id,
#       year,
#       month
#     )
# }else{
#
#   mdt_and_pi <-
#     mdt_validated |>
#     dplyr::filter(
#       coverage >= 50,
#       length_quality >= 98.5
#     ) |>
#     dplyr::left_join(
#       trp_index_monthly,
#       by = c("trp_id", "year", "month")
#     ) |>
#     dplyr::left_join(
#       trp_names,
#       by = "trp_id"
#     ) |>
#     dplyr::select(
#       trp_id,
#       name,
#       year,
#       month,
#       mdt_coverage = coverage,
#       length_quality,
#       mdt,
#       index
#     ) |>
#     dplyr::arrange(
#       name,
#       trp_id,
#       year,
#       month
#     )
#
# }
#
# mdt_and_pi_check <-
#   mdt_and_pi |>
#   dplyr::select(
#     name,
#     year,
#     month,
#     mdt,
#     index
#   ) |>
#   tidyr::complete(
#     name,
#     year,
#     month
#   ) |>
#   dplyr::mutate(
#     month_label = lubridate::make_date(
#       year = 2000,
#       month = month,
#       day = 1
#     ) |>
#       lubridate::month(label = TRUE),
#     valid_value =
#       dplyr::case_when(
#         is.na(mdt) & is.na(index) ~ "",
#         is.na(mdt) & !is.na(index) ~ "index",
#         !is.na(mdt) & is.na(index) ~ "mdt",
#         !is.na(mdt) & !is.na(index) ~ "BOTH",
#       )
#   ) |>
#   dplyr::select(
#     -month,
#     -mdt,
#     -index
#   ) |>
#   tidyr::pivot_wider(
#     names_from = "month_label",
#     values_from = "valid_value"
#   )
