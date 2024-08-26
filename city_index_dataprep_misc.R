# Leftover code for special use

## Chained pointindex ----
# drop this
# trp_index_from_refyear <-
#   this_citys_trp_index %>%
#   dplyr::select(
#     trp_id,
#     tidyselect::starts_with("index")
#   ) %>%
#   dplyr::filter(
#     dplyr::if_all(
#       .cols = tidyselect::starts_with("index"),
#       .fns = ~ !is.na(.x)
#     )
#   ) %>%
#   dplyr::mutate(
#     dplyr::across(
#       .cols = tidyselect::starts_with("index"),
#       .fns = ~ index_converter(.))
#   ) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(index = prod(c_across(tidyselect::starts_with("index")))) %>%
#   dplyr::mutate(index = round(100 * (index - 1), digits = 1)) %>%
#   dplyr::select(trp_id, index)
#
# this_citys_trp_index_refyear <-
#   this_citys_trp_index %>%
#   dplyr::left_join(trp_index_from_refyear)

# readr::write_rds(
#   #this_citys_trp_index_refyear,
#   this_citys_trp_index,
#   file = paste0("data_indexpoints_tidy/indekspunkt_", city_number, ".rds")
# )

# City index monthly ----
# city_monthly <-
#   dplyr::bind_rows(
#     monthly_city_index(city_index_2017),
#     monthly_city_index(city_index_2018),
#     monthly_city_index(city_index_2019),
#     monthly_city_index(city_index_2020),
#     monthly_city_index(city_index_2021),
#     monthly_city_index(city_index_2022)
#   ) %>%
#   dplyr::select(
#     area_name,
#     year,
#     month,
#     period,
#     month_object,
#     month_name,
#     index_p,
#     standard_deviation,
#     confidence_width,
#     base_volume,
#     calc_volume
#   ) %>%
#   dplyr::left_join(
#     n_points_per_month,
#     by = c("year", "month")
#   ) %>%
#   dplyr::mutate(
#     standard_error = round(standard_deviation / sqrt(n_points), digits = 1)
#   )
#
# write.csv2(
#   city_monthly,
#   file =
#     paste0(
#       "data_indexpoints_tidy/byindeks_maanedlig_",
#       city_number,
#       ".csv"
#     ),
#   row.names = F
# )

# MDT NorTraf ----
# How many MDTs in reference year from NorTraf?
# n_nortraf_mdt_reference_year <-
#   mdt |>
#   dplyr::filter(
#     length_range == "[..,5.6)",
#     year == reference_year
#   ) |>
#   dplyr::mutate(
#     is_nortraf = dplyr::if_else(is.na(coverage), 1, 0)
#   ) |>
#   dplyr::group_by(
#     trp_id
#   ) |>
#   dplyr::summarise(
#     n_months_nortraf = sum(is_nortraf)
#   )
#
# n_valid_mdt_reference_year <-
#   mdt_validated |>
#   dplyr::filter(
#     year == reference_year
#   ) |>
#   dplyr::mutate(
#     valid_quality = coverage >= 50 & length_quality >= 99
#   ) |>
#   dplyr::filter(
#     valid_quality == TRUE
#   ) |>
#   dplyr::group_by(
#     trp_id
#   ) |>
#   dplyr::summarise(
#     n_months_good_quality = n()
#   )
#
# this_citys_trp_nortraf_reference_year <-
#   points |>
#   dplyr::filter(trp_id %in% city_trps) |>
#   split_road_system_reference() |>
#   dplyr::select(
#     trp_id,
#     name,
#     road_category_and_number
#   ) |>
#   dplyr::left_join(
#     n_valid_mdt_reference_year,
#     by = "trp_id"
#   ) |>
#   dplyr::left_join(
#     n_nortraf_mdt_reference_year,
#     by = "trp_id"
#   ) |>
#   dplyr::filter(
#     n_months_good_quality >= 10,
#     n_months_nortraf > 4
#   )

# write.csv2(
#   this_citys_trp_nortraf_reference_year,
#   file = "nortraf_buskerudbyen.csv",
#   row.names = F,
#   fileEncoding = "latin1"
# )

# mdt_test <-
#   mdt |>
#   dplyr::filter(
#     length_range == "[..,5.6)",
#     is.na(sd_length_range)
#   )


# Valid length low in summer ----
# Nedre Glomma, TRP med klart lavere lengdekvalitet sommerstid
# trp_low_in_summer <-
#   mdt_filtered |>
#   dplyr::filter(
#     year_month == "2023-07-01",
#     length_quality < 99.5
#   ) |>
#   dplyr::left_join(
#     trp_names,
#     by = join_by(trp_id)
#   )
# # Alle 13 er EMU3
#
# trp_high_in_summer <-
#   mdt_filtered |>
#   dplyr::filter(
#     year_month == "2023-07-01",
#     length_quality > 99.94
#   ) |>
#   dplyr::left_join(
#     trp_names,
#     by = join_by(trp_id)
#   )

# pointindices_longformat_by_month <-
#   dplyr::bind_rows(
#     pointindex_20_all[[2]],
#     pointindex_21_all[[2]],
#     pointindex_22_all[[2]]
#   ) %>%
#   dplyr::filter(
#     day_type == "ALL",
#     is_excluded == FALSE,
#     is_manually_excluded == FALSE,
#     length_excluded == FALSE,
#     length_range == "lette"
#   ) |>
#   dplyr::group_by(year) %>%
#   dplyr::filter(
#     period == "month"
#   ) %>%
#   dplyr::select(
#     trp_id,
#     year,
#     month,
#     index
#   )


## TRP MDT table ----
# To show MDTs in a table in the report
# mdt_each_year <-
#   purrr::map_dfr(
#     years_from_reference_to_today,
#     ~ filter_mdt(mdt_filtered, .x)
#   ) |>
#   dplyr::select(
#     -n_months
#   ) |>
#   tidyr::pivot_wider(
#     names_from = year,
#     names_prefix = "mdt_",
#     values_from = mean_mdt
#   )
#
# city_trps_mdt <-
#   points %>%
#   dplyr::filter(trp_id %in% city_trps) %>%
#   split_road_system_reference() %>%
#   dplyr::select(
#     trp_id,
#     name,
#     road_category_and_number
#   ) %>%
#   dplyr::left_join(
#     mdt_each_year,
#     by = "trp_id"
#   ) |>
#   dplyr::mutate(
#     dplyr::across(
#       tidyselect::starts_with("mdt_"),
#       ~ round(
#         .x,
#         digits = -1
#         )
#     )
#   )
#
# readr::write_rds(
#   city_trps_mdt,
#   file =
#     paste0(
#       "data_indexpoints_tidy/city_trps_mdt_",
#       city_number,
#       ".rds"
#     )
# )



# Report ----
#```{r}
##| label: tbl-trp-2
##| tbl-cap: "Trafikkregistreringspunkt som inngår i byindeksen, del 2."
##| ft.arraystretch: 0.9

# if(nrow(all_point_info) > 30) {
#
#   if(city_number == 960) {
#     all_point_info |>
#       dplyr::slice(31:70) |>
#       create_point_table_trd() |>
#       flextable::footnote(
#         i = 1,
#         j = 2,
#         value = flextable::as_paragraph(
#           c("B er bomstasjon, T er trafikkregistreringspunkt.")
#         ),
#         ref_symbols = c("a"),
#         part = "header"
#       ) |>
#       flextable::fontsize(
#         size = 8,
#         part = "footer"
#       )
#   }else{
#     all_point_info |>
#       dplyr::slice(31:70) |>
#       create_point_table()
#   }
#
# }
#```


#```{r}
##| label: tbl-trp-3
##| tbl-cap: "Trafikkregistreringspunkt som inngår i byindeksen, del 3."
##| ft.arraystretch: 0.9

# if(nrow(all_point_info) > 70) {
#   all_point_info |>
#     dplyr::slice(71:100) |>
#     create_point_table()
# }

# {{< pagebreak >}}
#```


# Sara Fjellvær ----
# toll_station_dt_2019 <-
#   tolling_data_daily_all_years |>
#   dplyr::filter(
#     year == 2019
#   ) |>
#   dplyr::left_join(
#     toll_meta_data,
#     by = join_by(trp_id)
#   ) |>
#   dplyr::select(
#     trp_id,
#     name,
#     road_reference,
#     road_link_position,
#     lat, lon,
#     date,
#     month,
#     day,
#     class,
#     traffic_volume = traffic
#   )
#
# library(writexl)
# writexl::write_xlsx(
#   toll_station_dt_2019,
#   "spesialuttak/bomdata_trd_2019.xlsx"
# )