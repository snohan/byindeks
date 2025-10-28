# Leftover code for special use

# Sub area
# sub_areas <-
#   trp_names |>
#   dplyr::select(
#     trp_id,
#     sub_area = municipality_name
#   )

# sub_area_36_month_trp_indices <-
#   calculate_rolling_indices(36, "by_sub_area")
#
# writexl::write_xlsx(
#   sub_area_36_month_trp_indices,
#   path = paste0(
#     "data_indexpoints_tidy/sub_area_rolling_indices_",
#     city_number,
#     ".xlsx"
#   )
# )


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


# TRPs with EMU3 ----
# Bergen 8952
# Buskerudbyen 1952
# Grenland 955
# Kristiansand og omegn 957 kommune 956
# Nedre Glomma 18952
# Nord-Jæren 952
# Oslo 959
# Trondheim 960
# Tromsø 961
# Tromsø 2022 16952
city_index_ids <-
  c(
    8952,
    1952,
    955,
    957,
    18952,
    952,
    959,
    960,
    16952
  )

city_trps <-
  purrr::map(
    city_index_ids,
    ~ get_published_pointindex_for_months(.x, 2024, 1)[[2]]
  ) |>
  purrr::list_rbind() |>
  dplyr::filter(
    trp_id != "98963V1719019" # Sandesund sør is wrongly included in API response
  )

city_trps_tidy <-
  city_trps |>
  dplyr::select(
    trp_id, area_name
  ) |>
  dplyr::distinct()


# From TRP API
source("get_from_trp_api.R")
trs_devices <- get_trs_device()

trs_device_latest <-
  trs_devices |>
  dplyr::slice_max(
    order_by = valid_from,
    by = trs_id
  ) |>
  dplyr::filter(
    device_type == "EMU"
  )

trs_trp_id <- get_trs_and_trp_id()

trp_meta_data <-
  readr::read_rds("trps_for_city_index.rds") |>
  dplyr::select(
    trp_id,
    trp_name = name,
    trp_road_category_and_number = road_category_and_number,
    municipality_name
  )


# Copied alarms from Adm, pasted to Excel and saved as CSV
alarms <-
  read.csv2(
    "spesialuttak/alarmer.csv"
  ) |>
  dplyr::select(
    trs_id,
    alarm_type
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(
    trs_id = as.character(trs_id)
  )

length(unique(alarms$trs_id))


# Finally
city_trs_with_emu3 <-
  trs_trp_id |>
  dplyr::select(
    trs_id,
    trs_name,
    trp_id
  ) |>
  dplyr::filter(
    trs_id %in% trs_device_latest$trs_id
  ) |>
  dplyr::inner_join(
    city_trps_tidy,
    by = join_by(trp_id)
  ) |>
  dplyr::left_join(
    trp_meta_data,
    by = join_by(trp_id)
  ) |>
  dplyr::relocate(area_name) |>
  dplyr::arrange(
    area_name, trs_id
  ) |>
  dplyr::left_join(
    alarms,
    by = join_by(trs_id)
  )

# With at least one alarm
city_trs_with_emu3 |>
  dplyr::filter(
    !is.na(alarm_type)
  ) |>
  dplyr::select(
    trs_id
  ) |>
  dplyr::distinct() |>
  base::nrow()


writexl::write_xlsx(
  city_trs_with_emu3,
  "spesialuttak/byindeks_trs_emu.xlsx"
)

# Compare exclusions of MDT and index ----
#source("compare_exclusions.R")s

# Theory ----
# Is the product of two normal variables still normal when means are close to 1 and with small deviation?
# Seems so
# library(extraDistr)
# n1 = extraDistr::rlst(1e4, 10, 1, .025)
# n2 = extraDistr::rlst(1e4, 10, 1, .025)
# #n1 <- rnorm(10000,1,.005)
# #n2 <- rnorm(10000,1,.005)
# n  <- n1*n2
# d  <- density(n)
# plot(d,lwd=2)
# x  <- par('usr')
# dn <- dnorm(d$x,mean=mean(n),sd=sd(n))
# x  <- seq(x[1],x[2],length.out=length(dn))
# lines(x, dn ,col=2, lwd=2)
# legend('topright', legend=c('Estimated density', 'Normal
#     distribution'), lwd=2, lty=c(1,1),col=c(1,2))

# Is the SD weighted from the API?
# test <-
#   trp_index_so_far_by_dec_from_2020 |>
#   dplyr::summarise(
#     n = n(),
#     sd = sd(index),
#     .by = year
#   )
# Yes, seems so.


# List of points with UTM ----
# library(sf)
# library(writexl)
#
# city_points_utm <-
#   this_citys_trps_all |>
#   dplyr::select(
#     name,
#     road_reference,
#     station_type,
#     lat, lon
#   ) |>
#   sf::st_as_sf(
#     coords = c("lon", "lat"),
#     crs = 4326
#   ) |>
#   sf::st_transform(32633) |>
#   dplyr::mutate(
#     utm33 = sf::st_as_text(geometry)
#   )
#
#
# city_points_utm |>
#   ggplot() +
#   geom_sf()
#
# city_points_utm |>
#   sf::st_drop_geometry() |>
#   writexl::write_xlsx(
#     path = "spesialuttak/trd_points_utm.xlsx"
#   )
#



# Length quality ----
plotly::ggplotly(
  mdt_filtered |>
    dplyr::filter(
      year == 2025
    ) |>
    ggplot(aes(year_month, length_quality, color = trp_id)) +
    geom_line()
)


## So far by index month ----
# Only relevant if index month is not 12!
# TODO: chained index - later!
# TODO: OLD VTI VERSION: fetch file given month for so far this year index
# As of now the csv files are implicitly containing index values for December's "so far".

# TODO: trp_index_so_far_by_index_month_pre_2020

trp_index_so_far_by_index_month_from_2020 <-
  trp_index_from_2020 |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "year_to_date",
    month == index_month
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    base_volume,
    index = index_short
  )

trp_index_year_to_date_by_index_month <-
  dplyr::bind_rows(
    #trp_index_so_far_by_index_month_pre_2020,
    trp_index_so_far_by_index_month_from_2020
  ) |>
  dplyr::filter(!is.na(base_volume)) |>
  dplyr::group_by(
    year
  ) |>
  dplyr::mutate(
    city_base_volume = sum(base_volume),
    squared_weight = (base_volume / sum(base_volume))^2
  ) |>
  dplyr::summarise(
    n_trp = n(),
    sum_of_squared_weights = sum(squared_weight)
  )

