# Write city index to Excel ----

if(city_number %in% c(19954, 19955, 20952)){

  list(
    punkt_adt = trp_info_adt,
    punktindeks_maned = trp_index_monthly_wide,
    byindeks_aarlig = city_index_yearly_all,
    byindeks_hittil = city_index_yearly_all_so_far,
    by_glid_indeks = all_rolling_indices
  ) |>
    writexl::write_xlsx(
      path = paste0(
        "data_indexpoints_tidy/tallmateriale_",
        city_number,
        ".xlsx"
      )
    )
}

if(city_number == 16952){

  list(
    punkt_adt = trp_info_adt,
    punktindeks_maned = trp_index_monthly_wide,
    byindeks_aarlig = city_index_final,
    by_glid_indeks = all_rolling_indexes_chained
  ) |>
    writexl::write_xlsx(
      path = paste0(
        "data_indexpoints_tidy/tallmateriale_",
        city_number,
        ".xlsx"
      )
    )
}

if(city_number == 960){

  list(
    punkt_adt = this_citys_trps_all_adt_final_index,
    punktindeks_maned = trp_index_monthly_wide,
    byindeks_aarlig = city_index_yearly_all,
    byindeks_hittil = city_index_so_far_all,
    by_glid_indeks = all_rolling_indices
  ) |>
    writexl::write_xlsx(
      path = paste0(
        "data_indexpoints_tidy/tallmateriale_",
        city_number,
        ".xlsx"
      )
    )
}

if(city_number %in% c(959)){

  list(
    punkt_adt = trp_info_adt,
    punktindeks_maned = trp_index_monthly_wide,
    byindeks_aarlig = city_index_yearly_all,
    byindeks_hittil = city_index_yearly_all_so_far,
    byindeks_aarlig_sub = city_index_yearly_all_sub,
    byindeks_aarlig_sub_hittil = city_index_yearly_all_sub_so_far,
    by_glid_indeks = all_rolling_indices,
    by_glid_indeks_sub = all_rolling_indices_sub
  ) |>
    writexl::write_xlsx(
      path = paste0(
        "data_indexpoints_tidy/tallmateriale_",
        city_number,
        ".xlsx"
      )
    )
}

if(!(city_number %in% c(959, 960, 16952, 19954, 19955, 20952))){

  list(
    punkt_adt = trp_info_adt,
    punktindeks_maned = trp_index_monthly_wide,
    byindeks_aarlig = city_index_yearly_all,
    byindeks_hittil = city_index_yearly_all_so_far,
    by_glid_indeks = all_rolling_indices
  ) |>
    writexl::write_xlsx(
      path = paste0(
        "data_indexpoints_tidy/tallmateriale_",
        city_number,
        ".xlsx"
      )
    )
}