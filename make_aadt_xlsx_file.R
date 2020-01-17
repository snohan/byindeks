# Get AADTs for specific trps
library(writexl)

points <- get_trp_for_vti()

# Filter away periodic
periodic_trp <- getPointsFromTRPAPI_filtered()

geodata_1 <- c(54, 18, 50, 15)
geodata_2 <- c(11, 38, 42, 46)
geodata_3 <- c(3, 30, 34)

points_chosen <- points %>%
  dplyr::select(-road_category, -lat, -lon,
                -road_link_position) %>%
  dplyr::filter(!(trp_id %in% periodic_trp$trp_id)) %>%
  dplyr::filter(county_number %in% geodata_3) %>%
  dplyr::select(-county_number, -first_commission_datainn)

aadt_chosen_raw <- getAdtForpoints_by_length(points_chosen$trp_id)

aadt_chosen <- aadt_chosen_raw %>%
  dplyr::select(-sd_length_range) %>%
  dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
  dplyr::mutate(length_range =
                  case_when(length_range == "[..,5.6)" ~ "lette",
                            length_range == "[5.6,..)" ~ "tunge"),
                klasse1 = length_range,
                klasse2 = length_range,
                godkjent_lengde = round(aadt_valid_length / aadt_total * 100,
                                        digits = 1),
                coverage = round(coverage, digits = 1)) %>%
  tidyr::pivot_wider(names_from = c(klasse1, klasse2, length_range),
                     values_from = c(aadt_length_range,
                                     aadt_ci_lowerbound_length_range,
                                     aadt_ci_upperbound_length_range)) %>%
  dplyr::rename(dekningsgrad = coverage,
                aadt_ki_start_total = aadt_ci_lowerbound_total,
                aadt_ki_slutt_total = aadt_ci_upperbound_total,
                aadt_lette = aadt_length_range_lette_lette_lette,
                aadt_ki_start_lette = aadt_ci_lowerbound_length_range_lette_lette_lette,
                aadt_ki_slutt_lette = aadt_ci_upperbound_length_range_lette_lette_lette,
                aadt_tunge = aadt_length_range_tunge_tunge_tunge,
                aadt_ki_start_tunge = aadt_ci_lowerbound_length_range_tunge_tunge_tunge,
                aadt_ki_slutt_tunge = aadt_ci_upperbound_length_range_tunge_tunge_tunge) %>%
  dplyr::select(-aadt_valid_length, -total.volume.confidenceInterval) %>%
  dplyr::select(trp_id:aadt_lette, aadt_ki_start_lette, aadt_ki_slutt_lette,
                aadt_tunge, aadt_ki_start_tunge, aadt_ki_slutt_tunge)

points_chosen_aadt <- points_chosen %>%
  dplyr::left_join(aadt_chosen)

geodata_3_aadt <- points_chosen_aadt


geodata_x_aadt <- bind_rows(geodata_1_aadt,
                            geodata_2_aadt,
                            geodata_3_aadt)

# Lager et Excelark med fire faner
# Må først lage en navgitt liste
# excelfaner <- list(fart = trafikkarbeid_nvdb,
#                    vegkategori = trafikkarbeid_veg_fylke,
#                    fylke = trafikkarbeid_fylke,
#                    land = trafikkarbeid_aar)

write_xlsx(geodata_x_aadt, path = "aadt_rapport_jan_2020.xlsx")
