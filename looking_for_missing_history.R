# Find TRS with missing device type history

trs_history <- get_trs_history()

trs_history_with_commission <- trs_history %>%
  dplyr::filter(!is.na(commission_valid_from)) %>%
  dplyr::filter(is.na(device_valid_from))
