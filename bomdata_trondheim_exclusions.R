# Excluding data from toll stations

# Exclusions may be monthly or daily

# Tungasletta høy andel ukjente juli og aug 2018, ukjentandel er over 30 %!
# 54 2021-08
# 85 and 86: 2021-01, 2021-03--2021-04 (high ratio of unknowns intermittently)
# Keep Nord for Sluppen bru (55) and not Bjørndalen (54) when Oslovegen is closed.
# - Bjørndalen is out of service.

# 56 siste halvdel av 2022: ombygging av veg (kulverter) med innsnevring har nok ført til flytting
# av bomstasjoner og mulig redusert antall felt

# 55 flyttet fra Nord for Sluppen bru til Oslovegen nord for Nydalsbrua med oppstart medio august 2023
# Skal derfor behandles som nytt punkt etter det. Men ligger nå på samme lenke som TRP Oslovegen.

tolling_data_daily_tidy <-
  tolling_data_daily |>
  dplyr::filter(!(trp_id == "51" & date %in% ymd(c("2025-10-04")))) |>
  dplyr::filter(!(trp_id == "52" & date %in% ymd(c("2022-05-12", "2022-05-13", "2022-05-20")))) |>
  dplyr::filter(!(trp_id == "52" & date %in% ymd(c("2023-01-18", "2023-01-19", "2023-04-24", "2023-04-25", "2023-04-26")))) |>
  dplyr::filter(!(trp_id == "52" & date %in% ymd(c("2025-10-04")))) |>
  dplyr::filter(!(trp_id == "54" & month == "2021-03-01")) |>
  dplyr::filter(!(trp_id == "54" & month == "2021-04-01")) |>
  dplyr::filter(!(trp_id == "54" & month == "2021-05-01")) |>
  dplyr::filter(!(trp_id == "54" & month == "2021-06-01")) |>
  dplyr::filter(!(trp_id == "54" & month == "2021-07-01")) |>
  dplyr::filter(!(trp_id == "54" & date %in% seq.Date(as.Date("2021-08-01"), as.Date("2021-08-15"), 1))) |>
  dplyr::filter(!(trp_id == "54" & year == 2022)) |>
  dplyr::filter(!(trp_id == "54" & month == "2024-10-01")) |>
  dplyr::filter(!(trp_id == "54" & month == "2024-11-01")) |>
  dplyr::filter(!(trp_id == "54" & month == "2024-12-01")) |>
  dplyr::filter(!(trp_id == "54" & date %in% seq.Date(as.Date("2025-02-26"), as.Date("2025-03-09"), 1))) |>
  dplyr::filter(!(trp_id == "55" & month == "2021-05-01")) |>
  dplyr::filter(!(trp_id == "55" & month == "2021-06-01")) |>
  dplyr::filter(!(trp_id == "55" & month == "2021-07-01")) |>
  dplyr::filter(!(trp_id == "55" & date %in% seq.Date(as.Date("2021-08-01"), as.Date("2021-08-15"), 1))) |>
  dplyr::filter(!(trp_id == "55" & date %in% ymd(c("2023-06-26")))) |>
  dplyr::filter(!(trp_id == "55" & month %in% seq.Date(as.Date("2023-07-01"), as.Date("2030-12-01"), by = "month"))) |>
  dplyr::filter(!(trp_id == "55" & date %in% seq.Date(as.Date("2025-02-26"), as.Date("2025-03-09"), 1))) |>
  dplyr::filter(!(trp_id == "56" & month == "2021-04-01")) |>
  dplyr::filter(!(trp_id == "59" & date %in% seq.Date(as.Date("2023-07-27"), as.Date("2023-07-28"), 1))) |>
  dplyr::filter(!(trp_id == "59" & date %in% ymd(c("2025-08-17", "2025-10-26")))) |>
  dplyr::filter(!(trp_id == "60" & date %in% ymd(c("2025-10-26")))) |>
  dplyr::filter(!(trp_id == "61" & date %in% ymd(c("2023-06-20")))) |>
  dplyr::filter(!(trp_id == "62" & date %in% ymd(c("2023-04-12", "2023-04-13")))) |>
  dplyr::filter(!(trp_id == "63")) |>
  dplyr::filter(!(trp_id == "64" & date %in% ymd(c("2023-06-23")))) |>
  dplyr::filter(!(trp_id == "67" & date %in% ymd(c("2023-06-27", "2023-06-28")))) |>
  dplyr::filter(!(trp_id == "67" & date %in% seq.Date(as.Date("2025-02-24"), as.Date("2025-03-09"), 1))) |>
  dplyr::filter(!(trp_id == "67" & date %in% seq.Date(as.Date("2025-05-12"), as.Date("2025-10-31"), 1))) |>
  dplyr::filter(!(trp_id == "68" & date %in% seq.Date(as.Date("2025-02-24"), as.Date("2025-03-09"), 1))) |>
  dplyr::filter(!(trp_id == "69" & date %in% ymd(c("2023-07-04", "2023-07-05")))) |>
  dplyr::filter(!(trp_id == "72" & month == "2021-04-01")) |>
  dplyr::filter(!(trp_id == "72" & date %in% ymd(c("2025-11-09", "2025-11-10", "2025-11-15", "2025-11-16", "2025-11-17")))) |>
  dplyr::filter(!(trp_id == "85" & date %in% ymd(c("2021-01-11", "2021-01-12", "2021-01-13", "2025-10-04")))) |>
  dplyr::filter(!(trp_id == "86" & month == "2021-01-01")) |>
  dplyr::filter(!(trp_id == "86" & month == "2021-03-01")) |>
  dplyr::filter(!(trp_id == "86" & month == "2021-04-01"))
