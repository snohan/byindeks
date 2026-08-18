tolling_data_daily_lane <-
  tolling_data_daily_lane_raw |> 
  # Check dates by weekday in each month, the plot doesn't show day of month on x axis!
  dplyr::filter(!(trp_id == "104" & date %in% ymd(c("2019-08-10", "2019-08-11", "2019-08-12", "2019-11-09")))) |> 
  dplyr::filter(!(trp_id == "106" & date %in% ymd(c("2019-03-09", "2019-03-10", "2019-11-09")))) |> 
  dplyr::filter(!(trp_id == "106" & date %in% seq.Date(as.Date("2022-03-21"), as.Date("2022-03-24"), 1))) |> 
  dplyr::filter(!(trp_id == "106" & date %in% ymd(c("2023-05-14", "2023-05-15", "2023-05-16")))) |> 
  dplyr::filter(!(trp_id == "107" & date %in% ymd(c("2018-11-06", "2018-11-07")))) |> 
  dplyr::filter(!(trp_id == "107" & date %in% ymd(c("2019-02-19", "2019-02-20", "2019-02-21", "2022-12-12", "2025-06-01", "2026-06-28")))) |> 
  dplyr::filter(!(trp_id == "108" & date %in% ymd(c("2020-03-04", "2026-05-01", "2026-05-02")))) |> 
  dplyr::filter(!(trp_id == "110" & date %in% seq.Date(as.Date("2021-11-08"), as.Date("2021-11-11"), 1))) |> 
  dplyr::filter(!(trp_id == "112" & date %in% seq.Date(as.Date("2018-10-01"), as.Date("2018-10-10"), 1))) |> 
  dplyr::filter(!(trp_id == "112" & date %in% ymd(c("2025-06-01")))) |> 
  dplyr::filter(!(trp_id == "113" & date %in% seq.Date(as.Date("2018-10-01"), as.Date("2018-10-16"), 1))) |> 
  dplyr::filter(!(trp_id == "113" & date %in% ymd(c("2019-11-09", "2022-03-15")))) |> 
  dplyr::filter(!(trp_id == "113" & date %in% seq.Date(as.Date("2021-06-07"), as.Date("2021-06-14"), 1))) |> 
  dplyr::filter(!(trp_id == "113" & date %in% seq.Date(as.Date("2022-03-20"), as.Date("2022-03-24"), 1))) |> 
  dplyr::filter(!(trp_id == "114" & date %in% seq.Date(as.Date("2020-10-05"), as.Date("2020-10-08"), 1))) |> 
  dplyr::filter(!(trp_id == "114" & date %in% ymd(c("2023-10-27")))) |> 
  dplyr::filter(!(trp_id == "201" & date %in% seq.Date(as.Date("2019-01-07"), as.Date("2019-01-14"), 1))) |> 
  dplyr::filter(!(trp_id == "201" & date %in% ymd(c("2020-09-12", "2023-09-18", "2023-09-19")))) |> 
  dplyr::filter(!(trp_id == "201" & date %in% seq.Date(as.Date("2024-09-02"), as.Date("2024-09-09"), 1))) |> 
  dplyr::filter(!(trp_id == "203" & date %in% seq.Date(as.Date("2018-10-01"), as.Date("2018-10-04"), 1))) |> 
  dplyr::filter(!(trp_id == "203" & date %in% ymd(c("2018-10-15", "2018-10-16", "2018-10-17")))) |> 
  dplyr::filter(!(trp_id == "204" & date %in% ymd(c("2018-11-27", "2018-11-28")))) |> 
  dplyr::filter(!(trp_id == "206" & date %in% ymd(c("2024-09-09", "2024-09-14")))) |> 
  dplyr::filter(!(trp_id == "207" & date %in% ymd(c("2024-06-22", "2024-06-23", "2024-09-09")))) |> 
  dplyr::filter(!(trp_id == "208" & date %in% ymd(c("2018-11-23", "2023-06-21")))) |> 
  dplyr::filter(!(trp_id == "209" & date %in% ymd(c("2018-11-23")))) |> 
  dplyr::filter(!(trp_id == "210" & date %in% ymd(c("2019-08-19", "2020-10-08")))) |> 
  dplyr::filter(!(trp_id == "302" & date %in% ymd(c("2019-03-05")))) |> 
  dplyr::filter(!(trp_id == "303" & date %in% ymd(c("2018-11-17", "2018-11-18", "2019-04-04", "2019-04-05", "2021-11-19", "2021-11-20", "2021-11-21")))) |> 
  dplyr::filter(!(trp_id == "305" & date %in% seq.Date(as.Date("2018-10-01"), as.Date("2018-10-10"), 1))) |> 
  dplyr::filter(!(trp_id == "306" & date %in% ymd(c("2018-12-01", "2018-12-02", "2018-12-03")))) |> 
  dplyr::filter(!(trp_id == "308" & date %in% seq.Date(as.Date("2018-10-01"), as.Date("2018-10-18"), 1))) |> 
  dplyr::filter(!(trp_id == "308" & date %in% ymd(c("2018-10-25")))) |> 
  dplyr::filter(!(trp_id == "308" & date %in% seq.Date(as.Date("2025-02-28"), as.Date("2025-05-31"), 1))) |> 
  dplyr::filter(!(trp_id == "309" & date %in% ymd(c("2018-12-01", "2018-12-02", "2018-12-03", "2021-09-13")))) |> 
  dplyr::filter(!(trp_id == "310" & date %in% ymd(c("2019-06-15", "2021-09-11", "2026-06-13")))) |> 
  dplyr::filter(!(trp_id == "401" & date %in% ymd(c("2018-12-03")))) |> 
  dplyr::filter(!(trp_id == "402" & date %in% seq.Date(as.Date("2019-07-06"), as.Date("2019-07-10"), 1))) |> 
  dplyr::filter(!(trp_id == "402" & date %in% seq.Date(as.Date("2020-01-04"), as.Date("2020-01-07"), 1))) |> 
  dplyr::filter(!(trp_id == "402" & date %in% ymd(c("2019-07-16", "2020-03-02", "2020-03-10")))) |> 
  dplyr::filter(!(trp_id == "402" & date %in% ymd(c("2020-06-19", "2020-06-20", "2020-06-21", "2020-07-08", "2020-08-01", "2020-08-02", "2020-08-03", "2020-10-03")))) |> 
  dplyr::filter(!(trp_id == "403" & date %in% ymd(c("2019-06-15", "2019-06-16")))) |> 
  dplyr::filter(!(trp_id == "501" & date %in% ymd(c("2018-11-30")))) |> 
  dplyr::filter(!(trp_id == "502" & date %in% ymd(c("2018-12-08", "2018-12-09")))) |> 
  dplyr::filter(!(trp_id == "603" & date %in% ymd(c("2018-11-12")))) |> 
  dplyr::filter(!(trp_id == "603" & date %in% seq.Date(as.Date("2018-11-30"), as.Date("2018-12-05"), 1))) |> 
  dplyr::filter(!(trp_id == "604" & date %in% ymd(c("2019-06-20"))))
  # dplyr::filter(!(trp_id == "54" & month == "2021-07-01")) |>
  # dplyr::filter(!(trp_id == "54" & year == 2022)) |>
