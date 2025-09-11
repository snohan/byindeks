# Traffic Data API

#mdt_test <- get_trp_mdt_with_coverage("91582V930281", "2020")
#mdt_test_2 <- get_trp_mdt_by_lane("91582V930281", "2020")

# MDT by length ----
# test_all_nortraf <- get_mdt_by_length_for_trp("43623V704583", 2014)
# test_mix_nortraf_new <- get_mdt_by_length_for_trp("43623V704583", 2015)
# test_all_new <- get_mdt_by_length_for_trp("43623V704583", 2016, "WEEKEND")
# test_none <- get_mdt_by_length_for_trp("16219V72812", 2022)
# test_single_day <- get_mdt_by_length_for_trp("01316V804837", 2025)
# trp_id <- "43623V704583"
# mdt_year <- 2015


# Labels ----
# Test
# mosvatn <- get_trp_labels("55507V319881")
# mosvatn_unnested <-
#   mosvatn |>
#   tidyr::unnest_wider(
#     affectedLanes
#   ) |>
#   dplyr::select(
#     trp_id = data.trafficRegistrationPoints.id,
#     label_start = validFrom,
#     label_end = validTo,
#     lane = lane.laneNumber
#   ) |>
#   dplyr::mutate(
#     dplyr::across(
#       .cols = c(label_start, label_end),
#       .fns = ~ floor_date(with_tz(ymd_hms(.x)), unit = "hour")
#     ),
#     date_interval = lubridate::interval(label_start, label_end)
#   ) |>
#   tidyr::unnest_longer(
#     lane,
#     keep_empty = TRUE
#   )


# TRP lists ----
#trp_list <- trp_distinct$trp_id[1:2]
#test_list <- trp_id = c("91582V930281", "01316V804837")
#test <- get_mdt_by_lane_for_trp_list(trp_list, "2020")
#test_adt <- getAdtForpoints_by_length(test_list)

# test_list <-
#   c(
#     "39215V320588", # kontinuerlig
#     "79404V1175648", # radar med lengde
#     "41078V805609", # radar uten lengde
#     "98963V1719019", # some early years without length data
#     "12567V1060674", # no ci
#     "63254V319822" # no data at all
#   )

# test_list <-
#   c(
#     "39215V320588", # kontinuerlig
#     "79404V1175648", # radar med lengde
#     "41078V805609" # radar uten lengde
#   )
#
# test_aadt <-
#   get_periodic_aadt_by_length_for_trp_list(test_list)


# AADT direction ----

#trp_id <- "12567V1060674"
#trp_id <- "63254V319822"

#test <- get_aadt_by_direction_and_length(trp_id)


# TRP index ----
#indexyear <- "2020"
#trp_ids <- "35258V2475662"
#trp_ids <- "47719V443837" # Vollsveien med alle lengdetall ekskludert
#trp_ids <- "17909V41450" # trp helt uten data


# Published index ----
# index_id <- 953
# indexyear <- 2023
# indexmonth <- 9

#indexmonth <- "10"
#indexyear <- "2020"
#index_id <- "960"



# HT DT ----
#trp_id <- "92719V1125906"
#from <- "2019-01-01T00:00:00.000+01:00"
#to <- "2019-02-01T00:00:00.000+01:00"

# trp_id <- "30868V1109333"
# from <- "2023-05-30T08:00:00.000+02:00"
# to <- "2023-05-30T09:00:00.000+02:00"

#from <- "2022-06-01T00:00:00+02:00"
#to <- "2022-06-03T00:00:00+02:00"

# trp_id <- "17681V704560"
# from_day <- "2019-01-01"
# to_day <- "2019-01-02"

## DT by length ----
trp_id <- "00000V1993681"
from <- "2018-07-01T00:00:00+01:00" # NorTraf, no coverage
to   <- "2018-07-06T00:00:00+01:00" # Coverage

trp_id <- "37235B802722" # No length data
trp_id <- "43623V704583" # With coverage
from <- "2022-01-01T00:00:00Z"
to   <- "2023-01-01T00:00:00Z"

trp_id <- "12021V805693" #
from <- "2020-01-02T00:00:00Z"
to   <- "2021-01-01T00:00:00Z"

test <- get_dt_by_length_for_trp(trp_id, from, to)

# Average HT and DT ----
#trp_id <- #"79743V1125914"
# "78481V42532"
#the_year <- "2019"
#day_type = "ALL"

# year = 2022
# week_no = 2
# day_type = "WEEKDAY"
# trp_id = "17681V704560"
