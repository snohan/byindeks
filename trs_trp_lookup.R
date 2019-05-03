# Lookup table between trs and trp
# From trp-api
library(jsonlite)

# query getAllTrsAndTrp{
#   trafficRegistrationPoints(stationType: [CONTINUOUS], trafficType: [VEHICLE]) {
#     id
#     legacyNortrafMpn
#   }
# }

# Mapping trp to trs from trp-api
trs_trp <- jsonlite::fromJSON("trs_trp_lookup.json",
                              simplifyDataFrame = T, flatten = T) %>%
  as.data.frame() %>%
  dplyr::rename(trp_id = id)

# Adding trp_id to Old city index list
byindekspunkter <- read_csv2("byindekspunkter_vedtatte.csv",
                             locale = readr::locale(encoding = "latin1")) %>%
  dplyr::left_join(trs_trp)

write.csv2(byindekspunkter, file = "byindeks_trp.csv", row.names = F)


