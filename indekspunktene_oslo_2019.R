# Henter alle punkter
# test
points <- getPoints()

# TODO: les inn hvilke som er utvalgt, må legges inn i trp-csv først
# merge med trs-trp?
# join points
oslopunkter <- read_csv2("byindeks_trp.csv") %>%
  dplyr::filter(city_area_name == "Oslo og Akershus",
                agreement_start == 2019)

indekspunktene_oslo <- dplyr::left_join(oslopunkter, points)
