# Henter alle punkter
# test
points <- getPoints()

# TODO: les inn hvilke som er utvalgt, må legges inn i trp-csv først
# merge med trs-trp?
# join points
oslopunkter <- read_csv2("byindeks_trp.csv") %>%
  dplyr::filter(city_area_name == "Oslo og Akershus",
                agreement_start == 2019) %>%
  dplyr::mutate(established = "Ja" )

indekspunkter_oslo <- dplyr::left_join(oslopunkter, points) %>%
  dplyr::select(1:5, 7:11, 6)

write.csv2(indekspunkter_oslo, file = "indekspunktene_oslo.csv",
           row.names = F)


# ADT
oslo_adt <- getAdtForpoints(indekspunktene_oslo$trp_id)
# TODO: filtrere ut 2018
# TODO: join
# TODO: hente fra NVDB de som mangler?

# TEst
#uten_adt <- getTrpAadt("32135V604101")
