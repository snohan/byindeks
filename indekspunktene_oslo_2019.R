# Henter alle punkter
points <- getPoints()

# TODO: les inn hvilke som er utvalgt, må legges inn i trp-csv først
# merge med trs-trp?
# join points
oslopunkter <- read_csv2("byindeks_trp.csv") %>%
  dplyr::filter(city_area_name == "Oslo og Akershus",
                agreement_start == 2019) %>%
  dplyr::mutate(established = "Ja" )

# Adding metadata
indekspunkter_oslo <- dplyr::left_join(oslopunkter, points) %>%
  dplyr::select(1:5, 7:11, 6)

# Legger inn ikke-etablerte punkter
indekspunkter_oslo_kunstige <- read.csv2("indekspunkter_oslo_kunstige.csv") %>%
  dplyr::mutate(established = "Nei" )

indekspunktene_oslo <- bind_rows(indekspunkter_oslo,
                                 indekspunkter_oslo_kunstige)

write.csv2(indekspunktene_oslo, file = "indekspunktene_oslo.csv",
           row.names = F)


# ADT
oslo_adt <- getAdtForpoints(indekspunktene_oslo$trp_id)
# TODO: filtrere ut 2018
# TODO: join
# TODO: hente fra NVDB de som mangler?

# Test
#uten_adt <- getTrpAadt("32135V604101")
