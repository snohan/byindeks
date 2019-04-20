#
# Henter inn månedstrafikk fra bomstasjonene, og beregner indeks.
# 23. mars 2017.
# Snorre Hansen
#

library(readr)
library(stringr)

# Funkjsoner ####
lesInnAllMaanedstrafikkForEtAar <- function(bomaaret) {
  # Leser inn alle filer for angitt år og setter de sammen.
  filnavn <- paste("Maanedstrafikk_", bomaaret, "_?", sep = "")
  filer <- list.files(pattern = filnavn)
  df <- do.call(rbind, lapply(filer, read.csv2))

  return(df)
}

# Leser inn ####
bombasisaar <- "2017"
bomindeksaar <- "2018"

maanedstrafikk.bombasisaar <- lesInnAllMaanedstrafikkForEtAar(bombasisaar)
maanedstrafikk.bomindeksaar <- lesInnAllMaanedstrafikkForEtAar(bomindeksaar)

# maanedstrafikk.bomindeksaar.summert <- maanedstrafikk.bomindeksaar %>%
#   group_by(Maaned) %>%
#   summarise(ant = n())

# Matcher år og punkt:
maanedstrafikk.begge <- inner_join(maanedstrafikk.bombasisaar,
                                  select(maanedstrafikk.bomindeksaar,
                                         -Bomstasjon),
                                  by = c("punktnr" = "punktnr",
                                         "Maaned" = "Maaned"),
                                  suffix = c("_basisaar", "_indeksaar"))

maanedstrafikk.byindeks <- maanedstrafikk.begge %>%
  filter(!punktnr %in% c(9916070, 9916072, 9916073, 9916074, 9916080,
                         9916081, 9916082, 9916083, 9916063))

maanedstrafikk.byindeks.punkt <-
  mutate(maanedstrafikk.byindeks,
         punktnr = punktnr,
         basisaar = bombasisaar,
         indeksaar = bomindeksaar,
         maaned = Maaned,
         lengdeklasse = 21,
         trafikkmengde.basisaar = Liten_bil_basisaar,
         trafikkmengde.indeksaar = Liten_bil_indeksaar,
         indeks = (trafikkmengde.indeksaar/
                     trafikkmengde.basisaar - 1) * 100,
         dekningsgrad = 100.000)

# Slå sammen med ordinære punkter
# Leser inn punktindeks fra Datainn:
byindeks.datainn <- read.csv2("pointindex-trondheim-2018-12_2017.csv",
                              stringsAsFactors = F) %>%
#                              locale = locale(encoding = 'ISO-8859-1'))
  select(2, 3, 11:17) %>%
  filter(indeks != "-")
# TODO: numeriske verdier på indekstall og måneder som tall

colnames(byindeks.datainn) <- c("msnr", "msnavn", "dogn", "lengdeklasse",
                                "maaned", "indeks", "dekningsgrad",
                                "trafikkmengde.indeksaar",
                                "trafikkmengde.basisaar")

byindeks.datainn.filtrert <- byindeks.datainn %>%
  filter(dogn == "Alle" & lengdeklasse == "< 5,6m") %>%
  filter(!str_detect(maaned, "Hittil|Siste")) %>%
  select(msnr, msnavn, maaned,
         trafikkmengde.basisaar,
         trafikkmengde.indeksaar, indeks, dekningsgrad)

byindeks.bomdata.filtrert <- maanedstrafikk.byindeks.punkt %>%
  select(punktnr, Bomstasjon, Maaned,
         trafikkmengde.basisaar,
         trafikkmengde.indeksaar,
         indeks, dekningsgrad) %>%
  rename(msnr = punktnr,
         msnavn = Bomstasjon,
         maaned = Maaned)

byindeks.trondheim <- rbind(byindeks.datainn.filtrert,
                            byindeks.bomdata.filtrert) %>%
  mutate(trafikkmengde.basisaar = as.numeric(trafikkmengde.basisaar),
         trafikkmengde.indeksaar = as.numeric(trafikkmengde.indeksaar)) %>%
  filter(!is.na(trafikkmengde.indeksaar))

write.csv2(byindeks.trondheim,
           file = "punktindeks_trondheim_alle_punkter_jan-des18.csv",
           row.names = F)

# byindeks.trondheim <- read.csv2("punktindeks_trondheim_alle_punkter.csv",
#                                 stringsAsFactors = F) %>%
#   mutate(trafikkmengde.basisaar = as.numeric(trafikkmengde.basisaar),
#          trafikkmengde.indeksaar = as.numeric(trafikkmengde.indeksaar))

# Beregner årsindeks for Trondheim
byindeks.trondheim.aar <- byindeks.trondheim %>%
  summarise(trafikkmengde.basisaar.sum = sum(trafikkmengde.basisaar,
                                             na.rm = T),
            trafikkmengde.indeksaar.sum = sum(trafikkmengde.indeksaar,
                                              na.rm = T),
            indeks = (trafikkmengde.indeksaar.sum/
                         trafikkmengde.basisaar.sum - 1) * 100)

# Parallell beregning for å få inn KFI
byindeks.trondheim.hittil.kfi <- byindeks.trondheim %>%
  group_by(msnr) %>%
  summarise(trafikkmengde.basisaar.sum = sum(trafikkmengde.basisaar,
                                             na.rm = T),
            trafikkmengde.indeksaar.sum = sum(trafikkmengde.indeksaar,
                                              na.rm = T),
            indeks = (trafikkmengde.indeksaar.sum/
                        trafikkmengde.basisaar.sum - 1) * 100) %>%
  summarise(ant_punkter = n(),
            sum.trafikkmengde_basisaar = sum(trafikkmengde.basisaar.sum),
            sum.trafikkmengde_indeksaar = sum(trafikkmengde.indeksaar.sum),
            indeksen = round(
              (sum.trafikkmengde_indeksaar / sum.trafikkmengde_basisaar - 1)
              * 100, digits = 3),
            vekt = 1 / (1 - sum(
              (trafikkmengde.basisaar.sum /
                 sum.trafikkmengde_basisaar) ^ 2)
            ),
            std = round(
              sqrt(vekt * sum(
                (trafikkmengde.basisaar.sum /
                   sum.trafikkmengde_basisaar) *
                  (indeks - indeksen) ^ 2)
              ),
              digits = 3),
            kfi = round(qt(0.975, ant_punkter) * std /
                          sqrt(ant_punkter), digits = 3)) %>%
  select(-vekt) %>%
  rename(trafikkmengde_basisaar = sum.trafikkmengde_basisaar,
         trafikkmengde_indeksaar = sum.trafikkmengde_indeksaar,
         indeks = indeksen)

# Tilpasning til PG (gammel versjon) ####


# maanedstrafikk.bom.pg <- mutate(maanedstrafikk.begge,
#                                 punktnr = punktnr,
#                                 basisaar = bombasisaar,
#                                 indeksaar = bomindeksaar,
#                                 maaned = Maaned,
#                                 lengdeklasse = 20,
#                                 trafikkmengde.basisaar = Sum_kj_basisaar,
#                                 trafikkmengde.indeksaar = Sum_kj_indeksaar,
#                                 indeks = (trafikkmengde.indeksaar/
#                                           trafikkmengde.basisaar - 1) * 100,
#                                 dekningsgrad = 100.000)

maanedsdager <- data.frame("maaned" = c(seq(1, 12, 1)),
                           "dager" = c(31, 28 , 31, 30, 31, 30,
                                       31, 31, 30 , 31, 30, 31))

maanedstrafikk.bom.pg <- left_join(maanedstrafikk.bom.pg, maanedsdager,
                                   by = c("Maaned" = "maaned"))

maanedstrafikk.bom.pg %<>% select(punktnr, basisaar:lengdeklasse, dager,
                                  trafikkmengde.basisaar:dekningsgrad)

# Finner en samlet indeks for bomstasjonene.
aarsindeks.bom <- (sum(maanedstrafikk.bom.pg$trafikkmengde.indeksaar)/
                     sum(maanedstrafikk.bom.pg$trafikkmengde.basisaar) - 1)*100

# Skriver til Postgresql-databasen ####
dbWriteTable(con, "punktindeks", value = maanedstrafikk.bom.pg,
             append = TRUE, row.names = FALSE)

#
# Slutt.
#