#
# Henter inn månedstrafikk fra bomstasjonene, og beregner indeks.
# 23. mars 2017.
# Snorre Hansen
#

library(tidyverse)

# Funkjsoner ####
lesInnAllMaanedstrafikkForEtAar <- function(bomaaret) {
  # Leser inn alle filer for angitt år og setter de sammen.
  filnavn <- paste("Maanedstrafikk_", bomaaret, "_?", sep = "")
  filer <- list.files(pattern = filnavn)
  df <- do.call(rbind, lapply(filer, read.csv2))

  return(df)
}

# Leser inn ####
bombasisaar <- "2018"
bomindeksaar <- "2019"

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
byindeks.datainn <- read.csv2("pointindex-2019-04_2018.csv",
                              stringsAsFactors = F) %>%
#                              locale = locale(encoding = 'ISO-8859-1'))
  dplyr::select(2, 3, 11:17) %>%
  dplyr::filter(indeks != "-") %>%
  dplyr::mutate(indeks = as.numeric(stringr::str_replace(indeks, ",", ".")),
                periode = dplyr::case_when(periode == "Januar" ~ 1,
                                           periode == "Februar" ~ 2,
                                           periode == "Mars" ~ 3,
                                           periode == "April" ~ 4,
                                           periode == "Mai" ~ 5,
                                           periode == "Juni" ~ 6,
                                           periode == "Juli" ~ 7,
                                           periode == "August" ~ 8,
                                           periode == "September" ~ 9,
                                           periode == "Oktober" ~ 10,
                                           periode == "November" ~ 11,
                                           periode == "Desember" ~ 12))

colnames(byindeks.datainn) <- c("msnr", "msnavn", "dogn", "lengdeklasse",
                                "maaned", "indeks", "dekningsgrad",
                                "trafikkmengde.indeksaar",
                                "trafikkmengde.basisaar")

byindeks.datainn.filtrert <- byindeks.datainn %>%
  filter(dogn == "Alle" & lengdeklasse == "< 5,6m") %>%
  filter(!is.na(maaned)) %>%
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

# Punktindeks per måned ####
byindeks.trondheim <- rbind(byindeks.datainn.filtrert,
                            byindeks.bomdata.filtrert) %>%
  mutate(trafikkmengde.basisaar = as.numeric(trafikkmengde.basisaar),
         trafikkmengde.indeksaar = as.numeric(trafikkmengde.indeksaar)) %>%
  filter(!is.na(trafikkmengde.indeksaar))

# Ekskluderinger av bompunkter ####
byindeks.trondheim.etter.ekskluderinger <- byindeks.trondheim %>%
  dplyr::filter(!(msnr %in% c(9916052)))

write.csv2(byindeks.trondheim.etter.ekskluderinger,
           file = "punktindeks_trondheim_alle_punkter_jan-apr19.csv",
           row.names = F)

# Plotter ####
# TODO: size = adt_basisaar
index_plot <-
  ggplot2::ggplot() +
  geom_point(data = byindeks.trondheim.etter.ekskluderinger,
             aes(x = msnavn, y = indeks, size = trafikkmengde.basisaar),
             color = "#ED9300") +
  facet_grid(maaned ~ .) +
  geom_hline(yintercept = -4.1, color = "#58B02C") +
  xlab("Registreringspunkt") +
  ylab("Indeks (%)") +
  ggtitle(label = "Endring i trafikkmengde",
          subtitle = "Fra 2018 til 2019") +
  coord_flip() +
  scale_size(name = "Trafikkmengde basisår") +
  theme_minimal() +
  theme(legend.position = "bottom")
index_plot

# Hittil i år per punkt ####
byindeks.trondheim.punkt.aar <- byindeks.trondheim.etter.ekskluderinger %>%
  group_by(msnavn) %>%
  summarise(trafikkmengde_basisaar = sum(trafikkmengde.basisaar),
            trafikkmengde_indeksaar = sum(trafikkmengde.indeksaar),
            indeks = round((trafikkmengde_indeksaar/
                              trafikkmengde_basisaar - 1) * 100,
                           digits = 1))

  ggplot2::ggplot() +
  geom_point(data = byindeks.trondheim.punkt.aar,
             aes(x = msnavn, y = indeks, size = trafikkmengde_basisaar),
             color = "#ED9300") +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -4.5, ymax = 0.1),
             alpha = 0.1, fill = "#008EC2") +
  geom_hline(yintercept = -2.2, color = "#58B02C") +
  xlab("Registreringspunkt") +
  ylab("Indeks (%)") +
  ggtitle(label = "Endring i trafikkmengde",
          subtitle = "Fra 2018 til 2019") +
  coord_flip() +
  scale_size(name = "Trafikkmengde basisår") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Beregner årsindeks for Trondheim ####
byindeks.trondheim.aar <- byindeks.trondheim.etter.ekskluderinger %>%
  summarise(trafikkmengde.basisaar.sum = sum(trafikkmengde.basisaar,
                                             na.rm = T),
            trafikkmengde.indeksaar.sum = sum(trafikkmengde.indeksaar,
                                              na.rm = T),
            indeks = (trafikkmengde.indeksaar.sum/
                         trafikkmengde.basisaar.sum - 1) * 100)

# Parallell beregning for å få inn KFI
byindeks.trondheim.hittil.kfi <- byindeks.trondheim.etter.ekskluderinger %>%
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

write.csv2(byindeks.trondheim.hittil.kfi,
           file = "byindeks_trondheim_hittil_201904.csv",
           row.names = F)

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