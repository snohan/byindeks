#
# Beregne månedstrafikk fra bomtrafikkdata.
# 18. februar 2015.
# Snorre Hansen.
#
# ALL DEPRECATED - USE "bomdata_trondheim.R" instead

# Pakker ####
library(tidyverse)
library(reshape2)
library(readxl)

# Leser inn ####
# TODO: flytte alle bomdata til H:/Programmering/R/byindeks/bomdata_trondheim
# Angir mappen som rådatafilene ligger i:
Mappe <-
  "O:/landsdekkende/Prosjekt/Handlingsplan Trafikkdata/12 Organisering og kompetanse/09 Grunnlagsmateriale/Byindeks/Punkter/Trondheim/Bomstasjondata/"
setwd(Mappe)

# TODO: lese inn alle alltid? Dvs. 2016-01 og framover
bomaar <- "2019"
bommaaned <- "04"
#bomfila <- paste("Bomtrafikk_", bomaar, "-", bommaaned, ".csv", sep = "")
bomfila <- paste("bom", bomaar, bommaaned, ".xlsx", sep = "")

# 2013-2016 ####
# Leser inn de aktuelle kolonnene fra fila:
# Bomdata <- read.csv2(Bomfil_vasket, header = T,
#                      colClasses = c("NULL", rep("character", 5),
#                                     "NULL", "integer"))
                                    #, "NULL", "NULL"))

# ENTEN:
# Hvis rådata er på langform (csv):
# Bomdata <- read.csv(bomfila, header = FALSE,
#                     colClasses = c(rep("NULL",169), rep("character",2),
#                                    rep("NULL",2), rep("character",2),
#                                    "NULL", "character", rep("NULL",4),
#                                    "character", rep("NULL",24)),
#                     encoding = "UTF-8")

# ELLER:
# Hvis rådata er på kortform (Excel):
#Bomdata <- read.csv2(bomfila)

# 2017 ####
# ELLER Excelfil i 2017-:
Bomdata <- read_xlsx(bomfila)
Bomdata %<>% select(-Prosjekt)

colnames(Bomdata) <- c("Dato", "Time", "Bomstasjon", "Felt",
                       "Kjoretoyklasse", "Trafikkmengde")

# Tar bort 29. februar ####
#Bomdata %<>% filter(Dato != "29.02.2016")

#Bomdata$Trafikkmengde <- gsub("[[:space:]]", "",
#                              Bomdata$Trafikkmengde, fixed = F)
#str(Bomdata)
#Bomdata$Trafikkmengde <- as.integer(Bomdata$Trafikkmengde)

# Analyserer ####
# Fjerner rampene på Kroppan:
#Bomdata <- filter(Bomdata, Felt != "KROP-N-1" & Felt != "KROP-S-1")

# Forteller R hva som er formatet paa datokolonnen:
Bomdata$Dato <- strptime(Bomdata$Dato, "%d.%m.%Y")

# Legger til en kolonne som viser maaned, dag og time:
Bomdata["Maaned"] <- Bomdata$Dato$mon+1 # Januar definert som 0.
Bomdata["Dag"] <- Bomdata$Dato$mday
Bomdata["Timetall"] <- as.numeric(substr(Bomdata$Time, 0, 2))

# Endrer formatet paa datokolonnen, slik at filterfunksjonen i dplyr fungerer:
Bomdata$Dato <- as.POSIXct(Bomdata$Dato, "%d.%m.%Y", tz = "CET")

# Legger til en kolonne for felt-tid-ID:
Bomdata["Felt_tid_ID"] <- paste(Bomdata$Felt,
                                Bomdata$Maaned, Bomdata$Dag,
                                Bomdata$Timetall, sep = "-")

# Alle verdier på samme feltlinje:
Bomdata_en_feltlinje <- dcast(Bomdata,
                              Dato + Time + Bomstasjon + Felt +
                                Felt_tid_ID + Maaned + Dag + Timetall ~
                                Kjoretoyklasse,
                              value.var = "Trafikkmengde",
                              fun.aggregate = sum, na.rm = TRUE)

# Endrer kolonnenavn:
colnames(Bomdata_en_feltlinje)[9] <- "Liten_bil"
colnames(Bomdata_en_feltlinje)[10] <- "Stor_bil"

# Legger til en kolonne for sum alle kjoretoy:
Bomdata_en_feltlinje["Sum_kj"] <- Bomdata_en_feltlinje[,9] +
  Bomdata_en_feltlinje[,10] + Bomdata_en_feltlinje[,11]

# Skal summere trafikken for hver maaned ved hver bomstasjon:
Maanedstrafikk_slank <- select(Bomdata_en_feltlinje,
                               Bomstasjon, Felt, Maaned, Liten_bil, Stor_bil,
                               Ukjent, Sum_kj)
#unique(Maanedstrafikk_slank$Felt)
Maanedstrafikk_slank$Bomstasjon <- gsub(" (Nordgående)", "",
                                        Maanedstrafikk_slank$Bomstasjon,
                                        fixed = T)
Maanedstrafikk_slank$Bomstasjon <- gsub(" (Sørgående)", "",
                                        Maanedstrafikk_slank$Bomstasjon,
                                        fixed = T)

# Maanedstrafikk_slank$Bomstasjon <- gsub(" (Nordgaende)", "",
#                                         Maanedstrafikk_slank$Bomstasjon,
#                                         fixed = T)
# Maanedstrafikk_slank$Bomstasjon <- gsub(" (Sorgaende)", "",
#                                         Maanedstrafikk_slank$Bomstasjon,
#                                         fixed = T)

# Henter inn bomstasjonskodene:
Bomstasjonkoder <- read.csv2("Bomstasjonkoder.csv", header = FALSE)
colnames(Bomstasjonkoder) <- c("punktnr", "Feltnr", "Felt", "Navn")

# Slår sammen tabellene:
Maanedstrafikk_slank_koder <- merge(Maanedstrafikk_slank, Bomstasjonkoder,
                                    by = "Felt")

# Summerer opp månedstrafikken:
maanedstrafikk <- summarise(
  group_by(Maanedstrafikk_slank_koder, punktnr, Bomstasjon, Maaned),
                Liten_bil = sum(Liten_bil),
                Stor_bil = sum(Stor_bil),
                Ukjent = sum(Ukjent),
                Sum_kj = sum(Sum_kj))

# Skriver ut til fil:
write.csv2(maanedstrafikk, paste("Maanedstrafikk_", bomaar,
                                 "_", bommaaned, ".csv", sep = ""),
           row.names = F)
