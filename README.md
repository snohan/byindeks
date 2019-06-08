# Byindeks
Mal for formidling av byindeks.

# Årsrapporter i PDF
Årsrapporter for byindeks skal legges ut på vegvesen.no som PDF-dokumenter. Hvert avtaleområde får sin egen rapport, som har Statens vegvesens generelle forside som mal (https://www.vegvesen.no/intranett/Etat/Støttefunksjoner/Visuell+kommunikasjon/Grafiske+maler/Forsider/Generelle+forsider).

I kode er dette en Rmd-fil som genererer innholdet som MS Word. Felles funksjoner samles i et kildeskript. Datagrunnlaget er csv-filer fra indeksmodulen.

# Årsrapportenes innhold
Egne årsrapporter lages for hvert avtaleområde. Disse tar med indekstall fra og med basisåret.

I tillegg lages en samlerapport med hovedtallene for alle avtaleområdene, også her med tall fra og med hver avtales basisår. Denne omtaler ikke punktindeksene.

## Kart over punktene
Hver årsrapport inneholder informasjon om punktene som er bestemt å inngå i grunnlaget for byindeks, selv om de eventuelt ikke har bidratt med data i perioden.

Hvilke punktene som inngår i avtalene må oppdateres manuelt i koden med liste over punkter (trp-id), og øvrig punktinfo hentes fra trafikkdata-API.

Kart, om nødvendig på flere zoom-nivå, med fargeskala basert på indeksverdi og størrelse basert på ÅDT i basisår. Bakgrunnskart som viser vegnettet tydelig: "NVDB"-kartet.

## Liste over punkter
En enkel tabell med punktene som inneholder navn, vegreferanse og ÅDT i basisår og indeksverdi hittil i år, samt hittil i perioden.

## Grafisk visning av punktindeksenes årsverdier
En graf for å vise spredningen av punktenes årsindekser. Vises som en punktgraf per år.

## Kommentarer til resultatene
En kortfattet tekst som omtaler:

- spredning i punktindeksene,
- spesielle trafikale forhold i området (manuelle merkinger),
- annet relevant,
- lokal trafikkdataansvarlig må være med i prosessen.

# Samlet oversikt over alle byindeksene
Et eget PDF-dokument som viser graf (liggende punkter med feilmargin) med navn, indeks og usikkerhet.

# HUSK
Endre tittel på innholdsfortegnelse til "Innhold" før omgjøring til PDF.

