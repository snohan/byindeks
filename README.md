# Byindeksrapporter
Dette prosjektet omhandler mal for og framstilling av rapporter for byindeksene.

# Publisering
Rapporter for byindeks legges ut på (vegvesen.no/fag/trafikk/indekser)[https://www.vegvesen.no/fag/trafikk/trafikkdata/indekser/byindeks] og oppdateres månedlig. 

Hvert avtaleområde får sin egen rapport i PDF, som har Statens vegvesens generelle forside som mal (https://www.vegvesen.no/intranett/Etat/Støttefunksjoner/Visuell+kommunikasjon/Grafiske+maler/Forsider/Generelle+forsider).

Egne rapporter lages for hvert avtaleområde, og i kode er dette en Rmd-fil kalt **byindeks_rapport_områdenavn_referanseår.Rmd**. Disse genererer innholdet som MS Word. Felles funksjoner samles i kildeskript. Datagrunnlaget er csv-filer fra indeksmodulen og metainformasjon om punktene fra Trafikkdata-API-et.

I tillegg lages en samlerapport med hovedtallene for alle avtaleområdene, også her med tall fra og med hver avtales basisår. Denne omtaler ikke punktindeksene.

## Kart over punktene
Hver srapport inneholder informasjon om punktene som er bestemt å inngå i grunnlaget for byindeks.

Hvilke punkter som inngår i avtalene oppdateres manuelt i **cities_points.csv**, og øvrig punktinfo hentes fra trafikkdata-API via **get_from_trafficdata_api.R**. Punktinfo per område prepareres i **indexpoint_tidying.R** og lagres som **indekspunkt_områdenavn_referanseår_beregningsår.csv**, klart til bruk i rapporten.

Kart, om nødvendig på flere zoom-nivå, med fargeskala basert på ÅDT i referanseår. Bakgrunnskart som viser vegnettet tydelig: "NVDB"-kartet.

## Tabell med punktene
Tabell med punktene som inneholder navn, vegreferanse og ÅDT i referanseår og indeksverdi hittil i år, samt hittil i perioden.

## Grafisk visning av punktindeksenes årsverdier
En graf for å vise spredningen av punktenes årsindekser.

## Kommentarer til resultatene
En kortfattet tekst som omtaler:

- spredning i punktindeksene,
- spesielle trafikale forhold i området (manuelle merkinger),
- annet relevant,

Lokal trafikkdataansvarlig må være med i prosessen!

# Samlet oversikt over alle byindeksene
Et eget PDF-dokument som viser graf (liggende punkter med feilmargin) med navn, indeks og usikkerhet.

# HUSK
Endre tittel på innholdsfortegnelse til "Innhold" før omgjøring til PDF.