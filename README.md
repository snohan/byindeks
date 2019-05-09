# Byindeks
Informasjon om formidling av byindeks.

# Årsrapporter i PDF
Årsrapporter for byindeksen skal legges ut på vegvesen.no som PDF-dokumenter. Hvert avtaleområde får sin egen rapport, som har Statens vegvesens generelle forside som mal (https://www.vegvesen.no/intranett/Etat/Støttefunksjoner/Visuell+kommunikasjon/Grafiske+maler/Forsider/Generelle+forsider).

I koden blir dette en Rmd-fil per område, som genererer innholdet som MS Word. Alle funksjoner samles i ett felles skript.

# Årsrapportenes innhold
Egne årsrapporter lages for hvert avtaleområde. I tillegg lages en samlerapport med hovedtallene for alle avtaleområdene.

## Kart over punktene
Basert på punktene som inngår i avtalene, og som oppdateres manuelt i koden med liste over punkter (trp-id), hentes punktinfo fra trafikkdata-API.

Kart, om nødvendig på flere zoom-nivå, med fargeskala basert på indeksverdi og størrelse basert på ÅDT i basisår.

Bakgrunnskart som viser vegnettet tydelig: Google Roadmap.

## Liste over punkter
En enkel tabell med punktene som inneholder navn, vegreferanse og ÅDT i basisår og indeks.

## Grafisk visning av punktindeksenes årsverdier
En graf for å vise spredningen av punktenes årsindekser. Vises som et punktplott per år.

## Kommentarer til resultatene
En kortfattet tekst som omtaler:

- spredning i punktindeksene,
- spesielle trafikale forhold i området,
- annet relevant.

# Samlet oversikt over alle byindeksene
Et eget PDF-dokument som viser graf (liggende punkter med feilmargin) med navn, indeks og usikkerhet.

# HUSK
Endre tittel på innholdsfortegnelse til "Innhold" før omgjøring til PDF.

