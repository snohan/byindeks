# byindeks
Informasjon om byindeks.

# Bakgrunn
Våren 2019 ønskes det at informasjon om byindeksen legges ut på vegvesen.no som PDF-dokumenter. I koden blir dette en rmd-fil per område, men alle funksjoner samles i eget skript.

# Informasjon om hvert avtaleområde
All informasjon om et avtaleområde samles i en egen PDF-fil og oppdateres årlig.

## Kart over punktene
Basert på punktene som inngår i avtalene, og som oppdateres manuelt i koden med liste over punkter (trp-id), hentes punktinfo fra trafikkdata-API.

Kart, om nødvendig på flere zoom-nivå, med fargeskala basert på indeksverdi og størrelse basert på ÅDT i basisår.

Bakgrunnskart som viser vegnettet tydelig.

## Liste over punkter
En enkel liste med punktene som inneholder navn, vegreferanse og ÅDT i basisår.

## Grafisk visning av punktindeksenes årsverdier
En graf for å vise spredningen av punktenes årsindekser. Vises som et boksplott per år.

## Kommentarer til resultatene
En kortfattet tekst som omtaler:

- spredning i punktindeksene,
- spesielle trafikale forhold i området,
- annet relevant.

# Samlet oversikt over alle byindeksene
Et eget PDF-dokument som viser graf (liggende punkter med feilmargin) med navn, indeks og usikkerhet.

