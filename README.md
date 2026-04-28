# Innhold
Kode for trafikkindeks:

- kvalitetssjekke
- beregne
- rapportere
- vurdere representativitet og punktutvalg
- regneeksempler forbedret metode


# Byindeksproduksjon
Rekkefølge:

- city_index_check.qmd (kall hele city_index_check_dataprep.R en gang per produksjonsrunde)
- lag indeks i Adm
- city_index_dataprep.R: start alltid med denne, men byer med bomstasjoner har eget løp for årlig indeks: city_index_dataprep_trd.R, city_index_dataprep_hau.R
- city_index_report.qmd
