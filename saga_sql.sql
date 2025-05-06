-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

select
    trafikkmelding_nokkel,
    hendelse_type,
    hendelse_notat note,
    hendelse_beskrivelse description,
    date(hendelse_start_tidspunkt) date_start,
    date(hendelse_slutt_tidspunkt) date_end,
    hendelse_alle_kjoretoy_flagg all_vehicles
from "trafikkmelding"
where date(hendelse_start_tidspunkt) between date('2025-02-01') and date('2025-03-01')
limit 15;