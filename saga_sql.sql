-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

-- SQL flavour: Trino
-- https://trino.io/docs/current/language.html

select *
from
(
  select
      trafikkmelding_nokkel,
      hendelse_type,
      hendelse_notat note,
      hendelse_beskrivelse description,
      date(hendelse_start_tidspunkt) date_start,
      date(hendelse_slutt_tidspunkt) date_end,
      date_diff('minute', hendelse_start_tidspunkt, hendelse_slutt_tidspunkt) duration,
      hendelse_alle_kjoretoy_flagg all_vehicles
  from silver.s_hbt.trafikkmelding
  where date(hendelse_start_tidspunkt) between date('2025-02-01') and date('2025-03-01')
)
as events
where duration > 60*24;

