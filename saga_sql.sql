-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

-- SQL flavour: Trino
-- https://trino.io/docs/current/language.html

-- Get data from s_hbt

-- 1. Create schema
create schema datalabs.d_traind_schema1
with (location = 's3a://sagadpl-prod-analyse-traind/d_traind_scehma1');


-- 2. Create views
create or replace view datalabs.d_traind_schema1.events
    security invoker
    as
select distinct
    t_id,
    date_start, date_end, duration_days,
    --hendelse_type,
    note, description, all_vehicles,
    --fylke_nummer,
    geografi, lokasjon_beskrivelse, vei_nummer
    from
    (select * from
        (select
            trafikkmelding_nokkel,
            trafikkmelding_nokkel t_id,
            --hendelse_type,
            -- there are multiple rows per same actual event, only differing on hendelse_type
            hendelse_notat note,
            hendelse_beskrivelse description,
            date(hendelse_start_tidspunkt) date_start,
            date(hendelse_slutt_tidspunkt) date_end,
            date_diff('day', hendelse_start_tidspunkt, hendelse_slutt_tidspunkt) duration_days,
            hendelse_alle_kjoretoy_flagg all_vehicles
        from silver.s_hbt.trafikkmelding
        --where date (hendelse_start_tidspunkt) between date ('2025-05-01') and date ('2025-05-01')
        )
    -- aliases not allowed in where clause
    as events
    join silver.s_hbt.trafikkmelding_lokasjon tl
    on events.trafikkmelding_nokkel = tl.trafikkmelding_nokkel
    )
as events_tl
join silver.s_hbt.lokasjon locations
on events_tl.lokasjon_nokkel = locations.lokasjon_nokkel
-- some locations have no geometry
-- some locations have both no geometry and geometry, resulting in duplicates!
-- but the missing ones are redundant!
-- some locations are polygons and should be removed
where duration_days >= 1 and geografi not like '%POLYGON%' and geografi is not null;

create or replace view datalabs.d_traind_schema1.events_wkb
    security invoker
    as
       select
           *,
           concat(date_format(date_start, '%d-%m-%Y'), '-', date_format(date_end, '%d-%m-%Y')) as periode,
           ST_AsBinary(ST_GeometryFromText(geografi)) as geometry,
           ST_X(ST_Centroid(ST_Envelope(ST_GeometryFromText(geografi)))) as xcenter,
           ST_XMax(ST_GeometryFromText(geografi)) as xmax,
           ST_XMin(ST_GeometryFromText(geografi)) as xmin,
           ST_Y(ST_Centroid(ST_Envelope(ST_GeometryFromText(geografi)))) as ycenter,
           ST_YMax(ST_GeometryFromText(geografi)) as ymax,
           ST_YMin(ST_GeometryFromText(geografi)) as ymin
        from datalabs.d_traind_schema1.events;


-- 3. Make the actual table with events
create table datalabs.d_traind_schema1.events_full_4 as
select * from datalabs.d_traind_schema1.events_wkb
-- not aksellastrestriksjoner
where note not like '%ksellast%' and
-- nightly work
note not like '%mellom 2_:%';

-- do not need the extra geo columns, so will not use the _wkb view
-- HERE EACH MONTH: MAKE NEW TABLE WITH MOST RECENT DATA
create table datalabs.d_traind_schema1.events_20260504 as
select * from datalabs.d_traind_schema1.events
-- not aksellastrestriksjoner
where note not like '%ksellast%' and
-- nightly work
note not like '%mellom 2_:%';


-- 4. Look at data quality
select count(*) from datalabs.d_traind_schema1.events_full_4;

select min(date_start) from datalabs.d_traind_schema1.events_full;
-- 2022-04-24
select max(date_start) from datalabs.d_traind_schema1.events_full;
-- 2025-12-26 as per 2025-12-01

-- mobile work, often not restricting flow considerably?
select * from datalabs.d_traind_schema1.events_full_2
where note like '%evegelig vegarbeid%';

-- is a closed road always mentioned by stengt?
select * from datalabs.d_traind_schema1.events_full_2
where note not like '%tengt%';

-- duplicates, turned out to be separate geometries and ok
-- should preferably have been merged to one geometry
create table datalabs.d_traind_schema1.duplicates as
select t_id, count(t_id) n
from datalabs.d_traind_schema1.events_full
group by t_id
order by n desc;

--create table datalabs.d_traind_schema1.duplicate_example as
select * from datalabs.d_traind_schema1.events_full
where t_id = '76924fe4e91fc43141588ab56e753e9b';

--create table datalabs.d_traind_schema1.pull as
select * from datalabs.d_traind_schema1.events_full
where t_id like '974f2%';

select * from datalabs.d_traind_schema1.events_full
where geografi is null;

select distinct
    description
from datalabs.d_traind_schema1.events_full
where description is not null;

-- Missing geometry
create table datalabs.d_traind_schema1.missing_geometry as
select distinct t_id from datalabs.d_traind_schema1.events_full
where geografi is null;

create table datalabs.d_traind_schema1.having_geometry as
select distinct t_id from datalabs.d_traind_schema1.events_full
where geografi is null;

select count(*) from datalabs.d_traind_schema1.missing_geometry;

-- Is there any t_id in missing_geometry that does not show up in having_geometry?
select * from datalabs.d_traind_schema1.missing_geometry mis
where mis.t_id not in (select hav.t_id from datalabs.d_traind_schema1.having_geometry hav);
-- NOPE! :)


--create table datalabs.d_traind_schema1.pull_trafikkmelding as
select * from silver.s_hbt.trafikkmelding
where trafikkmelding_nokkel like '1dd19a601962322ca527ff3521b5c75d%';

--where date_start between date('2025-06-01') and date('2025-06-10') or
--     date_end between date('2025-06-01') and date('2025-06-10');


-- 5. Clean up
drop table datalabs.d_traind_schema1.events_20260102;
drop view datalabs.d_traind_schema1.events_bare;

