# Packages ----
library(tidyverse)
library(httr2) # https://httr2.r-lib.org/


# Basics ----
# API spec:
# https://trafikkdata-api-v2.atlas.vegvesen.no/swagger-ui/index.html

basic_request <- 
  httr2::request("https://trafikkdata-api-v2.atlas.vegvesen.no/") |> 
  httr2::req_headers(
    "accept" = "application/json",
    "x-client" = "snorre.hansen@vegvesen.no"
  )


trafikkdata_api_paginate <- function(req) {

  responses_all_pages <- 
    httr2::req_perform_iterative(
      req,
      next_req = httr2::iterate_with_cursor(
        "start",
        resp_param_value = function(resp) {
          
          body <- httr2::resp_body_json(resp)

          if(!body$metadata$harFlereResultater) {
            return(NULL)
          }

          body$metadata$neste$start

        }
      ),
      max_reqs = Inf
    )

}

parse_resultater <- function(resultater) {

  if (is.null(resultater) || length(resultater) == 0) {
    return(tibble::tibble())
  }

  resultater |>
    tidyr::unnest(dagtyper) |>
    jsonlite::flatten()
}


trafikkdata_api_bind_pages <- function(resp) {
  
  resp |>
    purrr::map(
      ~ httr2::resp_body_json(
        .x,
        simplifyVector = TRUE
      )$resultater |> 
        parse_resultater()
    ) |> 
    purrr::list_rbind()

}

tidy_xdt <- function(data) {

  if (nrow(data) == 0) {
    return(data)
  }

  data |>
    tidyr::unnest(dplyr::where(base::is.list)) |> 
    dplyr::select(
    -periodeTil, 
    -tidyselect::starts_with("beregningsmetode"), 
    -kilde, 
    -tidyselect::contains("beskrivelse"), 
    -tidyselect::contains("konfidens"),
    -tidyselect::contains("inkluderte"), 
    -tidyselect::contains("ekskluderte"), 
    -tidyselect::contains("utilgjengelige"), 
    -tidyselect::contains("antallTotalt"), 
    -tidyselect::contains("enhet"), 
    -tidyselect::contains("Grense")
  ) |> 
  dplyr::rename(
    klasse = tidyselect::contains("klasse"),
    dekningsgrad_prosent = tidyselect::contains("dekningsgrad.prosent")
  ) 
}


# Queries ----
trafikkdata_api_get_xdt <- function(
  xdt = "kmdt",
  trp_id, 
  year_from, year_to, 
  day_type = NULL, 
  classification = NULL,
  road_division = NULL, 
  include = NULL
  ) {

  # Query parameters:
  # xdt: choose aggregate level: sdt, mdt, kmdt, aadt
  # fra: year as int
  # til: year as int
  # dagtype: ALLEDOGN (default), HELGEDOGN, YRKEDOGN
  # klassifisering: ALLE_KLASSIFISERINGER, INGEN_KLASSIFISERING (default), 
  #   LENGDEKLASSE_ENKEL, LENGDEKLASSE_DETALJERT, NORSIKTKLASSE_NIVAA2, NORSIKTKLASSE_NIVAA3, NORSIKTKLASSE_NIVAA4
  # oppdeling: ALLE_OPPDELINGER, TVERRSNITT (default), RETNING, KJOREFELT
  # inkluder: RETNINGSDETALJER

  # NB! The API can return a nested response with several classifications, but the parsing will be cumbersome.

  xdt_level <-
    dplyr::case_when(
      xdt == "mdt" ~ "maanedsdogntrafikk",
      xdt == "kmdt" ~ "kalenderjustertmaanedsdogntrafikk",
      xdt == "sdt" ~ "sesongdogntrafikk",
      xdt == "aadt" ~ "aarsdogntrafikk"
    )

  req <- 
    basic_request |> 
    httr2::req_url_path_append(
      "v2",
      "beta",
      "trafikkmengde",
      "trafikkregistreringspunkter",
      trp_id,
      xdt_level
    ) |> 
    httr2::req_url_query(
      fra = year_from,
      til = year_to,
      dagtype = day_type,
      klassifisering = classification,
      oppdeling = road_division,
      inkluder = include,
      .multi = "explode"
    )
  
data <- 
  trafikkdata_api_paginate(req) |> 
  trafikkdata_api_bind_pages() |> 
  tidy_xdt() |> 
  dplyr::mutate(trp_id)

}

