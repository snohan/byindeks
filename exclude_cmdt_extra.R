# Extra exclusions ----
# Make one table for each index period.
# Must contain trp_ids, universal_year_period_ids and a short textual explanation.
# Serves two purposes:
# 1. Filtering mdts before index calculations.
# 2. Human readable table.

table_exclusions <- function(text_chr, trp_chr, uyp_start = NA_integer_, uyp_end = NA_integer_) {

  # Period ids do not have to be specified. 
  # If not, the exclusion period will be set to NA, meaning it spans the whole or rest of indexperiod.

  et <-
    tidyr::crossing(
      text = text_chr,
      trp_id = trp_chr      
    ) |> 
    dplyr::mutate(
      uyp_start = uyp_start,
      uyp_end = uyp_end
    ) 

}

exclude_periods <- function(raw_df, et_df) {

  clean_df <-
    raw_df |> 
  # Remove alltimers
  dplyr::anti_join(
      et_df |> 
        dplyr::filter(is.na(uyp_start) & is.na(uyp_end)),
      by = c("trp_id")
  ) |> 
  # Remove unended
  dplyr::anti_join(
      et_df |> 
        dplyr::filter(!is.na(uyp_start) & is.na(uyp_end)),
      by = dplyr::join_by(trp_id, universal_year_period_id >= uyp_start)
  ) |> 
  # Remove closed
  dplyr::anti_join(
      et_df |> 
        dplyr::filter(!is.na(uyp_start) & !is.na(uyp_end)),
      by = dplyr::join_by(trp_id, universal_year_period_id >= uyp_start, universal_year_period_id <= uyp_end)
  )

  return(clean_df)
}


# All events that shall lead to exclusions must be given below with their start and stop uyp_id,
# and this will subsequently be filtered out from the data and not be part of the index calculations.

## Nord-Jæren example with toll stations ----
# Chain part 1: okt17_sep18__okt18_sep19
# universal_year_period_ids: 
exclusions_nj_okt17_sep18__nj_okt18_sep19 <-
  dplyr::bind_rows(
    table_exclusions("Ny arm mellom fv. og E39 ved Hove", c("43296V319721", "88125V320152")),
  )


# Chain part 2: okt18_sep19__2023
# universal_year_period_ids: 40-112
exclusions_nj_okt18_sep19__2023 <-
  dplyr::bind_rows(
    table_exclusions(
      "Åpning av Eiganestunnelen", 
      c("906727263", "906727262", "22231V320583", "906727257", "45342V320223", "50749V319525", "55507V319881", "71535V319524", "10795V320297",
        "32842V319521", "58562V320296"), 
      61
    ),
    table_exclusions(
      "Åpning av Ryfylketunnelen", 
      c("12478V320582", "906727246", "40696V1727469", "41451V320581", "50741V1727509", "35382V1727514", "64040V320581", "66678V320582", "81631V1727485", 
        "93189V320582", "17949V320695", "57279V320244", "68351V319882", "92102V319885")
    ),
    table_exclusions("Strandgata bygges om til bussvei", c("83652V319725", "16074V319868", "906727238"), 100),
    table_exclusions("Ukjent", c("906727244", "71798V319583"), 94),
    table_exclusions("Ny arm mellom fv. og E39 ved Hove", c("906727237", "43296V319721"), 40, 40),
    table_exclusions("Arbeider i Hoveveien?", c("906727237", "88125V320152"), 91),
    # table_exclusions("Ukjent", c("35382V1727514"), 87, 91),
    table_exclusions("Lite data", c("89794V320138"))
  )

# Chain part 3: 2023__
# universal_year_period_ids: 99-
exclusions_nj_2023__ <-
  dplyr::bind_rows(
    table_exclusions("Bussvegen i Strandgata", c("12478V320582", "906727244", "83652V319725")),
    table_exclusions("Hoveveien unormal hele 2023", c("89794V320138")),
    table_exclusions("Ukjent", c("906727264"), 124, 131),
    table_exclusions("Arbeider nord for Storhaugtunnelen", c("57279V320244"), 122, 132),
    table_exclusions("Ukjent", c("906727246"), 119, 119),
    table_exclusions("Ukjent", c("906727267"), 139),
    table_exclusions("Feil antall vindinger på sensorene", c("64040V320581"), 138),
    table_exclusions("Feil antall vindinger på sensorene?", c("86207V319742"), 125),
    table_exclusions("Arbeider ved Bjergsted", c("58562V320296"), 122, 149),
    table_exclusions("Lassa", c("71535V319524"), 124, 131),
    table_exclusions("Ukjent", c("73355V319671"), 142, 149),
    table_exclusions("Ukjent", c("906727234"), 142)
  )
