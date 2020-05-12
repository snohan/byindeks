# Index tidying functions

choose_city_trp_ids <- function(city_name,
                                start_year) {

  trp_ids <- cities_points %>%
    dplyr::filter(city_area_name == city_name,
                  agreement_start == start_year) %>%
    dplyr::select(trp_id, legacyNortrafMpn) %>%
    dplyr::rename(msnr = legacyNortrafMpn) %>%
    dplyr::filter(!is.na(msnr)) %>%
    dplyr::filter(!is.na(trp_id))
}

readPointindexCSV <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename) %>%
    filter(døgn == "Alle",
           lengdeklasse == "< 5,6m",
           periode == "Hittil i år") %>%
    mutate(trs = as.numeric(msnr),
           trafikkmengde.basisaar = as.numeric(
             as.character(trafikkmengde.basisår)),
           trafikkmengde.indeksaar = as.numeric(
             as.character(trafikkmengde.indeksår))) %>%
    group_by(trs) %>%
    summarise(trafikkmengde_basisaar = sum(trafikkmengde.basisaar),
              trafikkmengde_indeksaar = sum(trafikkmengde.indeksaar),
              index = round((trafikkmengde_indeksaar/
                               trafikkmengde_basisaar - 1) * 100,
                            digits = 1)) %>%
    rename(msnr = trs) %>%
    select(msnr, index)
}

read_new_pointindex_csv <- function(filename) {
  # Read new csv export from Datainn
  read.csv2(filename) %>%
    filter(døgn == "Alle",
           lengdeklasse == "< 5,6m",
           periode == "Hittil i år") %>%
    mutate(index = as.numeric(str_replace(indeks, ",", ".")),
           trafikkmengde.basisaar = as.numeric(
             as.character("trafikkmengde basisår")),
           trafikkmengde.indeksaar = as.numeric(
             as.character("trafikkmengde indeksår"))) %>%
  rename(trp_id = trpid) %>%
  select(trp_id, index)
}

filename <- "data_index_raw/punktindeks_nord-jaeren-2020-01.csv"
#filename <- "data_index_raw/pointindex_trondheim-2019-12_2018.csv"
read_pointindex_csv_with_volumes <- function(filename) {
  # Read standard csv export from Datainn
  df <- read.csv2(filename,
                  stringsAsFactors = F) %>%
    dplyr::filter(døgn == "Alle",
           lengdeklasse == "< 5,6m",
           periode == "Hittil i år") %>%
    dplyr::mutate(index = round(
                            as.numeric(decimal_point(indeks)),
                            digits = 1)) %>%
    dplyr::rename(base_volume = trafikkmengde.basisår,
                  calc_volume = trafikkmengde.indeksår) %>%
    dplyr::mutate(base_volume = as.numeric(base_volume),
                  calc_volume = as.numeric(calc_volume)) %>%
    dplyr::select(msnr, base_volume, calc_volume, index)
}

read_bikepointindex_csv <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename) %>%
    filter(døgn == "Alle",
           periode == "Hittil i år") %>%
    mutate(msnr = as.numeric(msnr),
           index = as.numeric(str_replace(indeks, ",", "."))) %>%
    select(msnr, index)
}

read_city_index_csv <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename) %>%
    rename_all(str_to_lower) %>%
    filter(vegkategori == "E+R+F+K",
           døgn == "Alle",
           lengdeklasse == "< 5,6m",
           periode == "Hittil i år") %>%
    mutate(index = as.numeric(str_replace(indeks, ",", ".")),
           dekning = as.numeric(str_replace(dekning, ",", ".")),
           standardavvik = as.numeric(as.character(standardavvik)),
           konfidensintervall = as.numeric(as.character(konfidensintervall))) %>%
    select(index, dekning, standardavvik, konfidensintervall) %>%
    as_tibble()
}

#filename <- "data_index_raw/Bergen-2017-12_2016.csv"
monthly_city_index <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename, stringsAsFactors = F) %>%
    rename_all(tolower) %>%
    filter(vegkategori == "E+R+F+K",
           døgn == "Alle",
           lengdeklasse == "< 5,6m",
           periode != "Siste 12 måneder",
           indeks != "-") %>%
    mutate(index = as.numeric(str_replace(indeks, ",", ".")),
           dekning = as.numeric(str_replace(dekning, ",", ".")),
           standardavvik = as.numeric(as.character(standardavvik)),
           konfidensintervall = as.numeric(as.character(konfidensintervall)),
           periode = if_else(periode == "Hittil i år",
                             "Hele året",
                             periode)) %>%
    rename(traffic_index_year = 'trafikkmengde.indeksår',
           traffic_base_year =  'trafikkmengde.basisår') %>%
    mutate(traffic_index_year = as.numeric(traffic_index_year),
           traffic_base_year = as.numeric(traffic_base_year)) %>%
    select(periode, traffic_index_year, traffic_base_year,
           index, dekning, standardavvik, konfidensintervall) %>%
    as_tibble()
}

read_bike_index_csv <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename) %>%
    filter(Vegkategori == "E+R+F+K",
           døgn == "Alle",
           lengdeklasse == "Alle",
           periode == "Hittil i år") %>%
    mutate(index = as.numeric(str_replace(indeks, ",", ".")),
           dekning = as.numeric(str_replace(dekning, ",", ".")),
           standardavvik = as.numeric(as.character(standardavvik)),
           konfidensintervall = as.numeric(as.character(konfidensintervall))) %>%
    select(index, dekning, standardavvik, konfidensintervall) %>%
    as_tibble()
}


index_converter <- function(index) {
  ifelse(
    is.na(index),
    1,
    index/100 + 1)
}

# Compound ci, need to iterate pairwise through the years!
# I.e. make accumulated index for one more year
#index_from_refyear <- 100*(prod(city_index_grenland$index_i)-1)

calculate_two_year_index <- function(city_index_df) {

  two_years <- city_index_df %>%
    select(index, index_i, dekning, variance, n_points) %>%
    slice(1:2)

  year_one <- str_sub(city_index_df$year[1], 1, 4)
  year_two <- str_sub(city_index_df$year[2], 6, 9)

  two_years_to_one <- list(
    index = 100 * (prod(two_years$index_i) - 1),
    index_i = prod(two_years$index_i),
    year = paste0(year_one, "-", year_two),
    dekning = mean(two_years$dekning),
    # Using Goodman's unbiased estimate (cannot use exact formula as we are
    # sampling)
    # But it can be negative if indexes are close to zero, large variance and
    # small n's.
    # Resolved by using exact formula
    # Must be something about the assumptions that are wrong?
    variance =
      two_years$index[1]^2 * two_years$variance[2] / two_years$n_points[2] +
      two_years$index[2]^2 * two_years$variance[1] / two_years$n_points[1] +
      two_years$variance[1] * two_years$variance[2] /
      (two_years$n_points[1] * two_years$n_points[2]),
    n_points = max(two_years$n_points)
  ) %>%
    as_tibble() %>%
    dplyr::mutate(standardavvik = sqrt(variance),
                  konfidensintervall = 1.96 * sqrt(variance) /
                    sqrt(2))
}


