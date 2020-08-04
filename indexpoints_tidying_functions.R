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

choose_new_city_trp_ids <- function(city_name,
                                start_year) {

  trp_ids <- cities_points %>%
    dplyr::filter(city_area_name == city_name,
                  agreement_start == start_year) %>%
    dplyr::select(trp_id) %>%
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

#filename <- "data_index_raw/punktindeks_trondheim-2020-04.csv"
read_new_pointindex_csv_with_volumes <- function(filename) {
  # Read new csv export from Datainn
  readr::read_csv2(filename,
                   locale = readr::locale(
                     encoding = "latin1",
                     decimal_mark = ",",
                     grouping_mark = " ")) %>%
    filter(døgn == "Alle",
           lengdeklasse == "< 5,6m",
           periode == "Hittil i år") %>%
    rename(trp_id = trpid,
           base_volume = 12,
           calc_volume = 11) %>%
    mutate(index = as.numeric(str_replace(indeks, ",", ".")),
           base_volume = as.numeric(as.character(base_volume)),
           calc_volume = as.numeric(as.character(calc_volume))) %>%
    select(trp_id, base_volume, calc_volume, index)
}

#filename <- "data_index_raw/punktindeks_nord-jaeren-2020-01.csv"
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

calculate_two_years_index_36_month_version <- function(city_index_df) {

  # TODO: add sd and ci

  months_1_24 <- city_index_df %>%
    select(index, index_i) %>%
    slice(1:2)

  months_25_36 <- city_index_df %>%
    select(index, index_i) %>%
    slice(3)

  first_24_months <- list(
    index = 100 * (prod(months_1_24$index_i) - 1),
    index_i = prod(months_1_24$index_i)) %>%
    as_tibble()

  months_1_36 <- bind_rows(first_24_months, months_25_36)

  all_36_months_index <- list(
    index = 100 * (prod(months_1_36$index_i) - 1),
    index_i = prod(months_1_36$index_i)) %>%
    as_tibble()

  return(all_36_months_index)
}

calculate_all_possible_36_month_indexes <- function(city_monthly_df) {

  # A for-loop that loops through all consecutive and possible
  # 36-month periods
  # Using just single months
  city_months <- city_monthly_df %>%
    filter(periode != "Hele året")

  no_months <- nrow(city_months)
  n_end <- no_months - 35

  all_possible_36_month_indexes <- tibble::tibble()

  for (n in 1:n_end) {

    # n from n_start to n_end
    start_month <- n
    end_month <- 35 + n

    # The 36 months for this iteration
    city_monthly_36 <- city_months %>%
      slice(start_month:end_month) %>%
      tibble::rowid_to_column("id") %>%
      mutate(three_year_group = case_when(
        id <= 12 ~ 1,
        id <= 24 ~ 2,
        id <= 36 ~ 3,
        TRUE ~ 4
      ))

    # The end month for this iteration
    city_monthly_36_period <- city_months %>%
      slice(end_month) %>%
      select(periode, year) %>%
      mutate(year = stringr::str_sub(year, 6, 9))

    # The 36 month index for this iteration
    city_monthly_36_index <- city_monthly_36 %>%
      #filter(three_year_group < 4) %>%
      group_by(three_year_group) %>%
      summarise(volume_index_year = sum(traffic_index_year),
                volume_base_year = sum(traffic_base_year),
                index = (volume_index_year / volume_base_year - 1 ) * 100,
                index_i = volume_index_year / volume_base_year) %>%
      #select(index, index_i) %>%
      calculate_two_years_index_36_month_version() %>%
      bind_cols(city_monthly_36_period)

    all_possible_36_month_indexes <- bind_rows(
      all_possible_36_month_indexes,
      city_monthly_36_index
    )

  }

  return(all_possible_36_month_indexes)
}



split_road_system_reference <- function(df) {

  df_with_split_reference <- df %>%
    tidyr::separate(road_reference, c("road_system", "intersection_part"),
                    sep = "[[:blank:]][[:alpha:]]{1}D",
                    remove = FALSE, fill = "right") %>%
    dplyr::mutate(road_category = stringr::str_sub(road_system, 1, 1)) %>%
    dplyr::mutate(road_category = factor(road_category,
                                         levels = c("E", "R", "F", "K", "P"))) %>%
    tidyr::separate(road_system, c("road", "section_meter"),
                    sep = " S") %>%
    dplyr::mutate(road_number = as.numeric(stringr::str_sub(road, 3, -1))) %>%
    dplyr::mutate(road_category_and_number = paste0(road_category, "v", road_number)) %>%
    tidyr::separate(section_meter, c("section_number", "subsection_meter"),
                    sep = "D", convert = TRUE) %>%
    tidyr::separate(subsection_meter, c("subsection_number", "meter"),
                    sep = " m", convert = TRUE) %>%
    tidyr::separate(intersection_part, c("intersection_part_number", "intersection_meter"),
                    sep = " m", convert = TRUE)  %>%
    dplyr::arrange(road_category, road_number,
                   section_number, subsection_number, meter,
                   intersection_part_number, intersection_meter)
}




