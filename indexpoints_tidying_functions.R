# Index tidying functions

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
    filter(Vegkategori == "E+R+F+K",
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

monthly_city_index <- function(filename) {
  # Read standard csv export from Datainn
  read.csv2(filename, stringsAsFactors = F) %>%
    filter(Vegkategori == "E+R+F+K",
           døgn == "Alle",
           lengdeklasse == "< 5,6m",
           periode != "Siste 12 måneder") %>%
    mutate(index = as.numeric(str_replace(indeks, ",", ".")),
           dekning = as.numeric(str_replace(dekning, ",", ".")),
           standardavvik = as.numeric(as.character(standardavvik)),
           konfidensintervall = as.numeric(as.character(konfidensintervall)),
           periode = if_else(periode == "Hittil i år",
                             "Hele året",
                             periode)) %>%
    select(periode, index, dekning, standardavvik, konfidensintervall) %>%
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

# TODO: get a compound ci, need to iterate pairwise through the years!
# I.e. make accumulated index for one more year
#index_from_refyear <- 100*(prod(city_index_grenland$index_i)-1)

# Need number of points each year
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

create_pointindex_map <- function(all_point_info_df) {
  # Create a red-green scale based on index values
  negative_value <- round(abs(min(all_point_info_df$index, na.rm = T)), digits = 0) + 1
  positive_value <- round(max(all_point_info_df$index, na.rm = T), digits = 0) + 1

  rc1 <- colorRampPalette(colors = c("red", "white"), space = "Lab")(negative_value)

  ## Make vector of colors for values larger than 0 (180 colors)
  rc2 <- colorRampPalette(colors = c("white", "green"), space = "Lab")(positive_value)

  ## Combine the two color palettes
  rampcols <- c(rc1, rc2)

  palett_index <-
    colorNumeric(palette = rampcols,
                 domain = NULL)

  pointindex_map <- all_point_info_df %>%
    leaflet(width = "100%",
            options = leafletOptions(crs = nvdb_crs,
                                     zoomControl = F)) %>%
    addTiles(urlTemplate = nvdb_map_url,
             attribution = nvdb_map_attribution) %>%
    addCircleMarkers(
      radius = 6,
      stroke = T,
      weight = 2,
      color = "#444f55",
      opacity = 0.8,
      fill = T,
      fillColor = ~palett_index(index),
      fillOpacity = 0.8
    ) %>%
    addLegend("bottomright",
              pal = palett_index,
              values = ~index,
              title = "Indeks",
              opacity = 0.7,
              labFormat = labelFormat(big.mark = " "))

  return(pointindex_map)
}
