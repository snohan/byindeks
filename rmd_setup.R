# Common set ups

# Packages ####
library(tidyverse)
library(leaflet)
library(knitr)
library(flextable)
library(ghql)
library(jsonlite)
library(httr)
library(lubridate)
library(RColorBrewer)
library(webshot)
library(officer)
library(scales)

source("H:/Programmering/R/byindeks/index_report_functions.R")

# knitr options ####
knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE,
                      error = FALSE,
                      cache = FALSE)

# Map essentials ####
nvdb_map_url <-
  "https://nvdbcache.geodataonline.no/arcgis/rest/services/Trafikkportalen/GeocacheTrafikkJPG/MapServer/tile/{z}/{y}/{x}"

nvdb_map_attribution <-
  "NVDB, Geovekst, kommunene og Open Street Map contributors (utenfor Norge)"

nvdb_crs <- leafletCRS(
  crsClass = "L.Proj.CRS", code = "EPSG:25833",
  proj4def = "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs",
  resolutions = c(
    21674.7100160867,
    10837.35500804335,
    5418.677504021675,
    2709.3387520108377,
    1354.6693760054188,
    677.3346880027094,
    338.6673440013547,
    169.33367200067735,
    84.66683600033868,
    42.33341800016934,
    21.16670900008467,
    10.583354500042335,
    5.291677250021167,
    2.6458386250105836,
    1.3229193125052918,
    0.6614596562526459,
    0.33072982812632296,
    0.16536491406316148
  ),
  origin = c(-2500000.0, 9045984.0))

# Functions
decimal_comma <- function(number) {
  stringr::str_replace(as.character(number), "\\.", ",")
}

decimal_point <- function(number) {
  stringr::str_replace(as.character(number), ",", "\\.")
}
