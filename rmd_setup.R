# Common set ups

# Packages ####
base::Sys.setlocale(locale = "nb.utf8")
library(tidyverse)
library(rmarkdown)
library(leaflet)
library(knitr)
library(flextable)
library(ghql)
library(jsonlite)
library(httr)
library(lubridate)
library(clock)
library(RColorBrewer)
library(webshot2)
library(officer) # fp_border color in flextable
#library(officedown)
library(scales)

source("H:/Programmering/R/byindeks/index_report_functions.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("H:/Programmering/R/byindeks/leaflet_nvdb_map_setup.R")

svv_background_color <- "#F5F5F5"

# knitr options ####
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  error = FALSE,
  cache = FALSE,
  dpi = 150,
  dev = "ragg_png",
  tab.topcaption = TRUE
  )

# Functions
decimal_comma <- function(number) {
  stringr::str_replace(as.character(number), "\\.", ",")
}

decimal_point <- function(number) {
  stringr::str_replace(as.character(number), ",", "\\.")
}

# Flextable defaults
flextable::set_flextable_defaults(
  font.size = 8,
  font.family = "Arial",
  #font.family = "Lucida Sans Unicode",
  #font.family = "LTF Etica Light",
  #padding.bottom = .2,
  #padding.top = .4,
  decimal.mark = ",",
  big.mark = " ",
  na_str = ""
)

# Numbering tables
# table_numbers <- officer::run_autonum(
#   seq_id = "table",
#   pre_label = "Tabell ",
#   post_label = ". ",
#   bkm = NULL,
#   bkm_all = FALSE,
#   prop = NULL
# )
