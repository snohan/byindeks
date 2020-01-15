# Testing new pointindex
source("rmd_setup.R")
source("get_from_trafficdata_api.R")
source("get_from_trp_api.R")
library(tictoc)

#trps <- get_trp_for_vti()
trps <- read.csv2("trp_for_vti_2020.csv")

#trp_ids <- '23392V625266", "44656V72812'

trp_ids_clean <- trps %>%
  dplyr::mutate(trp_id = as.character(trp_id)) %>%
  dplyr::filter(trp_id != "59628V885948") # Ok med MM nå

trp_ids <- trp_ids_clean$trp_id[501] %>%
  stringr::str_c(collapse = "\", \"")

#cat(trps$trp_id[1:2], sep = '", "')

tic()
trp_index <- get_pointindices(trp_ids, "2019")
toc()

trp_with_index <- trps %>%
  dplyr::left_join(trp_index)
