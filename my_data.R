# Get county polygons 

source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")

# 31 Østfold         
# 32 Akershus        
# 33 Buskerud        
#  3 Oslo            
# 34 Innlandet       
# 39 Vestfold        
# 40 Telemark        
# 42 Agder           
# 11 Rogaland        
# 46 Vestland        
# 15 Møre og Romsdal 
# 50 Trøndelag       
# 18 Nordland        
# 55 Troms           
# 56 Finnmark        

fylkenumre <- c(31, 32, 33, 3, 34, 39, 40, 42, 11, 46, 15, 50, 18, 55, 56)

fylker <-
  purrr::map(
    fylkenumre,
    ~ hent_fylke(.x)
  ) |> 
  dplyr::bind_rows() |> 
  sf::st_transform("+proj=longlat +datum=WGS84")

readr::write_rds(
  fylker,
  "H:/my_data/fylker.rds"
)
