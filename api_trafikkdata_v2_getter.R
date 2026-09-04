library(tidyverse)
library(httr2)

# https://httr2.r-lib.org/
# https://trafikkdata-api-v2.atlas.vegvesen.no/swagger-ui/index.html


basic_request <- 
  httr2::request("https://trafikkdata-api-v2.atlas.vegvesen.no/") |> 
  httr2::req_headers(
    "accept" = "application/json",
    "x-client" = "snorre.hansen@vegvesen.no"
  )

# httr2::req_dry_run(basic_request)




trp_id <- "10795V320297"

# get_kmdt
test <- 
  basic_request |> 
  httr2::req_url_path_append("/v2/beta/trafikkmengde/trafikkregistreringspunkter/") |> 
  httr2::req_url_path_append(trp_id) |> 
  httr2::req_url_path_append("/kalenderjustertmaanedsdognstrafikk?fra=2026&til=2026")

response <- 
  httr2::req_perform(test) |> 
  httr2::resp_body_json()

kmdt <- 
  response$resultater |> 
  bind_rows()


# ?
  uthenta <-
    jsonlite::fromJSON(
      stringr::str_conv(
        respons$content,
        encoding = "UTF-8"
      ),
      simplifyDataFrame = T,
      flatten = T
    )
