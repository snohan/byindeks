cookies = c(
  '$Cookie: datainn-theme' = 'light',
  '_ga' = 'GA1.2.1110575218.1550050524',
  '_hjid' = '5da218dc-c9f0-4d68-914e-9b04cfd91cf7',
  '_gid' = 'GA1.2.2059720420.1587370005',
  'mywork.tab.tasks' = 'false',
  'www.vegvesen.no-oam' = '\\u0021dHC5cJ5fUbVqsKN70DO+g5MXuqiwU26+CtHU4H5u5t4a6pFUUKBOQRShJfVWYwdhyCenNoDM1RHExD4=',
  'i18next' = 'nb-NO',
  'iPlanetDirectoryProOAM' = 'AQIC5wM2LY4SfczrJb3bo7lyaaHvIhkCiKmjOf8ApkeK5jg.*AAJTSQACMDIAAlNLABM3MDU1NDQxNzk0NjQ0OTEwNjAwAAJTMQACMDU.*',
  'datainn.vegvesen.no' = '118880010.36895.0000',
  'trafikkdata-lb.vegvesen.no-8051' = '487913226.29471.0000',
  'trafikkdata-lb.vegvesen.no-8052' = '471136010.29727.0000',
  'www.vegvesen.no-443' = '\\u0021A183IaEFOXoBqIp70DO+g5MXuqiwU/VhrVjrnXQZ3fQoMkvZ0Zhn9NJfMoZ2BddquzTcZXS9EQXamBM=',
  'amlbcookie' = '01',
  'iPlanetDirectoryProOAMutv' = 'GQ44FzxgnpDCKyor7dAkVuDI4oQ.*AAJTSQACMDIAAlNLABxyM1lDZDY2cVVjU292MzN2RDVGZFlJUWQ1SmM9AAR0eXBlAANDVFMAAlMxAAIwMQ..*'
)

headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json',
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.92 Safari/537.36',
  `Content-Type` = 'application/json',
  `Origin` = 'https://www.vegvesen.no',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Sec-Fetch-Dest` = 'empty',
  `Referer` = 'https://www.vegvesen.no/datainn/adm/traffic-registration-point/?query=query%20%7B%0A%20%20areas%20%7B%0A%20%20counties%20%7B%0A%20%20%20%20id%0A%20%20%20%20name%0A%20%20%7D%0A%09%7D%0A%7D',
  `Accept-Language` = 'nb-NO,nb;q=0.9,no;q=0.8,nn;q=0.7,en-US;q=0.6,en;q=0.5,sv;q=0.4,da;q=0.3'
)

data = '{"query":"query { areas { counties { id name } } }" }'

api_query <-
  "query vti_trp { trafficRegistrationPoints (trafficType: VEHICLE) { id name } }"

api_query_string <- paste0('{"query":"', api_query, '" }')

res <- httr::POST(url = 'https://www.vegvesen.no/datainn/adm/traffic-registration-point/api/',
                  httr::add_headers(.headers = trp_api_headers),
                  httr::set_cookies(.cookies = trp_api_cookies),
                  body = api_query_string)

res_parsed <- fromJSON(str_conv(res$content, encoding = "UTF-8"),
         simplifyDataFrame = T,
         flatten = T) %>%
  as.data.frame()
