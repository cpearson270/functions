#### WaterLevelUpdate
## DESCRIPTION: Completes pull request using EQuIS API OData
## CALLED BY: 00_Start
## REQUIRES:
# API details
# api_key <- readLines('API/Key.txt', warn = FALSE)
# base_url <- "https://newcrestuat.equisonline.com/api/odata/"
# query_table <- "DT_WATER_LEVEL"
# query_filter <- "?$filter=DRY_INDICATOR_YN eq null"
# orderby <- #column name

## Investigate use of custom fn and "map" {purr} fn

## OUTPUT:
# df <- contents of pull request

## Load packages
required_libs <- c("httr", "jsonlite")

if(any(!required_libs %in% (.packages()))){
  load_libs <- dplyr::setdiff(required_libs, (.packages()))
  lapply(load_libs, library, character.only = TRUE)
  rm(load_libs)
}
rm(required_libs)

api_key <- readLines('API/Key.txt', warn = FALSE)

encoded_query <- URLencode(paste0(query_table,query_filter))

# Complete the full URL with the encoded query string
url <- paste0(base_url, encoded_query)

# Make the GET request
response <- GET(
  url,
  add_headers(Authorization = paste("Bearer", api_key))
)

# Check the status of the response
status_code(response)

# Parse the JSON content (assuming the response is in JSON format)
if (status_code(response) == 200) {
  data <- content(response, as = "text")
  parsed_data <- fromJSON(data)
  df <- parsed_data[["value"]]

  # provide API error message
} else {
  error_message <- content(response, as = "text", encoding = "UTF-8")
  print(error_message)
}

i<-0
df_temp<-c(1)

while (length(df_temp)>0) {
  i<-i+1
  query <- paste0(query_table
                  ,query_filter
                  ,"&$skip="
                  ,i
                  ,"000
                    &$top=1000
                    &$orderby="
                  ,orderby)
  encoded_query <- URLencode(query)
  
  # Complete the full URL with the encoded query string
  url <- paste0(base_url, encoded_query)
  
  # Make the GET request
  response <- GET(
    url,
    add_headers(Authorization = paste("Bearer", api_key))
  )
  
  # Check the status of the response
  status_code(response)
  
  # Parse the JSON content (assuming the response is in JSON format)
  if (status_code(response) == 200) {
    data <- content(response, as = "text")
    parsed_data <- fromJSON(data)
    df_temp <- parsed_data[["value"]]
    df <- bind_rows(df, df_temp)
  }
}
rm(df_temp)