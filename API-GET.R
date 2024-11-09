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

api_odata_get <- function(base_url
                          ,query_table,query_filter,oderby
                          ,api_key_path = 'API/Key.txt') {

  api_key <- readLines(api_key_path, warn = FALSE)
  i <- -1
  df <- data.frame()
  repeat {
    i <- i+1
    query <- paste0(query_table
                    ,query_filter
                    ,"&$skip="
                    ,i * 1000
                    ,"&$top=1000
                      &$orderby="
                    ,orderby)
    
    url <- paste0(base_url, URLencode(query))
    
    response <- GET(
      url,
      add_headers(Authorization = paste("Bearer", api_key))
    )
    
    if (status_code(response) != 200) {
      print(content(response, as = "text", encoding = "UTF-8"))
      break
    }
    
    data <- content(response, as = "text")
    df_temp <- fromJSON(data)[["value"]]
    
    if (length(df_temp) == 0) {
      break
    }
    
    df <- bind_rows(df, df_temp)
  }
  return(df)
}