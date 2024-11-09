#### WaterLevelUpdate
## Description: Completes push request using EQuIS API OData
## REQUIRES:
# df <- subset table of original GET request (for edit_urls)
# patch_data <- list(
  #  cas_rn = ">C10_C19"
  #)

## CALLED BY: 00_Start


## Load packages
required_libs <- c("httr", "jsonlite")

if(any(!required_libs %in% (.packages()))){
  load_libs <- dplyr::setdiff(required_libs, (.packages()))
  lapply(load_libs, library, character.only = TRUE)
  rm(load_libs)
}
rm(required_libs)

api_odata_push <- function(df
                          ,patch_data
                          ,api_key_path = 'API/Key.txt') {
  
  api_key <- readLines(api_key_path, warn = FALSE)
  
  # initialise status record
  patch_status <- tibble(record=character()
                         ,status_code=integer()
                         ,status_desc=character()
                         ,status_message=character())
  
  records <-  df %>% pull("@odata.id")
  
  #update_remark <- paste0(
  #  '[',
  #  initials,
  #  format(Sys.Date(),"%Y%m%d"),
  #  ']cas_rn updated from ',
  #  cas_rn_current)
  
  for (record in records) {
    
    url_patch <- URLencode(record)
    
    # Prepare body
  #  patch_data <- list(
  #    cas_rn = cas_rn_updated,
  #    remark = if(is.na(df$REMARK[i]))
  #      update_remark
  #    else paste(update_remark,df$REMARK[i] ,sep = "; ")
  #  )
    
    json_data <- toJSON(patch_data, auto_unbox = TRUE)
    
    # Make the PATCH request
    response <- PATCH(
      url_patch,
      add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json",
        Accept = "application/json"
      ),
      body = json_data
    )
    
    # Print the response content (if any)
    content <- content(response, as = "text", encoding = "UTF-8")
    
    patch_status <- patch_status %>% 
      add_row(record = str_extract(record, "\\(.*?\\)")
              ,status_code = status_code(response)
              ,status_desc = if (status_code(response) == "204") {
                "Success."
              } else if (status_code(response) == "400") {
                "OData path not recognized."
              } else if (status_code(response) == "401") {
                "User credentials are required."
              } else if (status_code(response) == "403") {
                "Not authorized to perform this action."
              } else if (status_code(response) == "404") {
                "The given record does not exist and cannot be updated."
              } else if (status_code(response) == "500") {
                "Internal server error."
              }
              ,status_message = content)
    cat(format(Sys.time())
        ,str_extract(record, "\\(.*?\\)")
        ,paste0("status=",status_code(response))
        ,"\n"
        ,sep="; "
        ,file = "outputs\\API_push_log.txt"
        ,append=TRUE)
  }
  return(list(
    status_detail = patch_status
    ,status_sumary= patch_status %>% 
      group_by(status_code,status_desc) %>% 
      summarise(count=n())
))
}
