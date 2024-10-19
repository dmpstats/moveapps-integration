library('move2')
library('jsonlite')
library('httr')
library('sf')
library('units')

## The parameter "data" is reserved for the data object passed on from the previous app

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

`%!in%` <- Negate(`%in%`)

# Showcase injecting app setting (parameter `year`)
rFunction_old = function(server,api_key,batch_size,event_type,moveapp_id,eventlist_fields,data, ...) {

  Sys.setenv(tz="UTC")
  if (is.null(batch_size))
  {
    logger.info("Your batch size is not supplied. We use default 50 observations per API call")
    batch_size <- 50
  }
  
  num_batches <- ifelse(batch_size==0,1,ceiling(nrow(data)/batch_size)) # determine number of batches needed for outer loop based on number of observations in the movestack

  # set up missing required columns
  if("tagID" %!in% names(data))  data$tagID <- mt_track_id(data)
  if("timestamp" %!in% names(data))  data$timestamp <- mt_time(data)

  if("location_long" %!in% names(data)){
    if(st_is_longlat(data)){
      lon_lat <- st_coordinates(data)
    }else{
      lon_lat <- data |>
        sf::st_transform(4326) |>
        sf::st_coordinates()
    }
    data$location_long <- lon_lat[, 'X']
    data$location_lat <- lon_lat[, 'Y']
  }
  
  status_codes <- numeric() # for capturing API response status codes

  names(data) <- gsub(".","_",names(data),fixed=TRUE)
  eventlist_fields_parsed <- unlist(strsplit(eventlist_fields,",|;")) # parse requested event list field names into a vector (allows comma or semicolon)
  eventlist_fields_trimmed <- gsub("\\s+","",eventlist_fields_parsed) # remove spaces from parsed event list field names
  eventlist_fields_filtered <- eventlist_fields_trimmed[eventlist_fields_trimmed %in% names(data)] # remove requested event list field names that are not in the dataset
  if (length(eventlist_fields_filtered)==0) eventlist_fields_filtered <- names(data) # use all field names if no event list field names were supplied
  eventlist_fields_filtered <- eventlist_fields_filtered[eventlist_fields_filtered != "geometry"]
  
  for (i in 1:num_batches)
  {
    if (batch_size==0) batch <- data else batch <- data[((i-1)*batch_size+1):(min(i*batch_size,nrow(data))),]

    for (j in 1:nrow(batch))
    {
      output <- list("device_id"=batch$tagID[j]
                     ,"recorded_at"=format(batch$timestamp[j],"%Y-%m-%d %X%z")
                     ,"location"=list("x"=batch$location_long[j],"y"=batch$location_lat[j]))
      event_details = list()
      for(field in eventlist_fields_filtered)
      {
        event_details[field] <- batch[[field]][j]
        if(class(event_details[field]) == "units") events_details[field] <- drop_units(events_details[field])
      }
      output$event_details <- event_details
    
      output_json <- toJSON(output,pretty=FALSE,auto_unbox=TRUE) # convert list to json
      output_json_str <- toString(output_json)
      if (j==1)
      {
        all_output_json_str <- output_json_str
      } else
      {
        all_output_json_str <- paste0(all_output_json_str,",",output_json_str)
      }
    }
    er_json <- paste0("[",all_output_json_str,"]") # convert list to json and add square brackets to conform to ER API expected json format
    url <- server
    if(!grepl("\\?", server))
    {
      url <- paste0(url, "?")
    }
    else
    {
      url <- paste0(url, "&")  
    }
    url <- paste0(url, "event_type=", URLencode(event_type), "&moveapp_id=", URLencode(moveapp_id))
    
    browser()
    
    logger.info(paste0("Posting data to ", url))
    er_post <- POST(
      url = url,
      add_headers(apikey = api_key,
                  "accept" = "application/json",
                  "content-type" = "application/json")
      ,body = er_json
    )
    
    status_codes[i] <- status_code(er_post)
  }
  
  logger.info(paste0(sum(status_codes==200)," of ",num_batches," batches posted successfully to EarthRanger"))
  result <- data
  return(result)
    
}
