library('move')
library('jsonlite')
library('httr')

rFunction = function(server,api_key,batch_size,event_type,moveapp_id,data) {
  Sys.setenv(tz="UTC")

  if (is.null(batch_size))
  {
    logger.info("Your batch size is not supplied. We use default 50 observations per API call")
    batch_size <- 50
  }
  
#  make.names(names(data@data),allow_=FALSE)
  num_batches <- ifelse(batch_size==0,1,ceiling(nrow(data)/batch_size)) # determine number of batches needed for outer loop based on number of observations in the movestack
  
  status_codes <- numeric() # for capturing API response status codes
  for (i in 1:num_batches)
  {
    if (batch_size==0) batch <- data else batch <- data[((i-1)*batch_size+1):(min(i*batch_size,nrow(data))),]
    names(batch@data) <- gsub(".","_",names(batch@data),fixed=TRUE)
    batch@data$moveapp_id <- moveapp_id
    batch@data[is.na(batch@data)] <- ""
    for (j in 1:nrow(batch))
      {
      output <- list("device_id"=batch@data$tag_id[j]
                     ,"recorded_at"=format(batch@data$timestamp[j],"%Y-%m-%d %X%z")
                     ,"location"=list("x"=batch@data$location_long[j],"y"=batch@data$location_lat[j])
                     ,"moveapp_id"=batch@data$moveapp_id
                     ,"title"=batch@study
                     ,"event_type"=event_type
                     ,"event_details"=as.list(batch[j]@data)
                     )
      output_json <- toJSON(output,pretty=TRUE,auto_unbox=TRUE) # convert list to json
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

    er_post <- POST(
      url = server,
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
