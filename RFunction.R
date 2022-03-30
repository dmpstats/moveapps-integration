library('move')
library('jsonlite')
library('httr')

rFunction = function(server,api_key,batch_size,data) {
  Sys.setenv(tz="UTC")

  if (is.null(batch_size))
  {
    logger.info("Your batch size is not supplied. We use default 50 observations per API call")
    batch_size <- 50
  }
  
  num_batches <- ifelse(batch_size==0,1,ceiling(nrow(data)/batch_size)) # determine number of batches needed for outer loop based on number of observations in the movestack
  for (i in 1:num_batches)
  {
    if (batch_size==0) batch <- data else batch <- data[((i-1)*batch_size+1):(min(i*batch_size,nrow(data))),]
    for (j in 1:nrow(batch))
    {
      if ("clusterID" %in% names(batch)) # if the input movestack contains a cluster id field then output as a cluster event
      {
        output <- list("device_id"=batch@data$tagID[j]
                       ,"recorded_at"=format(batch@data$timestamp[j],"%Y-%m-%d %X%z")
                       ,"location"=list("x"=batch@data$location.long[j],"y"=batch@data$location.lat[j])
                       ,"title"=batch@study
                       ,"event_type"="moveapps_cluster"
                       ,"event_details"=list("cluster_id"=batch@data$clusterID[j],"clustered_points"=batch@data$n.locs[j],"clustered_ids"=batch@data$n.ids[j])
        )
      } else # else output a generic event
      {
        output <- list("device_id"=batch@data$tagID[j]
                       ,"recorded_at"=format(batch@data$timestamp[j],"%Y-%m-%d %X%z")
                       ,"location"=list("x"=batch@data$location_long[j],"y"=batch@data$location_lat[j])
                       ,"title"=batch@study
                       ,"event_type"="moveapps_other"
                       ,"event_details"=list("dummy_detail_1"=1,"dummy_detail_2"=2)
        )
      }
      output_json <- toJSON(output,pretty=TRUE,auto_unbox=TRUE) # convert list to json and add square brackets to conform to ER API expected json format
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
      , body = er_json
    )
  }
  
  result <- data
  return(result)
}
