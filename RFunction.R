library('move')
library('jsonlite')
library('httr')

rFunction = function(server,api_key,data) {
  Sys.setenv(tz="UTC")

  status_codes <- numeric() # for capturing API response status codes
  for (i in 1:nrow(data))
  {
    if ("clusterID" %in% names(data)) # if the input movestack contains a cluster id field then output as a cluster event
    {
      output <- list("device_id"=data@data$tagID[i]
                     ,"recorded_at"=format(data@data$timestamp[i],"%Y-%m-%d %X%z")
                     ,"location"=list("x"=data@data$location.long[i],"y"=data@data$location.lat[i])
                     ,"title"=data@study
                     ,"event_type"="moveapps_cluster"
                     ,"event_details"=list("cluster_id"=data@data$clusterID[i],"clustered_points"=data@data$n.locs[i],"clustered_ids"=data@data$n.ids[i])
                     )
    } else # else output a generic event
    {
      output <- list("device_id"=data@data$tagID[i]
                     ,"recorded_at"=format(data@data$timestamp[i],"%Y-%m-%d %X%z")
                     ,"location"=list("x"=data@data$location_long[i],"y"=data@data$location_lat[i])
                     ,"title"=data@study
                     ,"event_type"="moveapps_other"
                     ,"event_details"=list("dummy_detail_1"=1,"dummy_detail_2"=2)
      )
    }
    
    er_json <- paste0("[",toJSON(output,pretty=TRUE,auto_unbox=TRUE),"]") # convert list to json and add square brackets to conform to ER API expected json format

    # ER API call
    er_post <- POST(
      url = server
      ,add_headers(.headers = c(paste0('apikey: ',api_key)
                                , 'accept: application/json'
                                , 'Content-Type: application/json'))
      , body = er_json
    )
    status_codes[i] <- status_code(er_post)
  }
  #print(paste0(sum(status_codes==200)," of ",nrow(data@data)," events posted successfully to EarthRanger"))
  
  result <- data
  return(result)
}
