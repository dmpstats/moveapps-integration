library('move2')
library("jsonify")
library('httr')
library('sf')
library('units')
library("dplyr")
library("tidyr")
library("purrr")
library("cli")
library('lubridate')

#local helpers
`%!in%` <- Negate(`%in%`)


rFunction = function(data,
                     server, 
                     api_key,
                     grouping_col = NULL,
                     event_type,
                     moveapp_id,
                     eventlist_fields = NULL) {
  
  ## Initial set up ------------------------------------------------------------

  Sys.setenv(tz="UTC")
  
  # store original data to return as the app's output (i.e. making it a pass-through app)
  data_orig <- data

  # useful variables
  tm_id_col <- mt_time_column(data)
  trck_id_col <- mt_track_id_column(data)

  # set maximum batch size in Megabytes
  batch_max_Mb <- 10
  
  
  ## Input Validation  ---------------------------------------------------------
  
  if(!is.null(grouping_col)){
    if(!is.character(grouping_col) || length(grouping_col) > 1){
      stop(paste0(
        "`grouping_col` must be a character string of length 1. Please provide a valid ",
        "column name to define groups for batch uploading to EarthRanger"
      ))
    } else if(grouping_col %!in% names(data) ){
      stop(paste0(
        "Specified column name '", grouping_col, "' must be present in the input data. ",
        "Please ensure the grouping column for batch uploading is correctly specified in ",
        "`grouping_col` argument"
      ))
    }
  }

  ## Pre-processing ------------------------------------------------------------
  
  logger.info("Pre-processing data")
  
  ### -- Add explicit grouping column, for internal use only 
  if(is.null(grouping_col)){
    # No specific groupings, i.e. rows are treated as independent
    # Implemented by setting rowwise groups, to conform with remaining code
    data$group_id <- 1:nrow(data)
  }else{
    data$group_id <- data[[grouping_col]]
  }
  
  
  ### -- Process fields chosen to be included in upload as event details
  if(is.null(eventlist_fields) || (length(eventlist_fields) == 1 && nchar(eventlist_fields) == 0)){
    event_fields <- names(data_orig)
  }else {
    # parse requested event list field names into a vector (allows comma or semicolon)
    event_fields_parsed <- unlist(strsplit(eventlist_fields,",|;")) 
    event_fields <- gsub("\\s+", "", event_fields_parsed)
  }
  
  # forcing names of all chosen fields to lower case, as per ER requirement
  data <- dplyr::rename_with(data, tolower, .cols = dplyr::any_of(event_fields))
  event_fields <- tolower(event_fields)
  
  
  ### -- Process expected lat lon columns
  if("location_long" %!in% names(data)){
    
    if(st_is_longlat(data)){
      lon_lat <- st_coordinates(data)
    }else{
      lon_lat <- data |>
        sf::st_transform(4326) |>
        sf::st_coordinates()
    }
    data$x <- lon_lat[, 'X']
    data$y <- lon_lat[, 'Y']
    
  }else{
    data <- dplyr::rename(data, x = location_long, y = location_lat)
  }
  
  
  ### -- Format key api request fields
  data <- data |> 
    data.frame() |> 
    dplyr::mutate(
      # date-time cols formatted as ISO 8601 strings
      dplyr::across(dplyr::where(~inherits(.x, "POSIXt")), \(x) format_iso8601(x)),
      # convert any columns of class integer64 to string, for safer json serialization
      dplyr::across(where(~inherits(.x, "integer64")), as.character),
      recorded_at = .data[[tm_id_col]],
    ) |> 
    dplyr::rename(any_of(c(device_id = trck_id_col))) |> 
    units::drop_units()
  
  
  ### -- Build the api endpoint url
  if(!grepl("\\?", server)){
    url <- paste0(server, "?")
  } else{
    url <- paste0(server, "&")  
  }
  
  url <- paste0(url, "event_type=", URLencode(event_type), "&moveapp_id=", URLencode(moveapp_id))
  
  
  
  ### -- Prepare data, via nesting, for dataframe-based json coercion 
  data_jsonble <- data |>
    mutate(rowindex = dplyr::row_number()) |> # to enforce row-level nesting
    tidyr::nest(
      location = c(x, y),
      event_details = any_of(event_fields),
      .by = c(rowindex, group_id, device_id, recorded_at)
    ) |>
    dplyr::mutate(
      location = lapply(location, as.list),
      event_details = lapply(event_details, as.list),
      # bring any double-listed elements of length 1, generated from nesting on
      # list-columns, 1-level up. This ensures arrays don't get double-wrapped
      # in json coercion
      event_details = purrr::modify_depth(event_details, 2, function(x){
        if(is.list(x) && length(x) == 1){
          x <- x[[1]]
        } 
        x
      })
    )
  
  
  
  ## Define batches for uploading  ------------------------------------------------
  
  logger.info(paste0("Defining batches (maximum batch size: ", batch_max_Mb, "Mb)"))
  
  ### -- Workout batches, ensuring that each batch: 
  ###  (i) shouldn't be greater than ~10Mb 
  ###  (ii) keeps specified groups intact
  
  #### get a sample row of the json data
  json_row <- data_jsonble |> 
    dplyr::slice(1) |> 
    dplyr::select(device_id, recorded_at, location, event_details) |> 
    jsonify::to_json(unbox = TRUE, digits = 4, numeric_dates = FALSE) |> 
    toString()
  
  #### get size of sample entry in bytes
  row_json_bytes <- as.numeric(object.size(json_row))
  
  #### table with batch ids
  batching_tbl <- data |> 
    # rows per group
    dplyr::count(group_id) |> 
    # size per group 
    dplyr::mutate(group_Mbs = (n * row_json_bytes)/1024^2) |> 
    dplyr::arrange(group_Mbs) |> 
    dplyr::mutate(
      # cumulative size over groups
      group_cum_Mbs = cumsum(group_Mbs),
      # generate batches with no more than ~`batch_size_Mb`
      batch_id = dplyr::dense_rank(group_cum_Mbs %/% batch_max_Mb)
    )
  
  # merge batching info with jsonable data
  data_jsonble <- data_jsonble |> 
    dplyr::left_join(
      batching_tbl |> dplyr::select(group_id, batch_id), 
      by = "group_id"
    )
  
  num_batches <- length(unique(batching_tbl$batch_id))
  
  
  ## Perform batch requests to EarthRanger endpoint ----------------------------
  
  logger.info(paste0("Starting the upload of ", num_batches, " batch(es) of data to ER"))
  
  if(num_batches > 1){
    if(is.null(grouping_col)){
      logger.info(paste0(
        "No grouping column specified, thus observations will be split arbitrarily ",
        "between consecutive batches"))
      }else{
        logger.info(paste0(
          "Batches comply with grouping column '", grouping_col, "', ensuring observations ",
          "from the same group are posted together"))  
      }  
  }
  
  status_codes <- data_jsonble |> 
    # split data by batch id
    split(~batch_id) |> 
    # iterate over list, each element performing a post request
    purrr::map_int(
      \(batch_dt){
        
        #browser()
        
        # coerce current batch to json
        er_json_str <- batch_dt |>
          dplyr::select(device_id, recorded_at, location, event_details) |>
          jsonify::to_json(unbox = TRUE, numeric_dates = FALSE, digits = 4) |>
          toString()
        
        # post request
        er_post <- POST(
          url = url,
          add_headers(
            apikey = api_key,
            "accept" = "application/json",
            "content-type" = "application/json"),
          body = er_json_str
        )

        # if present, report request error to the logger
        if(http_error(er_post)){
          logger.warn(paste0("    |- Error in POST request. Message:\n'", content(er_post)$message, "'"))
        }

        # return response status code
        status_code(er_post)

      }, .progress = list(
        format = "Posted batch {cli::pb_current}/{cli::pb_total} {cli::pb_bar}| ETA: {cli::pb_eta}"
      )
    )
  
  logger.info(paste0(sum(status_codes==200), " of ", num_batches, " batches posted successfully to EarthRanger"))
  
  # return initial input data
  return(data_orig)
  
}



# wrapper to lubridate's `format_ISO8601()` to insert colon in TZ's time
# component (i.e. +HH:MM), which seems to be a requirement when POSTing from
# MoveApps to ER...
format_iso8601 <- function(x, tz_colon = TRUE){
  
  if(!is.POSIXt(x)) cli::cli_abort("x must be a {.cls POSIXt} object, not {.cls {class(x)}}")
  
  out <- lubridate::format_ISO8601(x, usetz = TRUE)
  
  if(tz_colon) out <- sub('([+-][0-9]{2})([0-9]{2}$)','\\1:\\2', out, fixed = FALSE)
  out
}



