# EarthRanger-Upload

MoveApps

Github repository: https://github.com/PADAS/moveapps-integration

## Description
Simple app to upload data from a move2 object to EarthRanger via API.

## Documentation
This app takes data from a move2 object, coerces to json via jsonlite packager, and iterates over the elements to push to EarthRanger via httr package. 

### Input data
move2 data

### Output data
move2 data (passthrough)

### Artifacts
none.

### Parameters 
- server:  EarthRanger target server
- api_key:  EarthRanger api key
- batch_size:  number of records to push to EarthRanger at one time - batch_size of 0 causes all records to be pushed at once
- event_type:  description of event to store in EarthRanger system
- moveapp_id:  a user-supplied id to pass to EarthRanger
- eventlist_fields:  Additional parameters from the move2 object to pass to EarthRanger as part of the event_details
