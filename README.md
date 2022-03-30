# EarthRanger-Upload

MoveApps

Github repository: https://github.com/leetalbott/EarthRanger-Upload

## Description
Simple app to upload data from a movestack to EarthRanger via API.

## Documentation
This app takes data from a movestack object, coerces to json via jsonlite packager, and iterates over the elements to push to EarthRanger via httr package. 

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
none.

### Parameters 
server:  EarthRanger target server
api_key:  EarthRanger api key
