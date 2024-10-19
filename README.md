# EarthRanger-Upload

MoveApps

Github repository: https://github.com/PADAS/moveapps-integration

## Description
Simple App to upload data from a `move2` object to EarthRanger via API.

## Documentation
This App takes data from a `move2` object, coerces to json via the `{jsonify}` package, and pushes it to EarthRanger in batches via the `{httr}` package. Each batch is limited to approximately 10MB, with an option to group observations by a specific attribute (e.g., study ID, track ID, cluster ID). Grouped observations will be kept together within the same batch during the upload process. 

As a pass-through App, no changes are made to the output data. Therefore, it is strongly advised that users check the App's logs to confirm the success of the data upload.

### Input data
`move2` data

### Output data
`move2` data (pass-through)

### Artifacts
none.

### Parameters 
- **Target server for upload** [`server`]:  EarthRanger target server
- **EarthRanger API key** [`api_key`]:  EarthRanger api key
- **Grouping Column** [`grouping_col`]: Name of the column in the input data for grouping observations that should remain together in the same batch during data upload to EarthRanger. If set to NULL, the data will be split into batches as needed without respecting any groupings
- **Event Type** [`event_type`]:  description of event to store in EarthRanger system
- **MoveApp Identifier** [`moveapp_id`]:  a user-supplied id to pass to EarthRanger
- **Fields to Pass to ER** [`eventlist_fields`]:  Additional parameters from the move2 object to pass to EarthRanger as part of the event_details


### Most common errors

When using parameter `grouping_col` for batching, please note:

- The specified grouping column must be present in the event-level table of the input `move2` object. Otherwise, the App will terminate and return an error.

- The batching process strictly adheres the specified grouping attribute, which means that a batch containing a large group may potentially exceed the intended 10MB batch size. Currently, transmissions to EarthRanger are effectively limited to a fixed 32MB per upload request. Therefore, if an upload fails due to an oversized batch, consider reducing the transmittable data by, e.g.: 

  - Reducing the number of fields to pass to EarthRanger with `eventlist_fields` parameter;
  
  - Thinning the input data (i.e. downsample points) using the App [Thin Data by Time](https://www.moveapps.org/apps/browser/9c814c17-c61c-4cad-857d-2b44402a78b3);
  
  - Shortening the time window of the input data.



 
 
