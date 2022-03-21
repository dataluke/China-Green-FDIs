library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(googlesheets4)
library(ISOcodes)
library(nominatimlite)

#### Master spreadsheet
sheet_Info <- sheets_get("XXX") # google sheets id

sheet_colnames_raw = read_sheet(sheet_Info$spreadsheet_id, sheet = 1, range = "A1:Az2", col_names = F)
sheet_colnames = apply(sheet_colnames_raw, 2, function(x) paste(x[!is.na(x)], collapse = "_"))

# reading the first 554 projects
# now read all projects since API column is populated
sheet_df = read_sheet(sheet_Info$spreadsheet_id, sheet = 1, range = "A3:Az964", col_names = sheet_colnames)

# filtering projects that are points and having level of precision greater than 3 or point projects with no coords
sheet_df_filtered <- sheet_df %>%
  filter(`Coordinate type` == "Point")

Key_Google_API = "" # removed API keys
key_MapQuest = ""

geocode_url_base = "https://maps.googleapis.com/maps/api/geocode/json?"
osm_geocode_url_base = "http://open.mapquestapi.com/nominatim/v1/search.php?"

for (i in 1:nrow(sheet_df_filtered)) {
  project_addr = sheet_df_filtered$`GOOGLE MAPS API INFORMATION`[i]
  
  if(is.na(project_addr)){
    next
  }else{
    project_index = sheet_df_filtered$`Geo-locating_Project Index`[i]
    project_ISO3166_3 = sheet_df_filtered$ISO[i]
    project_ISO3166_2 = tolower(ISO_3166_1$Alpha_2[which(ISO_3166_1$Alpha_3==project_ISO3166_3)])
    project_addr_format = gsub(" ","+", project_addr)
    
    API_url = paste0(geocode_url_base,
                     "address=",project_addr_format,
                     "&region=",project_ISO3166_2,
                     "&key=",Key_Google_API)
    
    API_url_encode = URLencode(API_url)
    #### Save Google maps geocode results to JSON
    Google_JSON_path = paste0("Google_json/Google_project_",
                              str_pad(project_index,4,pad = "0"),
                              ".json")
    project_geocode = content(GET(API_url_encode, 
                                  write_disk(Google_JSON_path, overwrite=TRUE)))
    
    #### OSM geocode search
    OSM_API_url = paste0(osm_geocode_url_base,
                         "format=json",
                         "&q=", project_addr_format,
                         "&countrycodes=",project_ISO3166_2,
                         "&addressdetails=1",
                         "&limit=1",
                         "&key=", key_MapQuest)
    
    OSM_API_url_encode = URLencode(OSM_API_url)
    #### Save OSM geocode reulsts to JSON
    OSM_JSON_path = paste0("OSM_json/OSM_project_",
                           str_pad(project_index,4,pad = "0"),
                           ".json")
    project_geocode_OSM = content(GET(OSM_API_url_encode, 
                                      write_disk(OSM_JSON_path, overwrite=TRUE)))
  }
  print(paste0(i," of ",nrow(sheet_df_filtered)))
  
  
}




