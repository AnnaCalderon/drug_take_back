dea_data <- read_csv(file = "DEA_public_disposal.csv") |> 
  clean_names() |> 
  distinct(bus_name, addr_1, addr_2, city_state_zip) |> 
  filter(str_detect(city_state_zip, "NY")) 

data1 <- dea_data |> 
  # start with a number 
  filter(str_starts(addr_1, "[0-9]+"))

geocoded_data1 <- data1 |> 
  select(bus_name, addr_1, city_state_zip) |> 
  separate(addr_1,c("num","addr_1"), sep="^\\S*\\K\\s+") |> 
  separate(city_state_zip,c("city","state_zip"), sep=",\\s*") |> 
  separate(state_zip, c("state","zip"), sep=" \\s*") |> 
  mutate(address_original = paste0(num, ", ", addr_1, ", ", city, ", ", 
                                   state, ", ", zip)) |> 
  slice(1:5) |> 
  tidygeocoder::geocode(
    address = address_original,
    #method = "osm"
    method ="arcgis" 
  )

reverse_data1 <- geocoded_data1 |> 
  tidygeocoder::reverse_geocode(
    lat = lat,
    long = long,
    method = "osm"
  ) 

counties_13 <- reverse_data1 |> 
  #remove everything before the word county
  mutate(county = gsub('\\s*County.*$', '', address)) |> 
  #remove everything before the last coma
  mutate(county = sub(",([^,]*)$", " --\\1", county) ) |> 
  mutate(county = gsub('^.*--\\s*', '', county)) |> 
  select(county, zip) |> 
  group_by(county) |> 
  mutate(county_locations = n()) |> 
  ungroup()
