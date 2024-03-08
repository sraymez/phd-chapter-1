## GPS coordinate extraction



# Environment set up ----



# Libraries
{library(pacman)
p_load(rio, here, ggmap, sf, tidyverse)}


# Working Data
{pathogen_data <- import(here("Extracted Data", "pathogen_data.csv"))
reservoir_data <- import(here("Extracted Data", "reservoir_data.csv"))
vector_data <- import(here("Extracted Data", "vector_data.csv"))}


# Shapefile data
sen_adm0 <- st_read(here("Shapefiles", "gadm41_SEN_0.shp"))


# API ----



# Create vector of API generated from Google Cloud Platform 
my_gmap_api <- "AIzaSyDnvX3UsM7Fx3kU9w4rdlKdpMPu27u92ZA"


# Double check API key is registered
register_google(key = my_gmap_api)




# Test extraction ----



# Specify location you want coordinates for
#location <- "dakar, senegal"


# Extract and store GPS coordinates into object
#coordinates <- geocode(location)


# Print coordinates
# options(digits = 5)
# print(coordinates)


# Source the area you're interested in using google base map 
# region_oi <- get_map(location = "simenti", 
#                      maptype = "roadmap", # can change to "satellite"
#                      source = "google", 
#                      api_key = my_gmap_api)


# Plot map
#ggmap(region_oi)


# Supply coordinates retrieved to cross check they match location
# region_oi_check <- get_map(location = c(coordinates$lon, coordinates$lat),
#                            maptype = "roadmap",
#                            source = "google",
#                            api_key = my_gmap_api)


# Plot map
#ggmap(region_oi_check)


# Can also plot coordinates as a point on map 
# ggmap(region_oi) +
#   geom_point(data = coordinates,
#              aes(x = lon, y = lat),
#              colour = "red",
#              size = 4) +
#   geom_segment(aes(x=37, y=-1, 
#                    xend=36.82195, yend=-1.292066), 
#                arrow = arrow(length=unit(.5, 'cm')),
#                colour = "red", lwd = 1)



# Extract coordinates for location column  ----



# Data from '02_data_cleaning'


# Subset rows that need coordinates
missing_coord_fnc <- function(data) {
  data %>% 
    filter(is.na(gps_latitude)) %>% 
    select(full_location_name) %>% 
    distinct(full_location_name, .keep_all = T)
}


{missing_path_coords <- pathogen_data %>% missing_coord_fnc()
missing_res_coords <- reservoir_data %>% missing_coord_fnc()
missing_vec_coords <- vector_data %>% missing_coord_fnc()}


# Extract coordinates
extract_coords_fnc <- function(data) {
  data %>% mutate(geocode(full_location_name))
}


extract_path_coords <- missing_path_coords %>% extract_coords_fnc()
extract_res_coords <- missing_res_coords %>% extract_coords_fnc()
extract_vec_coords <- missing_vec_coords %>% extract_coords_fnc()


# Rename columns to match original data frames  
rename_fnc <- function(data) {
  data %>% 
    rename(gps_latitude = lat,
           gps_longitude = lon)
}


{extract_path_coords <- extract_path_coords %>% rename_fnc()
extract_res_coords <- extract_res_coords %>% rename_fnc()
extract_vec_coords <- extract_vec_coords %>% rename_fnc()}


# Check whether extracted coordinates are correct


# Senegal bounding box
st_bbox(sen_adm0)


# Check whether any coordinate falls outside Senegal bounding box (if none, returns df with 0 rows)
extract_path_coords %>% 
  mutate(wrong_coords = case_when(
    gps_longitude > -11.34247 & gps_longitude < -17.54319 &
    gps_latitude > 16.69207 & gps_latitude < 12.30786 ~ "Yes",
    TRUE ~ "No")) %>%
  filter(wrong_coords == "yes")

extract_res_coords %>% 
  mutate(wrong_coords = case_when(
    gps_longitude > -11.34247 & gps_longitude < -17.54319 &
      gps_latitude > 16.69207 & gps_latitude < 12.30786 ~ "Yes",
    TRUE ~ "No")) %>%
  filter(wrong_coords == "yes")

extract_vec_coords %>% 
  mutate(wrong_coords = case_when(
    gps_longitude > -11.34247 & gps_longitude < -17.54319 &
      gps_latitude > 16.69207 & gps_latitude < 12.30786 ~ "Yes",
    TRUE ~ "No")) %>%
  filter(wrong_coords == "yes")


# Check for too many duplicated coordinates
dup_coords <- extract_res_coords %>% 
  filter(duplicated(gps_longitude))


# Merge extracted coordinates back into original data
pathogen_data <- pathogen_data %>% 
  left_join(extract_path_coords, 
            by = "full_location_name", suffix = c("_df1", "_df2")) %>% 
  mutate(gps_latitude = coalesce(gps_latitude_df1, gps_latitude_df2),
         gps_longitude = coalesce(gps_longitude_df1, gps_longitude_df2)) %>% 
  select(-c(gps_latitude_df1, gps_latitude_df2, gps_longitude_df1, gps_longitude_df2))


reservoir_data <- reservoir_data %>% 
  left_join(extract_res_coords, 
            by = "full_location_name", suffix = c("_df1", "_df2")) %>% 
  mutate(gps_latitude = coalesce(gps_latitude_df1, gps_latitude_df2),
         gps_longitude = coalesce(gps_longitude_df1, gps_longitude_df2)) %>% 
  select(-c(gps_latitude_df1, gps_latitude_df2, gps_longitude_df1, gps_longitude_df2))


vector_data <- vector_data %>% 
  left_join(extract_vec_coords, 
            by = "full_location_name", suffix = c("_df1", "_df2")) %>% 
  mutate(gps_latitude = coalesce(gps_latitude_df1, gps_latitude_df2),
         gps_longitude = coalesce(gps_longitude_df1, gps_longitude_df2)) %>% 
  select(-c(gps_latitude_df1, gps_latitude_df2, gps_longitude_df1, gps_longitude_df2))



# Save intermediate data ----



# Write .csv of intermediate data  
{export(pathogen_data, here("Working Data", "pathogen_data_complete_coords.csv"))
export(reservoir_data, here("Working Data", "reservoir_data_complete_coords.csv"))
export(vector_data, here("Working Data", "vector_data_complete_coords.csv"))}



# End ----