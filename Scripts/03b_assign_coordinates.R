## GPS coordinate extraction



# Environment set up ----



# Libraries
{library(pacman)
p_load(ggmap, sf, tidyverse)}


# Working Data
{pathogen_data <- import(here("Working Data", "pathogen_data_clean.csv"))
reservoir_data <- import(here("Working Data", "reservoir_data_clean.csv"))
vector_data <- import(here("Working Data", "vector_data_clean.csv"))}


# Manually extracted coordinate data
coordinates <- import(here("Working Data", "all_location_names_coords_found.csv"))



# Assign coordinates to location names



# Make sure coordinates are numeric
coordinates <- coordinates %>% 
  mutate(across(c(gps_latitude, gps_longitude), as.numeric)) # NA's introduced are location where coords couldn't be found


# # Merge extracted coordinates back into original data 
pathogen_data_comp_coords <- pathogen_data %>% 
  left_join(coordinates, 
            by = "full_location_name", 
            suffix = c("_df1", "_df2")) %>% 
  mutate(gps_latitude = coalesce(gps_latitude_df1, 
                                 gps_latitude_df2),
         gps_longitude = coalesce(gps_longitude_df1, 
                                  gps_longitude_df2))%>% 
  select(-c(gps_latitude_df1, gps_latitude_df2, gps_longitude_df1, gps_longitude_df2))


reservoir_data_comp_coords <- reservoir_data %>% 
  left_join(coordinates, 
            by = "full_location_name", 
            suffix = c("_df1", "_df2")) %>% 
  mutate(gps_latitude = coalesce(gps_latitude_df1, 
                                 gps_latitude_df2),
         gps_longitude = coalesce(gps_longitude_df1, 
                                  gps_longitude_df2))%>% 
  select(-c(gps_latitude_df1, gps_latitude_df2, gps_longitude_df1, gps_longitude_df2))


vector_data_comp_coords <- vector_data %>% 
  left_join(coordinates, 
            by = "full_location_name", 
            suffix = c("_df1", "_df2")) %>% 
  mutate(gps_latitude = coalesce(gps_latitude_df1, 
                                 gps_latitude_df2),
         gps_longitude = coalesce(gps_longitude_df1, 
                                  gps_longitude_df2))%>% 
  select(-c(gps_latitude_df1, gps_latitude_df2, gps_longitude_df1, gps_longitude_df2))



# Save intermediate data ----



# Write .csv of intermediate data  
{write.csv(pathogen_data_comp_coords, here("Working Data", "pathogen_data_complete_coords.csv"))
write.csv(reservoir_data_comp_coords, here("Working Data", "reservoir_data_complete_coords.csv"))
write.csv(vector_data_comp_coords, here("Working Data", "vector_data_complete_coords.csv"))}



# End ----