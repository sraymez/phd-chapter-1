## Data cleaning 



# Environment set up ----



# Packages
{library(pacman)
p_load(tidyverse)}


# Data
{pathogen_data <- import(here("Extracted Data", "pathogen_data.csv"))
reservoir_data <- import(here("Extracted Data", "reservoir_data.csv"))
vector_data <- import(here("Extracted Data", "vector_data.csv"))}


# Data wrangling ----



# Prefix ID columns with relevant nomenclature
# Drop columns not being used
# Prepare location name column
# Apply str_to_title over important columns



pathogen_data_clean <- pathogen_data %>% 
  mutate(across(everything(), ~ifelse(. == "", NA, as.character(.)))) %>% 
  mutate(across(c(study_id, pathogen_id, sample_size, num_pos_pcr, 
                  num_pos_ser, num_pos_other, gps_latitude, gps_longitude),
                as.numeric)) %>% 
  mutate(pathogen_id_new = paste0("path", "_", pathogen_id), .after = pathogen_id) %>% 
  select(-c(associated_reservoir_id, associated_vector_id, comment)) %>% 
  mutate(full_location_name = str_replace_all(location_name, "_", " "),
         full_location_name = str_to_title(full_location_name),
         country = str_to_title(country),
         full_location_name = paste0(full_location_name, ", ", country), .after = location_name) %>% 
  mutate(across(c(month, season, pathogen_tested, population_tested, topography, ecoregion), str_to_title)) %>% 
  mutate(pathogen_tested = case_when(
    pathogen_tested == "Trypanosoma_congolense" |
      pathogen_tested == "Trypanosoma_congolense And Trypanosoma_vivax" | 
        pathogen_tested == "Trypansosomiasis" ~ "Trypanosomiasis",
    TRUE ~ as.character(pathogen_tested)))


reservoir_data_clean <- reservoir_data %>% 
  mutate(across(everything(), ~ifelse(. == "", NA, as.character(.)))) %>% 
  mutate(across(c(study_id, reservoir_id, sample_size, gps_latitude, gps_longitude),
                as.numeric)) %>% 
  mutate(reservoir_id_new = paste0("res", "_", reservoir_id), .after = reservoir_id) %>% 
  select(-c(associated_pathogen_id, associated_vector_id, sampling_method, comment)) %>% 
  mutate(full_location_name = str_replace_all(location_name, "_", " "),
         full_location_name = str_to_title(full_location_name),
         country = str_to_title(country),
         full_location_name = paste0(full_location_name, ", ", country), .after = location_name) %>% 
  mutate(across(c(month, season, genus, common_name, topography, ecoregion), str_to_title)) %>% 
  mutate(common_name = case_when(
    common_name == "Small_ruminant" ~ "Small_ruminants",
    common_name == "Mouse" ~ "House_mouse",
    common_name == "Hore" ~ "Horse",
    TRUE ~ as.character(common_name)))


vector_data_clean <- vector_data %>% 
  mutate(across(everything(), ~ifelse(. == "", NA, as.character(.)))) %>% 
  mutate(across(c(study_id, vector_id, sample_size, gps_latitude, gps_longitude),
                as.numeric)) %>% 
  mutate(vector_id_new = paste0("vec", "_", vector_id), .after = vector_id) %>% 
  select(-c(associated_pathogen_id, associated_reservoir_id, sampling_method, comment)) %>% 
  mutate(full_location_name = str_replace_all(location_name, "_", " "),
         full_location_name = str_to_title(full_location_name),
         country = str_to_title(country),
         full_location_name = paste0(full_location_name, ", ", country), .after = location_name) %>% 
  mutate(across(c(month, season, genus, topography, ecoregion), str_to_title)) %>% 
  mutate(
    genus = case_when(
      genus == "Aedes " ~ "Aedes",
      TRUE ~ as.character(genus)),
    species = case_when(
      species == "africanus" ~ "africana",
      species == "furficer" ~ "furcifer",
      species == "furficer_taylori" ~ "furcifer_taylori",
      species == "vitattue" ~ "vittatus",
      species == "vitattus" ~ "vittatus",
      TRUE ~ as.character(species)))



# Save intermediate data ----



# Write .csv of intermediate data 
{export(pathogen_data_clean, here("Working Data", "pathogen_data_clean.csv"))
export(reservoir_data_clean, here("Working Data", "reservoir_data_clean.csv"))
export(vector_data_clean, here("Working Data", "vector_data_clean.csv"))}



# Write .csv of place names with no coordinates ----


# Pull NA values for coordinates from all data and bind together
# all_locations_no_coords <- reservoir_data %>% 
#   select(full_location_name, gps_latitude, gps_longitude) %>% 
#   bind_rows(vector_data %>% 
#               select(full_location_name, gps_latitude, gps_longitude)) %>%
#   filter(is.na(gps_latitude)) %>% 
#   group_by(full_location_name) %>% 
#   count() %>% 
#   arrange(desc(n))
# 
# write.csv(all_locations_no_coords, "R/Working Data/all_location_names_no_coords.csv", row.names = F)



# End ----