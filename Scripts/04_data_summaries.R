## Summaries of extracted data



# Environment set up ----



# Libraries
{library(pacman)
p_load(rio, here, tmap, sf, raster, terra, tidyverse)}


# Working Data
{pathogen_data_summaries <- import(here("Working Data", "pathogen_data_complete_coords.csv"))
reservoir_data_summaries <- import(here("Working Data", "reservoir_data_complete_coords.csv"))
vector_data_summaries <- import(here("Working Data", "vector_data_complete_coords.csv"))}


# Shapefile data
{sen_adm0 <- st_read(here("Shapefiles", "gadm41_SEN_0.shp"))
gmb_adm0 <- st_read(here("Shapefiles", "gadm41_GMB_0.shp"))}


# Negate function
`%ni%` <- Negate(`%in%`)


# Ggplot themes
source("Scripts/ggplot_themes.R")


# Custom colour ramp
colscalex = colorRampPalette(RColorBrewer::brewer.pal(3, name="YlGnBu"))(3)


# Graphs ----



# When were studies most carried out
path_years_plot <- pathogen_data_summaries %>%
  mutate(start_year = str_sub(year, start = 1, end = 4)) %>% 
  select(study_id, start_year, country)

res_years_plot <- reservoir_data_summaries %>% 
  mutate(start_year = str_sub(year, start = 1, end = 4)) %>% 
  select(study_id, start_year, country)

vec_years_plot <- vector_data_summaries %>% 
  mutate(start_year = str_sub(year, start = 1, end = 4)) %>% 
  select(study_id, start_year, country)

  
plot <- path_years_plot %>% 
  bind_rows(res_years_plot) %>% 
  bind_rows(vec_years_plot) %>% 
  distinct(study_id, start_year, .keep_all = T) %>% 
  group_by(start_year, country) %>% 
  count() %>% 
  drop_na() %>% 
  ggplot(aes(x = start_year, 
             y = n, 
             fill = country)) +
  geom_bar(stat = "identity", 
           position = "stack") +
  custom_theme() +
  labs(x = "Year", 
       y = "Count", 
       fill = "Country",
       title = "Number of references by year and country") +
  scale_fill_manual(values = c( "orange3", "blue4"))
plot


# Clean environment to avoid cluttering
rm(path_years_plot, res_years_plot, vec_years_plot)


# Where were studies mostly carried out
location_plot <- reservoir_data_summaries %>% 
  select(full_location_name) %>% 
  group_by(full_location_name) %>% 
  count() 


ggplot(location_plot %>% 
         filter(n >= 20), 
       aes(x = reorder(full_location_name, -n), 
           y = n)) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = "darkgreen") +
  labs(x = "Location", 
       y = "Count") +
  custom_theme()


# What pathogen was most tested
pathogen_plot <- pathogen_data_summaries %>% 
  select(pathogen_tested) %>% 
  group_by(pathogen_tested) %>% 
  count()


ggplot(pathogen_plot,
       aes(x = reorder(pathogen_tested, -n), 
           y = n)) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = "blue4") +
  labs(x = "Pathogen tested", 
       y = "Count") + 
  custom_theme()


# What reservoir was most sampled
reservoir_plot <- reservoir_data_summaries %>% 
  select(common_name) %>% 
  group_by(common_name) %>% 
  count()


ggplot(reservoir_plot %>% 
         filter(!is.na(common_name)), 
       aes(x = reorder(common_name, -n) , 
           y = n)) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = "darkred") +
  labs(x = "Animal common name", 
       y = "Count") +
  custom_theme()


# What vector was most sampled
vector_plot <- vector_data_summaries %>% 
  select(genus, species) %>% 
  group_by(genus, species) %>% 
  count() %>% 
  mutate(full_name = paste0(genus, ", ", species)) %>% 
  filter(genus != "Glossina")


ggplot(vector_plot %>% 
         filter(!is.na(full_name)), 
       aes(x = reorder(full_name, -n) , 
           y = n)) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = "darkorange") +
  labs(x = "Vector species name", 
       y = "Count") +
  custom_theme()


# Maps ----



# Convert data frames to spatial objects
pathogen_data_sf <- pathogen_data_summaries %>% 
  filter(!is.na(gps_latitude)) %>% 
  st_as_sf(coords = c("gps_longitude", "gps_latitude"), crs = 4326)


reservoir_data_sf <- reservoir_data_summaries %>% 
  filter(!is.na(gps_latitude)) %>% # remove location names without coordinates
  st_as_sf(coords = c("gps_longitude", "gps_latitude"), crs = 4326)


vector_data_sf <- vector_data_summaries %>% 
  filter(!is.na(gps_latitude)) %>% # remove location names without coordinates
  st_as_sf(coords = c("gps_longitude", "gps_latitude"), crs = 4326)


# Map of pathogen data
tm_shape(sen_adm0) +
  tm_polygons("white", 
              border.alpha = 0.75) +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 2) +
  tm_layout(frame = F, 
            legend.position = c(0.8,0.6), 
            legend.height = 0.4) +
  tm_shape(pathogen_data_sf) +
  tm_dots(size = 0.2, 
          border.alpha = 0.05, 
          col = "pathogen_tested", 
          palette = "viridis",
          title = "Pathogen") +
  tm_compass(type = "4star", 
             size = 1, 
             position = c("left", "top"))


# Map of livestock data
tm_shape(sen_adm0) +
  tm_polygons("white", 
              border.alpha = 0.5) +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 1.75) +
  tm_layout(frame = F, 
            legend.position = c(0.8,0.75)) +
  tm_shape(reservoir_data_sf %>% 
             filter(common_name %in% c("Cattle", "Goat", "Sheep", "Small_ruminant", "Small_ruminants"))) +
  tm_dots(size = 0.2, 
          border.alpha = 0.05, 
          col = "common_name",
          palette = "viridis",
          title = "Livestock",) +
  tm_compass(type = "4star", size = 1, position = c("left", "top"))


# Map of rodent data
tm_shape(sen_adm0) +
  tm_polygons("white", 
              border.alpha = 0.5) +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 1.75) +
  tm_layout(frame = F, 
            legend.position = c(0.9,0.4)) +
  tm_shape(reservoir_data_sf %>% 
             filter(genus %in% c("Arvicanthis", "Cricetomys", "Crocidura", "Mastomys",
                                 "Mus", "Mus_nannomys", "Rattus", "Praomys", "Steatomys",
                                 "Taterillus", "Thryonomys", "Uranomys"))) +
  tm_dots(size = 0.2, 
          border.alpha = 0.05, 
          col = "genus",
          palette = "viridis", 
          title = "Rodents") +
  tm_compass(type = "4star", 
             size = 1, 
             position = c("left", "top"))


tm_shape(sen_adm0) +
  tm_polygons("white", 
              border.alpha = 0.5) +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 1.75) +
  tm_layout(frame = F, 
            legend.position = c(0.9,0.5)) +
  tm_shape(reservoir_data_sf %>% 
             filter(common_name %in% c("Chimpanzee", "Green_monkey", "Guinea_baboon", "Patas_monkey", "Red_colobus", "Vervet_monkey"))) +
  tm_dots(size = 0.2, 
          border.alpha = 0.05, 
          col = "genus",
          palette = "viridis", 
          title = "Primates") +
  tm_compass(type = "4star", 
             size = 1, 
             position = c("left", "top"))


# Map of aedes and culex data
tm_shape(sen_adm0) +
  tm_polygons("white", 
              border.alpha = 0.5) +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 1.75) +
  tm_layout(frame = F, 
            #legend.position = c(0.9,0.2),
            legend.outside = T) +
  tm_shape(vector_data_sf %>% 
             filter(genus %in% c("Aedes", "Culex"))) +
  tm_dots(size = 0.2, 
          border.alpha = 0.05, 
          col = "species",
          palette = "viridis", 
          title = "Aedes and Culex vectors") +
  tm_compass(type = "4star", 
             size = 1, 
             position = c("left", "top"))


# Map of tick data
tm_shape(sen_adm0) +
  tm_polygons("white", border.alpha = 0.5) +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 1.75) +
  tm_layout(frame = F, 
            legend.position = c(0.9,0.5)) +
  tm_shape(vector_data_sf %>% 
             filter(genus %in% c("Ambyloma", "Hyalomma", "Ornithodoros",
                                 "Reticulinasus", "Rhicephalus", "Tick",
                                 "Argas", "Boophilus"))) +
  tm_dots(size = 0.2, 
          border.alpha = 0.05, 
          col = "species",
          palette = "viridis", 
          title = "Tick vectors") +
  tm_compass(type = "4star", 
             size = 1, 
             position = c("left", "top"))



# Pathogen data: rift valley fever had the highest case occurence data
# Reservoir and host data: livestock and mosquitoes had the highest occurence data
# As such, ENM will be conducted for only RVF


# Calculating prevalence or seroprevalence for RVF 
rvf_prev_sf <- pathogen_data_sf %>% 
  filter(pathogen_tested == "Rift_valley_fever") %>% 
  filter(!is.na(sample_size)) %>% 
  filter(sample_size %ni% c(0,1)) %>% 
  mutate(
    any_pos = coalesce(num_pos_pcr, num_pos_ser),
    any_prev = round(any_pos/sample_size*100,2))


# Rift valley pathogen data
rvf_map <-
  tm_shape(sen_adm0) +
  tm_polygons("white") +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 1.75) +
  tm_layout(frame = F, 
            legend.position = c(0.7,0.8),
            legend.title.size = 1.5,
            legend.text.size = 1,
            main.title = "Rift valley fever cases") +
  tm_shape(pathogen_data_sf %>% 
             filter(pathogen_tested == "Rift_valley_fever")) +
  tm_dots(size = 0.2,
          col = "population_tested",
          palette = c("#FF0303FE", "#0D0DFFFE", "#00FA00FE"),
          title = "Population tested") +
  tm_compass(type = "4star", 
             size = 3, 
             position = c("left", "top"))


# Rift valley prevalence data
prev_map <- 
  tm_shape(sen_adm0) +
  tm_polygons("white") +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 1.75) +
  tm_layout(frame = F, 
            legend.position = c(0.7,0.8),
            legend.title.size = 1.5,
            legend.text.size = 1,
            main.title = "RVF Prevalence") +
  tm_shape(rvf_prev_sf) +
  tm_dots(size = 0.2,
          col = "any_prev",
          palette = "viridis")
prev_map


# Rift valley host data
host_map <- 
  tm_shape(sen_adm0) +
  tm_polygons("white") +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 1.75) +
  tm_layout(frame = F, 
            legend.position = c(0.7,0.8),
            legend.title.size = 1.5,
            legend.text.size = 1,
            main.title = "Livestock occurences") +
  tm_shape(reservoir_data_sf %>% 
             filter(common_name %in% c("Cattle", "Goat", "Sheep", "Small_ruminants"))) +
  tm_dots(size = 0.2,
          col = "common_name",
          palette = c("#A200FFFE", "#F7B900FE", "#16C6CCFC", "#3B820CFE"),
          title = "Host") +
  tm_compass(type = "4star", 
             size = 3, 
             position = c("left", "top"))


# Rift valley vector data
vector_map <-
  tm_shape(sen_adm0) +
  tm_polygons("white") +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 1.75) +
  tm_layout(frame = F, 
            legend.position = c(0.8,0.8),
            legend.title.size = 1.5,
            legend.text.size = 1,
            main.title = "Vector occurences") +
  tm_shape(vector_data_sf %>% 
             filter(genus %in% c("Aedes", "Culex"))) +
  tm_dots(size = 0.2,
          col = "genus",
          palette = c("#289E86FE", "#A8102FFE"),
          title = "Vector") +
  tm_compass(type = "4star", 
             size = 3, 
             position = c("left", "top"))


# Arrange maps
tmap_arrange(rvf_map, host_map, vector_map)


# Density maps (indicator of geographical sampling bias)


# Livestock
rvf_case_density <- pathogen_data_summaries %>% 
  filter(pathogen_tested == "Rift_valley_fever")

ggplot(sen_adm0) +
  geom_sf(colour = "black", fill = "transparent") +
  xlab("Longitude") +
  ylab("Latitude") +
  stat_density2d(aes(x = gps_longitude, y = gps_latitude, fill = after_stat(level)),
                 geom = "polygon",
                 contour_var = "ndensity",
                 h = 0.5,
                 n = 100, 
                 alpha = 1,
                 bins = 10,
                 na.rm = T,
                 data = rvf_case_density) +
  geom_point(aes(x = gps_longitude, y = gps_latitude), 
             size = 0.75,
             data = rvf_case_density) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_minimal()


# Livestock
livestock_density <- reservoir_data_summaries %>% 
  filter(common_name %in% c("Cattle", "Goat", "Sheep", "Small_ruminant", "Small_ruminants"))

ggplot(sen_adm0) +
  geom_sf(colour = "black", fill = "transparent") +
  xlab("Longitude") +
  ylab("Latitude") +
  stat_density2d(aes(x = gps_longitude, y = gps_latitude, fill = after_stat(level)),
                 geom = "polygon",
                 contour_var = "ndensity",
                 h = 0.5,
                 n = 100, 
                 alpha = 1,
                 bins = 10,
                 na.rm = T,
                 data = livestock_density) +
  geom_point(aes(x = gps_longitude, y = gps_latitude), 
             size = 0.75,
             data = livestock_density) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_minimal()


# Rodents
rodent_density <- reservoir_data_summaries %>% 
  filter(genus %in% c("Arvicanthis", "Cricetomys", "Crocidura", "Mastomys",
                      "Mus", "Mus_nannomys", "Rattus", "Praomys", "Steatomys",
                      "Taterillus", "Thryonomys", "Uranomys"))

ggplot(sen_adm0) +
  geom_sf(colour = "black", fill = "transparent") +
  xlab("Longitude") +
  ylab("Latitude") +
  stat_density2d(aes(x = gps_longitude, y = gps_latitude, fill = after_stat(level)),
                 geom = "polygon",
                 contour_var = "ndensity",
                 h = 0.5,
                 n = 100, 
                 alpha = 1,
                 bins = 10,
                 na.rm = T,
                 data = rodent_density) +
  geom_point(aes(x = gps_longitude, y = gps_latitude), 
             size = 0.75,
             data = rodent_density) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_minimal()


# Mosquitos
mosquito_density <- vector_data_summaries %>% 
  filter(genus %in% c("Aedes", "Culex"))

ggplot(sen_adm0) +
  geom_sf(colour = "black", fill = "transparent") +
  xlab("Longitude") +
  ylab("Latitude") +
  stat_density2d(aes(x = gps_longitude, y = gps_latitude, fill = after_stat(level)),
                 geom = "polygon",
                 contour_var = "ndensity",
                 h = 0.75,
                 n = 100, 
                 alpha = 1,
                 bins = 10,
                 na.rm = T,
                 data = mosquito_density) +
  geom_point(aes(x = gps_longitude, y = gps_latitude), 
             size = 0.75,
             data = mosquito_density) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_minimal()





# Empres-i data ----

empres_rvf <- import(here("Working Data", "empresi_senegal_rvf_cases.csv"))

# clean up dataframe
empres_rvf <- empres_rvf %>% 
  dplyr::filter(`Diagnosis Status` == "Confirmed") %>% 
  dplyr::select(Disease, latitude, longitude, Country, Species, `Diagnosis Source`, `Diagnosis Status`)

# convert to spatial object
empres_rvf_sf <- empres_rvf %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# plot cases
tm_shape(sen_adm0) +
  tm_polygons("white", 
              border.alpha = 0.75) +
  tm_shape(gmb_adm0) +
  tm_polygons("white", 
              border.col = "black", 
              lwd = 2) +
  tm_layout(frame = F, 
            legend.position = c(0.8,0.6), 
            legend.height = 0.4) +
  tm_shape(empres_rvf_sf) +
  tm_dots(size = 0.1, 
          border.alpha = 0.05,
          palette = "viridis",
          title = "Pathogen")


