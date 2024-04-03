## Spatial autocorrelation 



# Environment set up ----



# Libraries
{library(pacman)
p_load(rio, here, geoR, tmap, sf, tidyverse)}

library(gstat)


# Working Data
{pathogen_data_summaries <- import(here("Working Data", "pathogen_data_complete_coords.csv"))
reservoir_data_summaries <- import(here("Working Data", "reservoir_data_complete_coords.csv"))
vector_data_summaries <- import(here("Working Data", "vector_data_complete_coords.csv"))}


# Shapefile data
{sen_adm0 <- st_read(here("Shapefiles", "gadm41_SEN_0.shp"))
gmb_adm0 <- st_read(here("Shapefiles", "gadm41_GMB_0.shp"))}


# RVF prevalence
rvf_prev <- pathogen_data_summaries %>% 
  filter(pathogen_tested == "Rift_valley_fever") %>% 
  select(year, population_tested, sample_size, num_pos_pcr, num_pos_ser, gps_latitude, gps_longitude) %>% 
  filter(!is.na(sample_size)) %>% 
  filter(sample_size != 0) %>% 
  mutate(
    prev_pcr = round(num_pos_pcr/sample_size*100,2),
    prev_ser = round(num_pos_ser/sample_size*100,2),
    prev_oa = coalesce(prev_pcr, prev_ser))


rvf_prev_sf <- rvf_prev %>% 
  st_as_sf(coords = c("gps_longitude", "gps_latitude"), crs = 4326)



tm_shape(sen_adm0) +
  tm_polygons("white") +
  tm_shape(gmb_adm0) +
  tm_polygons("white") +
  tm_layout(frame = F, 
            legend.position = c(0.8,0.6), 
            legend.height = 0.4) +
  tm_shape(rvf_prev_sf) +
  tm_dots(size = 0.5, 
          col = "prev_oa",
          border.alpha = 0.05, 
          palette = "viridis",
          title = "Pathogen") +
  tm_compass(type = "4star", 
             size = 1, 
             position = c("left", "top"))



rvf_autocorr <- rvf_prev_sf %>% select(geometry)


vari <- ggvario(coords = st_coordinates(rvf_autocorr), data = rvf_prev_sf$prev_oa)

vario <- variogram(prev_oa ~ 1, data = rvf_prev_sf, cloud  = F)




