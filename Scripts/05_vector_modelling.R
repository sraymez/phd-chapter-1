## Modelling



# Environment set up ----



# Libraries
{library(pacman)
p_load(rio, here, tmap, sf, raster, terra, geodata, MODIStsp, tidyverse)}


# Working Data
{pathogen_data_summaries <- import(here("Working Data", "pathogen_data_complete_coords.csv"))
reservoir_data_summaries <- import(here("Working Data", "reservoir_data_complete_coords.csv"))
vector_data_summaries <- import(here("Working Data", "vector_data_complete_coords.csv"))}


# Shapefile data
{sen_adm0 <- st_read(here("Shapefiles", "gadm41_SEN_0.shp"))
gmb_adm0 <- st_read(here("Shapefiles", "gadm41_GMB_0.shp"))}


# Create master shapefile for both countries 
sen_gmb_ext <- st_union(sen_adm0, gmb_adm0)



# Download raster data with geodata package ----



# Worldclim data - stored as multi-layer SpatRaster
sen_gmb_wc <- worldclim_country(var = "bio", # get all 19 bioclim variables
                                res = 0.5, # 0.5 arc sec is ~1km
                                country = "SEN", # spatial extent - also includes gambia
                                path = "rasters")


# Crop and then mask to extent of Senegal and Gambia
sen_gmb_wc <- crop(sen_gmb_wc, sen_gmb_ext)
sen_gmb_wc <- mask(sen_gmb_wc, sen_gmb_ext)

tm_shape(sen_gmb_wc[[1]]) +
  tm_raster(style = "cont", palette = "Spectral", title = "Annual mean temp (c)") +
  tm_layout(frame = F, legend.outside = T) +
  tm_shape(gmb_adm0) +
  tm_borders(lwd = 2)


# Population density
world_pop_dens <- population(year = 2020,
                             res = 0.5, # ~1km
                             path = "rasters")


# Crop and then mask to extent of Senegal and Gambia
pop_dens_ext <- crop(world_pop_dens, sen_gmb_ext)
pop_dens_ext <- mask(pop_dens_ext, sen_gmb_ext)

# Log transform values for easier visualisation 
pop_dens_ext_log <- log(pop_dens_ext + 1) # +1 handles 0 values

tm_shape(pop_dens_ext_log) +
  tm_raster(style = "cont", palette = "-Spectral") +
  tm_layout(frame = F)

# Log transform population density...?




# Download raster data from WorldPop ----



# Water bodies
sen_dst_water <- raster("Rasters/World_pop/sen_esaccilc_dst_water_100m_2000_2012.tif")
gmb_dst_water <- raster("Rasters/World_pop/gmb_esaccilc_dst_water_100m_2000_2012.tif")


sen_dst_waterway <- raster("Rasters/World_pop/sen_osm_dst_waterway_100m_2016.tif")
gmb_dst_waterway <- raster("Rasters/World_pop/gmb_osm_dst_waterway_100m_2016.tif")


sen_dst_road <- raster("Rasters/World_pop/sen_osm_dst_road_100m_2016.tif")
gmb_dst_road <- raster("Rasters/World_pop/gmb_osm_dst_road_100m_2016.tif")


tm_shape(sen_dst_water) +
  tm_raster(style = "cont", palette = "-Spectral") +
  tm_shape(gmb_dst_water) +
  tm_raster(style = "cont", palette = "-Spectral") +
  tm_layout(frame = F)


tm_shape(sen_dst_waterway) +
  tm_raster(style = "cont", palette = "Spectral") +
  tm_shape(gmb_dst_waterway) +
  tm_raster(style = "cont", palette = "Spectral") +
  tm_layout(frame = F)


tm_shape(sen_dst_road) +
  tm_raster(style = "cont", palette = "Spectral") +
  tm_shape(gmb_dst_road) +
  tm_raster(style = "cont", palette = "Spectral") +
  tm_layout(frame = F)



# Download MODIS land cover with MODIStsp package -----



# Get bounding box
st_bbox(sen_gmb_ext)

# Specify land cover modis download
MODIStsp(gui = FALSE,
         out_folder = "Rasters/MODIS/Master",
         out_folder_mod = "Rasters/MODIS/Master",
         selprod = "LandCover_Type_Yearly_500m (MCD12Q1)",
         bandsel = "LC1",
         sensor = "Terra",
         user = "santirg",
         password = "Sg09193Sg09193",
         start_date = "2021.12.31",
         end_date = "2022.12.31",
         verbose = TRUE,
         bbox = c(-17.54319, 12.30786, -11.34247, 16.69207),
         spatmeth = "bbox",
         out_format = "GTiff",
         compress = "LZW",
         out_projsel = "User Defined",
         output_proj = ": +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
         e_hdf = T,
         lel = T)

# Import data
landcover_files_ext <- list.files("Rasters/MODIS/Master/LandCover_Type_Yearly_500m_v61/LC1", "MCD12Q1_LC1_2022_001", full.names = T)
landcover_ext <- terra::rast(landcover_files_ext)
names(landcover_ext) <- 'landcover_ext'

# Crop raster to spatial extent of Gambia shapefile
landcover_ext <- mask(landcover_ext, sen_gmb_ext)

# Numeric range of values
range(landcover_ext)

# Plot raster to see values of landcover - use style = "cat" to see individual values that are present
tm_shape(landcover_ext) +
  tm_raster(style = "cat") +
  tm_layout(frame = F, legend.position = c(0,0.5))

# Rename land cover classifications to categorical 
temp_lc <- 
  as.data.frame(
    cbind(
      landcover_breaks = c(2,4,5,7,8,9,10,11,12,13,14,16,17),
      landcover_classes = c("Evergreen Broadleaf Forests", "Deciduous Broadleaf Forests", 
                            "Mixed Forests", "Open Shrublands", "Woody Savannas", "Savannas", 
                            "Grasslands", "Permanent wetlands", "Croplands", "Urban and Built-up land",
                            "Cropland/Natural Vegetation Mosaic", "Barren land", "Water bodies"))) %>% 
  mutate(landcover_breaks = as.numeric(landcover_breaks))

# Assign landcover classifications to numeric values
levels(landcover_ext) <- temp_lc

# Plot landcover raster
tm_shape(landcover_ext) +
  tm_raster(palette = "Spectral", title = "Land cover categories") +
  tm_layout(frame = F, legend.outside = T) +
  tm_shape(gmb_adm0) +
  tm_borders(lwd = 2)




# Vegetation index



# Spatial data processing ----












# Grid over senegal
cellsize_deg <- (1/111)*5 # one degree is ~111km, so 1km in degrees is 1/111

sen_grid <- st_make_grid(sen_adm0, cellsize = cellsize_deg, what = "centers")

sen_grid_sf <- sen_grid %>% 
  st_as_sf() %>% 
  st_join(sen_adm0, left = F)

vector_data_sf <- vector_data_summaries %>% 
  filter(!is.na(gps_latitude)) %>% # remove location names without coordinates
  st_as_sf(coords = c("gps_longitude", "gps_latitude"), crs = 4326)

tm_shape(sen_adm0) +
  tm_polygons("white") +
  tm_shape(sen_grid_sf) +
  tm_dots(col = "red3") +
  tm_shape(vector_data_sf %>% 
             filter(genus %in% c("Aedes", "Culex", "Mansonia", "Anopheles"))) +
  tm_dots(size = 0.25)