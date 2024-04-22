## Modelling vectors of rift valley fever



# Environment set up ----



# Libraries
{library(pacman)
p_load(rio, here, tmap, sf, raster, terra, geodata, MODIStsp, spThin, dismo, rJava, usdm, tidyverse)}


# Occurrence records
{vector_data_summaries <- import(here("Working Data", "vector_data_complete_coords.csv"))
gbif_vector_occ <- st_read(here("Working Data", "gbif_vector_records.shp"))}

# Shapefile data
{sen_adm0 <- st_read(here("Shapefiles", "gadm41_SEN_0.shp"))
gmb_adm0 <- st_read(here("Shapefiles", "gadm41_GMB_0.shp"))}


# Create master shapefile for both countries 
sen_gmb_ext <- st_union(sen_adm0, gmb_adm0)



# Processing occurrence records ----


# Subset to just Aedes and Culex, select relevant columns and remove missing gps
rvf_vector_lit <- vector_data_summaries %>% 
  filter(genus %in% c("Aedes", "Culex")) %>% 
  select(genus, gps_latitude, gps_longitude) %>% 
  relocate(gps_longitude, .before = gps_latitude) %>% 
  filter(!is.na(gps_longitude))


# Subset gbif occurrences and convert back to data frame
rvf_vector_gbif <- gbif_vector_occ %>%
  filter(Vector %in% c("Aedes", "Culex")) %>% 
  select(Vector, geometry) %>% 
  rename(genus = Vector) %>%
  mutate(gps_longitude = st_coordinates(.)[,1],
         gps_latitude = st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL) 


# Bind data together 
rvf_vectors <- bind_rows(rvf_vector_lit, rvf_vector_gbif)


# Quick plot of occurrences to make sure they are all within Senegal-Gambia extent
ggplot() +
  geom_sf(data = sen_gmb_ext, fill = "white") +
  geom_point(data = rvf_vectors,
             aes(x = gps_longitude,
                 y = gps_latitude,
                 colour = genus),
             size = 1) +
  theme_minimal()


# Remove some objects from env to keep minimal
rm(gmb_adm0, sen_adm0, gbif_vector_occ, rvf_vector_gbif, rvf_vector_lit, vector_data_summaries)


# Remove duplicated coordinates
rvf_vectors_distinct <- rvf_vectors %>% distinct(genus, gps_longitude, gps_latitude)


# Spatial thinning 
# Need consider cause for sampling bias: due to biased research efforts or reflection of truly suitable habitat 
# Flight range estimates for Aedes/Culex - by thinning at the limit of flight ranges, we are adjusting for spatial autocorrelation

# Kernel density estimation
ggplot(sen_gmb_ext) +
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
                 data = rvf_vectors_distinct) +
  geom_point(aes(x = gps_longitude, y = gps_latitude), 
             size = 0.75,
             data = rvf_vectors_distinct) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_minimal()


# 5km thinning based on maximum flight distances observed for Aedes and Culex mosquitos
thinning_occ <- rvf_vectors_distinct %>% 
  mutate(species = "rvf_vectors") %>% 
  select(-c(genus)) %>% 
  thin(lat.col = "gps_latitude",
       long.col = "gps_longitude",
       spec.col = "species",
       thin.par = 5,
       reps = 100,
       write.files = F,
       locs.thinned.list.return = T
       )


# Select the dataframe from the thinned list with the maximum number of rows
max_thinned_df <- thinning_occ[[which.max(sapply(thinning_occ, nrow))]]


# Merge original data with thinned data keeping records of original that are in max thinned
rvf_vectors_thinned <- rvf_vectors_distinct %>% 
  inner_join(max_thinned_df, by = c("gps_longitude" = "Longitude", 
                                    "gps_latitude"= "Latitude"))


# Create presence/absence variable 
rvf_vectors_thinned <- rvf_vectors_thinned %>% 
  mutate(presence = 1)


# Create spatial points polygon
rvf_vectors_spp <- rvf_vectors_thinned %>% 
  st_as_sf(coords = c("gps_longitude", "gps_latitude"), crs = 4326) %>% 
  as_Spatial()


# Calculate a buffer, dissolving overlapping polygon boundaries
presence_buffer <- raster::buffer(rvf_vectors_spp, width = 500000, dissolve = TRUE)


# Create extent of study region 
bg_ext <- raster::extent(sen_gmb_wc_rast)

# Crop 
bg_crop <- raster::crop(x = sen_gmb_wc, y = bg_ext)

# Mask
bg_mask <- raster::mask(x = bg_crop, mask = presence_buffer)

#
bg_pts <- dismo::randomPoints(sen_gmb_wc, 
                              n = 10000,
                              ext = sen_gmb_ext,
                              mask = T)



sen_gmb_wc_stk <- stack(sen_gmb_wc)

raster_mask <- raster(extent(sen_gmb_ext), resolution = resolution(sen_gmb_wc))

raster_mask <- rasterize(sen_gmb_ext, raster_mask, field = 1)

bg_pts <- dismo::randomPoints(sen_gmb_wc_stk,
                              n = 10000,
                              mask = raster_mask)



 
# Download raster covariates for vector modelling ----

# Covariates to be included in initial variable screening:
# Temperature
# Precipitation
# Water bodies
# Land cover



# Worldclim variables ----


# Download all bioclim variables - stored as multi-layer SpatRaster
sen_gmb_wc <- geodata::worldclim_country(
  var = "bio", # get all 19 bioclim variables
  res = 0.5, # 0.5 arc sec is ~1km
  country = "SEN", # spatial extent - also includes gambia
  path = "rasters")


# Crop and then mask to extent of Senegal and Gambia
sen_gmb_wc <- crop(sen_gmb_wc, sen_gmb_ext)
sen_gmb_wc <- mask(sen_gmb_wc, sen_gmb_ext)


# plot all bioclim
bio1_val <- sen_gmb_wc[[1]]
bio1_val <- values(bio1_val)

bio2_val <- sen_gmb_wc[[2]]
bio2_val <- values(bio2_val)

df <- data.frame(Value1 = bio1_val, Value2 = bio2_val)
df <- na.omit(df)

df_sub <- df %>% sample_n(size = 10000)
  
  
ggplot(data = df,
       aes(x = wc2.1_30s_bio_1, 
           y = wc2.1_30s_bio_2)) +
  geom_point()

plot(sen_gmb_wc[[1]], sen_gmb_wc[[2]])


# Plot single layer
tm_shape(sen_gmb_wc[[1]]) +
  tm_raster(style = "cont", palette = "Spectral", title = "Annual mean temp (c)") +
  tm_layout(frame = F, legend.outside = T) +
  tm_shape(gmb_adm0) +
  tm_borders(lwd = 2)



# WorldPop variables ----


# Water bodies
sen_dst_water <- raster("Rasters/World_pop/sen_esaccilc_dst_water_100m_2000_2012.tif")
gmb_dst_water <- raster("Rasters/World_pop/gmb_esaccilc_dst_water_100m_2000_2012.tif")


# WorldPop - waterways
sen_dst_waterway <- raster("Rasters/World_pop/sen_osm_dst_waterway_100m_2016.tif")
gmb_dst_waterway <- raster("Rasters/World_pop/gmb_osm_dst_waterway_100m_2016.tif")


# Plot layer
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


# MODIS land cover variables ----



# Get bounding box for region of interest
st_bbox(sen_gmb_ext)

# Download MODIS land cover data
MODIStsp::MODIStsp(
  gui = FALSE,
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


# Crop raster to spatial extent of Senegal-Gambia
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



# Spatial data processing ----







# Population density
# world_pop_dens <- geodata::population(
#   year = 2020,
#   res = 0.5, # ~1km
#   path = "rasters")
# 
# 
# # Crop and then mask to extent of Senegal and Gambia
# pop_dens_ext <- crop(world_pop_dens, sen_gmb_ext)
# pop_dens_ext <- mask(pop_dens_ext, sen_gmb_ext)
# 
# # Log transform values for easier visualisation 
# pop_dens_ext_log <- log(pop_dens_ext + 1) # +1 handles 0 values
# 
# tm_shape(pop_dens_ext_log) +
#   tm_raster(style = "cont", palette = "-Spectral") +
#   tm_layout(frame = F)