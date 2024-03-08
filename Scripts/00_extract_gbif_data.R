## GBIF Extraction



# Environment set up ----



# Packages ----
if (!require("pacman")) install.packages("pacman")
pkgs = c("rgbif", "vroom", "usethis", "tmap", "sf", "tidyverse")
pacman::p_load(pkgs, character.only = T)


# GBIF downloads ----

# Provide gbif credentials
#usethis::edit_r_environ() # set username, password and email


# Vectors occurence data
myVectors <- c("Aedes aegypti", "Aedes taylori", "Aedes furcifer", "Aedes dalzieli", "Aedes luteocephalus", 
               "Aedes africanus", "Aedes metallicus", "Aedes vexans", "Aedes vittatus", "Culex neavei",
               "Culex poicilipes", "Culex antennatus", "Culex perfuscus", "Culex quinquefasciatus",
               "Culex tritaeniorhynchus","Anopheles coustani","Mansonia uniformis", "Mansonia africana")


gbif_vec_gm <- occ_data(scientificName = myVectors, hasCoordinate = TRUE, country = "GM")
gbif_vec_sn <- occ_data(scientificName = myVectors, hasCoordinate = TRUE, country = "SN")


# method 2 - streamlined
genus_aedes <- "Aedes"
genus_culex <- "Culex"
#genus_glossina <- "Glossina"
order_ixodida <- "Ixodida"


backbone_res_aedes <- name_backbone(name = genus_aedes)
backbone_res_culex <- name_backbone(name = genus_culex)
#backbone_res_glossina <- name_backbone(name = genus_glossina)
backbone_res_ixodida <- name_backbone(name = order_ixodida)


aedes_key <- backbone_res_aedes$genusKey
culex_key <- backbone_res_culex$genusKey
#glossina_key <- backbone_res_glossina$
ixodida_key <- backbone_res_ixodida$orderKey


gbif_aedes_occ <- occ_search(taxonKey = aedes_key, country = c("GM", "SN"), hasCoordinate = T) 
gbif_culex_occ <- occ_search(taxonKey = culex_key, country = c("GM", "SN"), hasCoordinate = T) 
gbif_ixodida_occ <- occ_search(taxonKey = ixodida_key, country = c("GM", "SN"), hasCoordinate = T)


# Reservoir occurence data
order_rodentia <- "Rodentia"
genus_bos <- "Bos" # cows
genus_capra <- "Capra" # goats
genus_ovis <- "Ovis" # sheep
order_bat <- "Chiroptera"


backbone_res_rodentia <- name_backbone(name = order_rodentia)
backbone_res_bos <- name_backbone(name = genus_bos)
backbone_res_capra <- name_backbone(name = genus_capra)
backbone_res_ovis <- name_backbone(name = genus_ovis)
backbone_res_bat <- name_backbone(name = order_bat)


rodentia_key <- backbone_res_rodentia$orderKey
bos_key <- backbone_res_bos$genusKey
capra_key <- backbone_res_capra$genusKey
ovis_key <- backbone_res_ovis$genusKey
bat_key <- backbone_res_bat$orderKey


gbif_occ_rod <- occ_search(taxonKey = rodentia_key, country = c("GM", "SN"), hasCoordinate = T)
gbif_occ_bos <- occ_search(taxonKey = bos_key, country = c("GM", "SN"), hasCoordinate = T)
gbif_occ_capra <- occ_search(taxonKey = capra_key, country = c("GM", "SN"), hasCoordinate = T)
gbif_occ_ovis <- occ_search(taxonKey = ovis_key, country = c("GM", "SN"), hasCoordinate = T)
gbif_occ_bat <- occ_search(taxonKey = bat_key, country = c("GM", "SN"), hasCoordinate = T)


# Shapefile data ----


# Country borders
gmAdm1 <- st_read(here("Shapefiles", "gmb_admbnda_adm1_ndma_20220901.shp"))
snAdm1 <- st_read(here("Shapefiles", "gadm41_SEN_1.shp"))


# Create data frame from gbif files ----


# Reservoirs

  rodent_data_gm <- as.data.frame(gbif_occ_rod$GM$data)
  rodent_data_sn <- as.data.frame(gbif_occ_rod$SN$data)

  cow_data_gm <- as.data.frame(gbif_occ_bos$GM$data)
  cow_data_sn <- as.data.frame(gbif_occ_bos$SN$data)
  
  goat_data_gm <- as.data.frame(gbif_occ_capra$GM$data)
  goat_data_sn <- as.data.frame(gbif_occ_capra$SN$data)
  
  sheep_data_gm <- as.data.frame(gbif_occ_ovis$GM$data) # 0 obs
  sheep_data_sn <- as.data.frame(gbif_occ_ovis$SN$data) # 0 obs
  
  bats_data_gm <- as.data.frame(gbif_occ_bat$GM$data)
  bats_data_sn <- as.data.frame(gbif_occ_bat$SN$data)

# Vectors
  
  aedes_data_gm <- as.data.frame(gbif_aedes_occ$GM$data)
  aedes_data_sn <- as.data.frame(gbif_aedes_occ$SN$data)

  culex_data_gm <- as.data.frame(gbif_culex_occ$GM$data)
  culex_data_sn <- as.data.frame(gbif_culex_occ$SN$data)
  
  tick_data_gm <- as.data.frame(gbif_ixodida_occ$GM$data)
  tick_data_sn <- as.data.frame(gbif_ixodida_occ$SN$data)

    
# Select relevant cols ----
colsFnc <- function(data) {data %>% select(key, scientificName, decimalLatitude, decimalLongitude, country)}

  
# Reservoirs

  rodent_data_gm <- rodent_data_gm %>% colsFnc()
  rodent_data_sn <- rodent_data_sn %>% colsFnc()

  cow_data_gm <- cow_data_gm %>% colsFnc()
  cow_data_sn <- cow_data_sn %>% colsFnc()
  
  goat_data_gm <- goat_data_gm %>% colsFnc()
  goat_data_sn <- goat_data_sn %>% colsFnc()
  
  bats_data_gm <- bats_data_gm %>% colsFnc()
  bats_data_sn <- bats_data_sn %>% colsFnc()

  
# Vectors
  
  aedes_data_gm <- aedes_data_gm %>% colsFnc()
  aedes_data_sn <- aedes_data_sn %>% colsFnc()
  
  culex_data_gm <- culex_data_gm %>% colsFnc()
  culex_data_sn <- culex_data_sn %>% colsFnc()
  
  tick_data_gm <- tick_data_gm %>% colsFnc()
  tick_data_sn <- tick_data_sn %>% colsFnc()
  
  
# Bind gambia and senegal data together -----

  
# Reservoir
  
  rodent_data <- bind_rows(rodent_data_gm, rodent_data_sn)
  cow_data <- bind_rows(cow_data_gm, cow_data_sn)
  goat_data <- bind_rows(goat_data_gm, goat_data_sn)
  bat_data <- bind_rows(bats_data_gm, bats_data_sn)

# Vector
  
  aedes_data <- bind_rows(aedes_data_gm, aedes_data_sn)
  culex_data <- bind_rows(culex_data_gm, culex_data_sn)
  tick_data <- bind_rows(tick_data_gm, tick_data_sn)
  
  
# Convert data frame to sf object ----

  
# Reservoir
  
  rodent_data_sf <- rodent_data %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
    mutate(Reservoir = "Rodent")
  cow_data_sf <- cow_data %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
    mutate(Reservoir = "Cattle")
  goat_data_sf <- goat_data %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
    mutate(Reservoir = "Goat")
  bats_data_sf <- bat_data %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
    mutate(Reservoir = "Bat")

  
# Vectors
  
  aedes_data_sf <- aedes_data %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
    mutate(Vector = "Aedes")
  culex_data_sf <- culex_data %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
    mutate(Vector = "Culex")
  tick_data_sf <- tick_data %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
    mutate(Vector = "Ticks")

  
# Bind all sf objects together 
reservoir_data_sf <- bind_rows(rodent_data_sf, cow_data_sf, goat_data_sf, bats_data_sf)
vector_data_sf <- bind_rows(aedes_data_sf, culex_data_sf, tick_data_sf)



# Plots ----



# Reservoir
tm_shape(snAdm1) +
  tm_polygons("white", border.alpha = 0.25) +
  tm_shape(gmAdm1) +
  tm_polygons("white", border.col = "black", lwd = 2) +
  tm_layout(frame = F, legend.position = c(0.8,0.6), legend.height = 0.4) +
  tm_shape(reservoir_data_sf) +
  tm_dots(size = .1, border.alpha = 0.05, col = "Reservoir", palette = c("darkblue", "green3", "darkred", "darkorange")) +
  tm_compass(type = "4star", size = 1, position = c("left", "top"))


# Vector
tm_shape(snAdm1) +
  tm_polygons("white", border.alpha = 0.25) +
  tm_shape(gmAdm1) +
  tm_polygons("white", border.col = "black", lwd = 2) +
  tm_layout(frame = F, legend.position = c(0.8, 0.6), legend.height = 0.4) +
  tm_shape(vector_data_sf) +
  tm_dots(size = .1, border.alpha = 0.05, col = "Vector", palette = c("darkblue", "green3", "darkorange")) +
  tm_compass(type = "4star", size = 1, position = c("left", "top"))



# End ----