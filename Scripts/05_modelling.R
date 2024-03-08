## Modelling



# Environment set up ----



# Libraries
{library(pacman)
p_load(rio, here, sf, tidyverse)}


# Working Data
{pathogen_data <- import(here("Working Data", "tbd.csv"))
reservoir_data <- import(here("Working Data", "tbd.csv"))
vector_data <- import(here("Working Data", "tbd.csv"))}


# Shapefile data
{sen_adm0 <- st_read(here("Shapefiles", "gadm41_SEN_0.shp"))
gmb_adm0 <- st_read(here("Shapefiles", "gadm41_GMB_0.shp"))}





