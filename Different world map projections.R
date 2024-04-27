# Header ------------------------------------------------------------------
#
# Script name: Different world map projections
# Script description: Exploring different world map projections using st_transform
#
# Author: Justin Tran
#
# Notes:
# The origins of this script was trying to convert WGS84 world maps to other projections
# I was originally using ggplot's world map which was a bitch to transform
# Naturalearth's map of the world worked instantly

# Dependencies ------------------------------------------------------------

if (!require("pacman"))
  install.packages("pacman")
# Load packages (CRAN)
pacman::p_load(tidyverse, sf, rnaturalearth, rnaturalearthdata)

# @@ SCRIPT @@ ------------------------------------------------------------

# Load world map
world_map <- ne_countries(scale = "medium", returnclass = "sf")
# The CRS is WGS84
st_crs(world_map)

world_map |> 
  ggplot(aes(fill = type))+
  geom_sf()
  
# Eckhert V
world_map |> 
  st_transform("ESRI:54011") |> 
  ggplot(aes(fill = type))+
  geom_sf()

# Cassini
world_map |> 
  st_transform("ESRI:53028") |> 
  ggplot(aes(fill = type))+
  geom_sf()
# Cassini too wacky to work properly.

# Wagner VII
world_map |> 
  st_transform("ESRI:54076") |> 
  ggplot(aes(fill = type))+
  geom_sf()

# Australian Antarctic Lambert
world_map |> 
  st_transform("EPSG:3033") |> 
  ggplot(aes(fill = type))+
  geom_sf()
# Some distortion is present, but it's in the Northern Hemisphere
# This projection is just for one hemisphere so working as intended I suppose.
# Need to crop to just the southern hemisphere to avoid seeing the northern hemisphere distortions

# Fuller
# world_map |> 
#   st_transform("ESRI:54050") |> 
#   ggplot(aes(fill = type))+
#   geom_sf()
# Sadly doesn't work. st_transform doesn't play nice with weird projections.

# Peirce
world_map |> 
  st_transform("ESRI:54091") |> 
  ggplot(aes(fill = type))+
  geom_sf()