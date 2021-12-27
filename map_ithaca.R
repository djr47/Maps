library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)

# https://www.vistaprint.com/photo-gifts/canvas-prints?GP=11%2f15%2f2021+17%3a26%3a36&GPS=6104513678&GNF=1
# 12x12 stretched canvas

## Get coordinate functions and theme
source('C:/Users/djr47/Desktop/Maps/map_functions_and_theme.R')

## Get Ithaca Data =============================================================

Center_Ithaca <- c(42.450699794608816, -76.49747046824697)
#Center_Ithaca <- c(42.439612559020446, -76.49682217904257)

bb_Ithaca <- make_square(Center_Ithaca, 1.5) %>% bb_box()

bridge <- bb_Ithaca %>%
  opq() %>%
  add_osm_feature(key = 'bridge',
                  value = 'yes') %>%
  osmdata_sf()

paths <- bb_Ithaca %>%
  opq() %>%
  add_osm_feature(key = 'highway',
                  value = c('footway',
                            'cycleway')) %>%
  osmdata_sf()

river <- bb_Ithaca %>% 
  opq() %>%
  add_osm_feature(key = "waterway", 
                  value = c('river',
                            'stream',
                            'riverbank',
                            'dam',
                            'waterfall')) %>%
  osmdata_sf()

water <- bb_Ithaca %>% 
  opq() %>%
  add_osm_feature(key = 'natural',
                  value = 'water') %>%
  osmdata_sf()

lake <- bb_Ithaca %>% 
  opq() %>%
  add_osm_feature(key = 'water', 
                  value = c('lake',
                            'pond',
                            'reservoir',
                            'river',
                            'stream',
                            'oxbow',
                            'basin',
                            'stream_pool')) %>%
  osmdata_sf()

reservoir1 <- bb_Ithaca %>%
  opq() %>%
  add_osm_feature(key = 'natural',
                  value = 'water') %>%
  osmdata_sf()

reservoir2 <- bb_Ithaca %>%
  opq() %>%
  add_osm_feature(key = 'water', 
                  value = 'reservoir') %>%
  osmdata_sf()

buildings <- bb_Ithaca %>%
  opq() %>%
  add_osm_feature(key = 'building',
                  value = c('yes',
                            'church',
                            'university',
                            'detached',
                            'house',
                            'dormitory',
                            'apartments',
                            'school',
                            'roof',
                            'hospital')) %>%
  osmdata_sf()

st_catherine <- bb_Ithaca %>%
  opq() %>%
  add_osm_feature(key = 'amenity',
                  value = 'place_of_worship') %>%
  osmdata_sf()


## Plot Ithaca =================================================================



ggplot() +
  geom_sf(data = reservoir1$osm_polygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = river$osm_lines,
          fill = water_color_dark,
          colour = water_color_dark) + 
  geom_sf(data = water$osm_multipolygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = buildings$osm_polygons, 
          fill = street_color_dark,
          alpha = 0.6,
          colour = NA) +
  geom_sf(data = st_catherine$osm_polygons, 
          fill = street_color_dark,
          alpha = 0.6,
          colour = NA) +
  coord_sf(xlim = c(bb_Ithaca[1,1], bb_Ithaca[1,2]),
           ylim = c(bb_Ithaca[2,1], bb_Ithaca[2,2]),
           expand = FALSE) +
  theme_void() +
  theme_dark +
  labs(title = "ithaca")


ggsave('ithaca.png', width = 7, height = 7, units = 'in')
