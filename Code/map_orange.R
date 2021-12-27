library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)

source('C:/Users/djr47/Desktop/Maps/map_functions_and_theme.R')

## Get Data =============================================================

Center_Orange <- c(33.7878865190379, -117.85312166303487)

bb_Orange <- make_square(Center_Orange, 0.15) %>% bb_box()

roads <- bb_Orange %>%
  opq() %>%
  add_osm_feature(key = 'highway', 
                  value = c('motorway','primary','secondary',
                            'tertiary','construction')) %>%
  osmdata_sf() 

roundabout <- bb_Orange %>%
  opq() %>%
  add_osm_feature(key = 'junction', 
                  value = 'roundabout') %>%
  osmdata_sf()

park <- bb_Orange %>%
  opq() %>%
  add_osm_feature(key = 'leisure', 
                  value = 'park') %>%
  osmdata_sf()

grass <- bb_Orange %>%
  opq() %>%
  add_osm_feature(key = 'landuse', value = 'grass') %>%
  osmdata_sf()

fountain <- bb_Orange %>%
  opq() %>%
  add_osm_feature(key = 'amenity', value = 'fountain') %>%
  osmdata_sf()

small_streets <- bb_Orange %>%
  opq() %>%
  add_osm_feature(key = 'highway', 
                  value = c("residential", "living_street",
                            "unclassified")) %>%
  osmdata_sf() 

#sidewalks <- bb_Orange %>%
#  opq() %>%
#  add_osm_feature(key = 'footway',
#                  value = 'sidewalk') %>%
#  osmdata_sf()

river <- bb_Orange %>% 
  opq() %>%
  add_osm_feature(key = "waterway", 
                  value = c('river','stream','riverbank','dam','waterfall')) %>%
  osmdata_sf()

buildings <- bb_Orange %>%
  opq() %>%
  add_osm_feature(key = 'building',
                  value = c('yes','church')) %>%
  osmdata_sf()

parking <- bb_Orange %>%
  opq() %>%
  add_osm_feature(key = 'amenity', 
                  value = 'parking') %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = river$osm_lines,
          fill = water_color_dark,
          colour = water_color_dark) + 
  geom_sf(data = parking$osm_polygons, 
          fill = 'grey90', alpha = 0.2) +
  geom_sf(data = roads$osm_lines,
          color = street_color_light,
          alpha=.4,
          size = 1.25) +
  geom_sf(data = roundabout$osm_polygons,
          color = street_color_light,
          alpha=.4,
          size = 1.25,
          fill = NA) +
  geom_sf(data = small_streets$osm_lines,
          color = small_street_color_light,
          alpha=.2,
          size = 1) +
  #geom_sf(data = sidewalks$osm_lines,
  #        color = small_street_color_light,
  #        alpha=.2,
  #        size = .4) +
  geom_sf(data = buildings$osm_polygons, 
          fill = 'grey80', alpha = 0.2) +
  geom_sf(data = grass$osm_polygons, 
          fill = 'grey80', alpha = 0.2) +
  geom_sf(data = fountain$osm_polygons, 
          fill = 'grey80', alpha = 0.2) +
  coord_sf(xlim = c(bb_Orange[1,1], bb_Orange[1,2]),
           ylim = c(bb_Orange[2,1], bb_Orange[2,2]),
           expand = FALSE) +
  theme_void() +
  theme_light +
  labs(title = "ORANGE, CALIFORNIA")
  

