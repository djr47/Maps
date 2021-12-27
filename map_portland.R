library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)

## Get coordinate functions and theme
source('C:/Users/djr47/Desktop/Maps/map_functions_and_theme.R')## Get Data =============================================================

Center_Portland <- c(45.52313413609776, -122.66943162206007)

bb_Portland <- make_square(Center_Portland, 1.5) %>% bb_box()

river <- bb_Portland %>% 
  opq() %>%
  add_osm_feature(key = "waterway", 
                  value = c('river','stream','riverbank','dam','waterfall',
                            'tidal_channel')) %>%
  osmdata_sf()

bay <- bb_Portland %>% 
  opq() %>%
  add_osm_feature(key = 'natural', 
                  value = c('water', 'bay','strait')) %>%
  osmdata_sf()

islands <- bb_Portland %>%
  opq() %>%
  add_osm_feature(key = 'place', 
                  value = c('islet','island')) %>%
  osmdata_sf() 

buildings <- bb_Portland %>%
  opq() %>%
  add_osm_feature(key = 'building',
                  value = c(
                    'yes',
                    'commercial',
                    'civic',
                    'residential',
                    'service',
                    'garage',
                    'parking',
                    'courthouse',
                    'industrial',
                    'hotel',
                    'office',
                    'yes;public',
                    'church',
                    'university',
                    'detached',
                    'house',
                    'dormitory',
                    'apartments',
                    'school',
                    'roof',
                    'hospital',
                    'military',
                    'public',
                    'train_station',
                    'retail',
                    'government',
                    'yes',
                            'church',
                            'university',
                            'detached',
                            'apartments',
                            'residential',
                            'grandstand',
                            'hotel',
                            'hospital',
                            'school')) %>%
  osmdata_sf()

bridges <- bb_Portland %>%
  opq() %>%
  add_osm_feature(key = 'man_made',
                  value = 'bridge') %>%
  osmdata_sf()

## Plot  =================================================================



ggplot() +
  geom_sf(data = river$osm_multipolygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = river$osm_polygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = river$osm_lines,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = bay$osm_multipolygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = islands$osm_polygons,
          fill = background_color_dark,
          colour = NA, lty = 0) +
  geom_sf(data = bridges$osm_polygons,
          fill = background_color_dark,
          colour = NA, lty = 0) +
  geom_sf(data = buildings$osm_polygons, 
          fill = street_color_dark,
          #alpha = 0.6,
          colour = NA) +
  coord_sf(xlim = c(bb_Portland[1,1], bb_Portland[1,2]),
           ylim = c(bb_Portland[2,1], bb_Portland[2,2]),
           expand = FALSE) +
  theme_void() +
  theme_dark +
  labs(title = "portland")

ggsave('portland.png', width = 7, height = 7, units = 'in')
