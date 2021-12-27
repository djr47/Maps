library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)

# https://www.vistaprint.com/photo-gifts/canvas-prints?GP=11%2f15%2f2021+17%3a26%3a36&GPS=6104513678&GNF=1
# 12x12 stretched canvas

## Get coordinate functions and theme
source('C:/Users/djr47/Desktop/Maps/map_functions_and_theme.R')

## Get Data =============================================================

Center_NYC <- c(40.7402, -73.9756) # below grand central station
#Center_NYC <- c(40.7857, -73.9617) # central park
Center_NYC <- c(40.72047873513923, -73.99400040749073) # chinatown

border <- getbb(place_name = 'New York County', format_out = 'polygon') %>%
  opq() %>%
  add_osm_feature(key = 'admin_level',
                  value = '6') %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name == 'New York County')

bb_NYC_polygon <- getbb(place_name = 'New York County', format_out = 'polygon')

bb_NYC <- make_square(Center_NYC, 1.50) %>% bb_box()

water <- bb_NYC %>%
  opq() %>%
  add_osm_features(features = c ("\"waterway\" = \"river\"",
                                 "\"waterway\" = \"stream\"",
                                 "\"waterway\" = \"riverbank\"",
                                 "\"waterway\" = \"dam\"",
                                 "\"waterway\" = \"waterfall\"",
                                 "\"waterway\" = \"tidal_channel\"",
                                 "\"natural\" = \"water\"",
                                 "\"natural\" = \"bay\"",
                                 "\"natural\" = \"strait\"",
                                 "\"water\" = \"lake\"",
                                 "\"water\" = \"pond\"",
                                 "\"water\" = \"reservoir\"",
                                 "\"water\" = \"river\"",
                                 "\"water\" = \"stream\"",
                                 "\"water\" = \"oxbow\"",
                                 "\"water\" = \"basin\"",
                                 "\"water\" = \"stream_pool\"")) %>%
  osmdata_sf()

lakes <- bb_NYC_polygon %>%
  opq() %>%
  add_osm_features(features = c ("\"natural\" = \"water\"",
                                 "\"water\" = \"lake\"",
                                 "\"water\" = \"pond\"",
                                 "\"water\" = \"reservoir\"")) %>%
  osmdata_sf()

buildings <- bb_NYC %>%
  opq() %>%
  add_osm_feature(key = 'building',
                  value = c('yes',
                            'church',
                            'dormitory',
                            'cathedral',
                            'civic',
                            'apartments',
                            'works',
                            'hospital',
                            'school',
                            'triumphal_arch',
                            #'parking',
                            'university',
                            'public',
                            'train_station',
                            'museum',
                            'warehouse',
                            'roof',
                            'office')) %>%
  osmdata_sf()

islands <- bb_NYC %>%
  opq() %>%
  add_osm_feature(key = 'place', 
                  value = c('island','islet')) %>%
  osmdata_sf() 

bridges <- bb_NYC %>%
  opq() %>%
  add_osm_feature(key = 'man_made',
                  value = 'bridge') %>%
  osmdata_sf()

## Plot  =================================================================

ggplot() +
  #geom_sf(data = border) +
  geom_sf(data = water$osm_multipolygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = water$osm_polygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = water$osm_lines,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = lakes$osm_multipolygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = bridges$osm_polygons,
          fill = background_color_dark,
          colour = NA, lty = 0) +
  geom_sf(data = islands$osm_multipolygons,
          fill = background_color_dark,
          colour = NA) +
  geom_sf(data = buildings$osm_polygons, 
          fill = street_color_dark, #street_color_light,
          #alpha = 0.6,
          colour = NA) +
  coord_sf(xlim = c(bb_NYC[1,1], bb_NYC[1,2]),
           ylim = c(bb_NYC[2,1], bb_NYC[2,2]),
           expand = FALSE) +
  theme_void() +
  theme_dark +
  labs(title = "nyc")

ggsave('nyc.png', width = 7, height = 7, units = 'in')
