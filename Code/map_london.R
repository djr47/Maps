library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)

# https://www.vistaprint.com/photo-gifts/canvas-prints?GP=11%2f15%2f2021+17%3a26%3a36&GPS=6104513678&GNF=1
# 12x12 stretched canvas

## Get coordinate functions and theme
source('C:/Users/djr47/Desktop/Maps/map_functions_and_theme.R')

## Get  Data =============================================================


## border definition

Center_London <- c(51.50073, -0.1087) # center of london


bb_London_square <- make_square(Center_London, 1.5) %>% bb_box()
bb_London_sq_border <- make_square(Center_London, 1.5) %>% bb_box()

water <- bb_London_square %>%
  opq() %>%
  add_osm_features(features = c ("\"waterway\" = \"river\"",
                                 "\"waterway\" = \"stream\"",
                                 "\"waterway\" = \"riverbank\"",
                                 "\"waterway\" = \"dam\"",
                                 "\"waterway\" = \"waterfall\"",
                                 "\"natural\" = \"water\"",
                                 "\"water\" = \"lake\"",
                                 "\"water\" = \"pond\"",
                                 "\"water\" = \"reservoir\"",
                                 "\"water\" = \"river\"",
                                 "\"water\" = \"stream\"",
                                 "\"water\" = \"oxbow\"",
                                 "\"water\" = \"basin\"",
                                 "\"water\" = \"stream_pool\"")) %>%
  osmdata_sf()
  

buildings <- bb_London_square %>%
  opq() %>%
  add_osm_feature(key = 'building',
                  value = c('yes',
                            'church',
                            'cathedral',
                            'residential',
                            'museum',
                            'civic',
                            'castle',
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
                            'government')) %>%
  osmdata_sf() #%>%
  #trim_osmdata(bb_London_polygon)

islands <- bb_London_square %>%
  opq() %>%
  add_osm_feature(key = 'place', 
                  value = 'islet') %>%
  osmdata_sf() 

bridges <- bb_London_square %>%
  opq() %>%
  add_osm_feature(key = 'man_made',
                  value = 'bridge') %>%
  osmdata_sf()

## Plot dc =================================================================

ggplot() +
  geom_sf(data = water$osm_multipolygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = water$osm_polygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = water$osm_lines,
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
  geom_sf(data = buildings$osm_multipolygons, 
          fill = street_color_dark,
          #alpha = 0.6,
          colour = NA) +
  coord_sf(xlim = c(bb_London_square[1,1], bb_London_square[1,2]),
           ylim = c(bb_London_square[2,1], bb_London_square[2,2]),
           expand = FALSE) +
  theme_void() +
  theme_dark +
  labs(title = "london")

ggsave('london.png', width = 7, height = 7, units = 'in')

