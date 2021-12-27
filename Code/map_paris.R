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

Center_Paris <- c(48.85989303637338, 2.343983686947395)
Center_Paris <- c(48.86176979365664, 2.3374649111147203) # lourve
Center_Paris <- c(48.85357263492959, 2.3480953413919403) # center of paris


# dc_border <- opq(bbox="District of Columbia") %>%
#   add_osm_feature(key="admin_level", value="6") %>%
#   osmdata_sf() %>%
#   .$osm_multipolygons %>%
#   select(osm_id, name, geometry) %>%
#   filter(name == 'Washington') 

bb_Paris_polygon <- getbb(place_name = 'Paris', 
                          display_name_contains = 'France',
                          format_out = 'sf_polygon', 
                          featuretype = 'city')

#bb_Paris_square <- getbb(place_name = 'Paris', 
#                         display_name_contains = 'France',
#                         featuretype = 'city')

bb_Paris_square <- make_square(Center_Paris, 1.5) %>% bb_box()
bb_Paris_sq_border <- make_square(Center_Paris, 1.5) %>% bb_box()

border <- getbb(place_name = 'Paris', 
                display_name_contains = 'France', 
                format_out = 'sf_polygon', 
                featuretype = 'city') %>%
  opq() %>%
  add_osm_feature(key = 'admin_level',
                  value = '6') %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name == 'Paris')

water <- bb_Paris_square %>%
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
  

buildings <- bb_Paris_square %>%
  opq() %>%
  add_osm_feature(key = 'building',
                  value = c('yes',
                            'church',
                            'cathedral',
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
  #trim_osmdata(bb_Paris_polygon)

islands <- bb_Paris_square %>%
  opq() %>%
  add_osm_feature(key = 'place', 
                  value = 'islet') %>%
  osmdata_sf() 

bridges <- bb_Paris_square %>%
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
  coord_sf(xlim = c(bb_Paris_square[1,1], bb_Paris_square[1,2]),
           ylim = c(bb_Paris_square[2,1], bb_Paris_square[2,2]),
           expand = FALSE) +
  theme_void() +
  theme_dark +
  labs(title = "paris")

ggsave('paris.png', width = 7, height = 7, units = 'in')

