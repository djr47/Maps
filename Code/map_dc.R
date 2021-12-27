library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)

# https://www.vistaprint.com/photo-gifts/canvas-prints?GP=11%2f15%2f2021+17%3a26%3a36&GPS=6104513678&GNF=1
# 12x12 stretched canvas

## Get coordinate functions and theme
source('C:/Users/djr47/Desktop/Maps/map_functions_and_theme.R')

## Get DC Data =============================================================

# admin_level
# 4 - 'District of Columbia',
# 6 - 'Washington'
# 10 - Neighborhoods

## border definition

Center_DC <- c(38.89497, -77.03947)

# dc_border <- opq(bbox="District of Columbia") %>%
#   add_osm_feature(key="admin_level", value="6") %>%
#   osmdata_sf() %>%
#   .$osm_multipolygons %>%
#   select(osm_id, name, geometry) %>%
#   filter(name == 'Washington') 

bb_DC_polygon <- getbb(place_name = 'District of Columbia', format_out = 'sf_polygon')

bb_DC_square <- make_square(Center_DC, 7.5) %>% bb_box()

border <- getbb(place_name = 'District of Columbia', format_out = 'sf_polygon') %>%
  opq() %>%
  add_osm_feature(key = 'admin_level',
                  value = '4') %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name == 'District of Columbia')

water <- bb_DC_square %>%
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
  

buildings <- bb_DC_polygon %>%
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
                            'hospital',
                            'military',
                            'public',
                            'train_station',
                            'government')) %>%
  osmdata_sf() %>%
  trim_osmdata(bb_DC_polygon)

islands <- bb_DC_square %>%
  opq() %>%
  add_osm_feature(key = 'place', 
                  value = 'islet') %>%
  osmdata_sf() 



## Plot dc =================================================================

water_tr_multi <- st_intersection(x = water$osm_multipolygons, y = border)
water_tr_poly  <- st_intersection(x = water$osm_polygons, y = border)
water_tr_lines <- st_intersection(x = water$osm_lines, y = border)

ggplot() +
  geom_sf(data = water_tr_multi,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = water_tr_poly,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = water_tr_lines,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = islands$osm_polygons,
          fill = background_color_dark,
          colour = NA, lty = 0) +
  geom_sf(data = buildings$osm_polygons, 
          fill = street_color_dark,
          #alpha = 0.6,
          colour = NA) +
  #coord_sf(xlim = c(bb_DC_square[1,1], bb_DC_square[1,2]),
  #         ylim = c(bb_DC_square[2,1], bb_DC_square[2,2]),
  #         expand = FALSE) +
  theme_void() +
  theme_dark +
  labs(title = "dc")

ggsave('dc.png', width = 7, height = 7, units = 'in')