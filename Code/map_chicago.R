library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)

## Get coordinate functions and theme
source('C:/Users/djr47/Desktop/Maps/map_functions_and_theme.R')## Get Data =============================================================

Center_Chigago <- c(41.8807, -87.6294)

bb_Chigago <- make_square(Center_Chigago, 1.5) %>% bb_box()

river <- bb_Chigago %>% 
  opq() %>%
  add_osm_feature(key = "waterway", 
                  value = c('river','stream','riverbank','dam','waterfall',
                            'tidal_channel')) %>%
  osmdata_sf()

bay <- bb_Chigago %>% 
  opq() %>%
  add_osm_feature(key = 'natural', 
                  value = c('water', 'bay','strait')) %>%
  osmdata_sf()

islands <- bb_Chigago %>%
  opq() %>%
  add_osm_feature(key = 'place', 
                  value = c('islet','island')) %>%
  osmdata_sf() 

buildings <- bb_Chigago %>%
  opq() %>%
  add_osm_feature(key = 'building',
                  value = c(
                    'yes',
                    'commercial',
                    'hangar',
                    'warehouse',
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

bridges <- bb_Chigago %>%
  opq() %>%
  add_osm_feature(key = 'man_made',
                  value = 'bridge') %>%
  osmdata_sf()


others <- bb_Chigago %>%
  opq() %>%
  add_osm_features(features = c ("\"landuse\" = \"construction\"",
                                 "\"leisure\" = \"stadium\"")) %>%
  osmdata_sf()



#library(tigris)
#
#counties_CA <- counties(state="CA",cb=T,class="sf",)
#counties_CA <- st_crop(counties_CA,
#                       xmin=bb_Chigago[1,1],xmax=bb_Chigago[1,2],
#                       ymin=bb_Chigago[2,1],ymax=bb_Chigago[2,2])
#
#get_water <- function(county_GEOID){
#  area_water("CA", county_GEOID, class = "sf")
#}
#water <- do.call(rbind, 
#                 lapply(counties_CA$COUNTYFP,get_water))
##water <- st_crop(water,
##                 xmin=bb_Chigago[1,1],xmax=bb_Chigago[1,2],
##                 ymin=bb_Chigago[2,1],ymax=bb_Chigago[2,2]) 
#
#st_erase <- function(x, y) {
#  st_difference(x, st_union(y))
#}
#counties_CA <- st_erase(counties_CA,water)



## Plot  =================================================================



ggplot() +
  #geom_sf(data=counties_CA,
  #        inherit.aes= FALSE,
  #        lwd=0.0,colour = NA, fill=background_color_dark) +
  geom_sf(data = river$osm_multipolygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = river$osm_polygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = river$osm_lines,
          fill = water_color_dark,
          colour = water_color_dark,
          size = 2.75) +
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
  geom_sf(data = others$osm_polygons, 
          fill = street_color_dark,
          #alpha = 0.6,
          colour = NA) +
  coord_sf(xlim = c(bb_Chigago[1,1], bb_Chigago[1,2]),
           ylim = c(bb_Chigago[2,1], bb_Chigago[2,2]),
           expand = FALSE) +
  theme_void() +
  theme_dark +
  #theme(panel.background = element_rect(fill = water_color_dark)) +
  labs(title = "chicago")

ggsave('chicago.png', width = 7, height = 7, units = 'in')
