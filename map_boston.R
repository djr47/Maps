library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)

# https://www.vistaprint.com/photo-gifts/canvas-prints?GP=11%2f15%2f2021+17%3a26%3a36&GPS=6104513678&GNF=1
# 12x12 stretched canvas

## Get coordinate functions and theme
source('C:/Users/djr47/Desktop/Maps/map_functions_and_theme.R')

## Get Data =============================================================

Center_Boston <- c(42.358798399916985, -71.06097449532807)
#Center_Boston <- c(42.3653, -71.0548)

bb_Boston <- make_square(Center_Boston, 3) %>% bb_box()
bb_Boston_large_square <- make_square(Center_Boston, 10) %>% bb_box()
bb_Boston_clip <- make_square(Center_Boston, 1.5) %>% bb_box()


water <- bb_Boston %>%
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


buildings <- bb_Boston %>%
  opq() %>%
  add_osm_feature(key = 'building',
                  value = c('yes',
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
                            'government')) %>%
  osmdata_sf() 

islands <- bb_Boston %>%
  opq() %>%
  add_osm_feature(key = 'place', 
                  value = 'islet') %>%
  osmdata_sf() 

bridges <- bb_Boston %>%
  opq() %>%
  add_osm_feature(key = 'man_made',
                  value = 'bridge') %>%
  osmdata_sf()

## maybe for boston harbor I have to query TIGRIS files?
## http://estebanmoro.org/post/2020-10-19-personal-art-map-with-r/

library(tigris)

counties_MA <- counties(state="MA",cb=T,class="sf",)
counties_MA <- st_crop(counties_MA,
                       xmin=bb_Boston[1,1],xmax=bb_Boston[1,2],
                       ymin=bb_Boston[2,1],ymax=bb_Boston[2,2])

get_water <- function(county_GEOID){
  area_water("MA", county_GEOID, class = "sf")
}
water <- do.call(rbind, 
                 lapply(counties_MA$COUNTYFP,get_water))
water <- st_crop(water,
                 xmin=bb_Boston[1,1],xmax=bb_Boston[1,2],
                 ymin=bb_Boston[2,1],ymax=bb_Boston[2,2])

st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}
counties_MA <- st_erase(counties_MA,water)



## Plot  =================================================================

ggplot() +
  geom_sf(data=counties_MA,
          inherit.aes= FALSE,
          lwd=0.0,colour = NA, fill=background_color_dark) +
  geom_sf(data = water$osm_multipolygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = water$osm_polygons,
          fill = water_color_dark,
          colour = water_color_dark) +
  geom_sf(data = water$osm_lines,
          fill = water_color_dark,
          colour = water_color_dark) +
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
  coord_sf(xlim = c(bb_Boston_clip[1,1], bb_Boston_clip[1,2]),
           ylim = c(bb_Boston_clip[2,1], bb_Boston_clip[2,2]),
           expand = FALSE) +
  theme_void() +
  theme_dark +
  theme(panel.background = element_rect(fill = water_color_dark)) +
  labs(title = "boston")

ggsave('boston.png', width = 7, height = 7, units = 'in')
