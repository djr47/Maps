## Coordinate Functions =========================================================
make_square <- function(latlon, distance) {
  # https://gis.stackexchange.com/questions/15545/calculating-coordinates-of-square-x-miles-from-center-point
  # North or south, one degree is about 69 miles
  # east or west, it is only 69*cos(latitude) miles.
  lat <- latlon[1]
  lon <- latlon[2]
  
  dlat <- distance / 69
  dlon   <- dlat / cos(lat * (pi/180))
  
  bottom <- lat - dlat
  top    <- lat + dlat
  left   <- lon - dlon
  right  <- lon + dlon
  
  coords <- c(left, bottom, right, top)
  coords
}


bb_box <- function(coords) {
  # coords must be in form (left,bottom,right,top)
  bb_matrix <- matrix(coords,
                      nrow = 2, ncol = 2, 
                      dimnames = list(c('x','y'),c('min','max')))
  bb_matrix
}


## Plot theme ==================================================================

## Light

#background_color_light <- '#f8f5d4'
background_color_light<-'#FAF9ED'
street_color_light<-'#13130C'
small_street_color_light<-'#37261A'
water_color_light<-'#ADC5CE'
font_color_light<-'#13130C'

theme_light <- theme(
  plot.title = element_text(family='Roboto Condensed',
                            color=font_color_light,
                            size = 18, face="bold", hjust=.5,
                            vjust=2.5),
  panel.border = element_rect(colour = font_color_light, fill=NA, size=2),
  plot.margin=unit(c(0.6,1.6,1,1.6),"cm"),
  plot.subtitle = element_text(color=font_color_light,
                               family='Roboto Condensed', size = 12, 
                               vjust=2.5, hjust=.5, 
                               margin=margin(2, 0, 5, 0)),
  plot.background = element_rect(fill = background_color_light)
)

## Dark

background_color_dark <-'#1a1711' #'#1E212B'
street_color_dark <-    "#dcd3c7"#'#FAD399'
small_street_color_dark <-"#dcd3c7"#'#D4B483'
river_color_dark <-"#59788E"#'#1C7293'
water_color_dark <-"#59788E"#'#1C7293'
font_color_dark <-'#FFFFFF'
path_color_dark <- '#767374'

theme_dark <- theme(
  plot.title = element_text(family='Roboto Condensed',
                            color=font_color_dark,
                            size = 18, face="bold", hjust=.5,
                            vjust=2.5),
  panel.border = element_rect(colour = font_color_dark, fill=NA, size=2),
  plot.margin=unit(c(0.6,1.6,1,1.6),"cm"),
  plot.subtitle = element_text(color=font_color_dark,
                               family='Roboto Condensed', size = 12, 
                               vjust=2.5, hjust=.5, 
                               margin=margin(2, 0, 5, 0)),
  plot.background = element_rect(fill = background_color_dark))

## fonts on google fonts
## Staatliches
## Bebas Neue
## Roboto Condensed
## Acme

# extrafont::font_import(pattern= 'Staatliches-Regular')

