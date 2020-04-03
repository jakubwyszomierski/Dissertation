setwd("~/Desktop/studia/dissertation/data")
library(rgdal)
library(sp)
library(tmap)
library(ggplot2)
library(ggmap)

WardsMap <- readOGR("statistical-gis-boundaries-london/ESRI/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))

tm_shape(run_map) + tm_borders(alpha=.4) + tm_shape(activity_points) + tm_dots(col="red",palette = "Reds", style = "equal", size=.2) 

gmap <- get_map(location = "London", zoom = 10)
ggmap(gmap, extent = "device") + geom_point(aes(x = coords.x1, y = coords.x2), colour = "red", 
                                                 alpha = 0.2, size = 2, data = as.data.frame(run.points@coords))

ggmap(gmap, extent = "device") + 
  stat_density2d(data = as.data.frame(run.points@coords),
                 aes(x = coords.x1, y = coords.x2,fill = ..level.., alpha = ..level..),
                 size = 0.1, bins = 16, geom = "polygon") + scale_fill_gradient(low = "red", high = "black") +
  scale_alpha(range = c(0, 0.25), guide = FALSE)