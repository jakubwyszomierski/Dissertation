#downloading OpenStreetMap data in order to correct the accuracy of GPS points
#buffors to subset the data to areas near the tracks, thus shortening the time of running of the code

setwd("~/Desktop/studia/dissertation/data")
library(rgeos)
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(osmdata)
library(maptools)
library(chron)
library(sf)
library(randomcoloR)

system.time(runs <- readOGR(dsn="MyData/runs.shp"))

#osm
system.time(q <-opq(bbox = c(min(runs@coords[,1]), min(runs@coords[,2]), max(runs@coords[,1]), max(runs@coords[,2]))) %>% 
  add_osm_feature(key='highway'))
system.time(paths_london <- osmdata_sp(q))
paths <- paths_london$osm_lines
paths <- spTransform(paths, CRS("+init=epsg:27700"))

#buffers
runs <- spTransform(runs, CRS("+init=epsg:27700"))
#i'm interested in roads that are within the 40m radius; this will decrease the time of calculations
system.time(buf1 <- gUnaryUnion(gBuffer(runs, width=40, byid=TRUE))) 
plot(buf1,col=randomColor())
plot(runs, add=T, cex=1/6, pch=16, col="red")
rm(runs, q, paths_london)
system.time(buf <- paths[buf1,])
rgdal::writeOGR(buf, ".", layer="MyData/buf", driver = "ESRI Shapefile", morphToESRI=T)
