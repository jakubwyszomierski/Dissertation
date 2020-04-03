#loading all of the runs and merging them into one object

setwd("~/Desktop/studia/dissertation/data")
library(rgdal)
library(tmap)
library(raster)
library(sp)
library(spatstat)
library(rgeos)
library(osmdata)
library(maptools)
library(chron)

#load data
WardsMap <- readOGR("Shapefiles/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:4326"))

#merge
file_list <- list.files("Strava")
file_list
for (i in file_list){
  # if the merged activity doesn't exist, create it
  if (!exists("runs")){
    runs <- readOGR(dsn=paste0("Strava/", i), layer = "track_points")
    runs$id <-  gsub("*.gpx", "",i)
  }
  # if the merged activity does exist, append to it
  if (exists("runs")){
    print(i)
    temp_dataset <-readOGR(dsn=paste0("Strava/", i), layer = "track_points")
    temp_dataset$id <- gsub("*.gpx", "",i)
    runs<-bind(runs, temp_dataset)
    rm(temp_dataset)
  }
}
head(runs)
#the first file is loaded twice
runs <- runs[-c(1:1895),]
runs <- spTransform(runs, CRS("+init=epsg:4326"))
runs <- runs[WardsMap,]
rownames(runs@data) <- 1:nrow(runs@data)
runs <- runs[c("ele", "time", "id")]
runs@data$pointid <- rownames(runs@data)
names(runs@data) <- c("ele", "time", "trackID", "pointID")
runs@data[,c("trackID", "pointID")] <- lapply(runs@data[,c("trackID", "pointID")], as.factor)

rgdal::writeOGR(runs, ".", layer="MyShapefiles/runs", driver = "ESRI Shapefile", morphToESRI=T)