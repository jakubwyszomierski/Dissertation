setwd("~/Desktop/studia/dissertation/data")
library(raster)
library(tmap)
library(rgdal)

#load data
#maps
BoroughsMap <- readOGR("LAEI/1_Supporting_Information/2_GIS_Geographies/ArcGIS/Borough_boundary.shp")
BoroughsMap <- spTransform(BoroughsMap, CRS("+init=epsg:27700"))

WardsMap <- readOGR("statistical-gis-boundaries-london/ESRI/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))

#runs
run.points <- readOGR(dsn="strava/strava+nike/activities/1563089769.gpx", layer="track_points")
run.points <- spTransform(run.points, CRS("+init=epsg:27700"))
plot(run.points)
run.points

#pollution
PM10 <- read.csv("LAEI/4_1_Concentrations_LAEI_2013_Update/2013/CSV/PostLAEI2013_2013_PM10.csv")
summary(PM10)
nrow(PM10)

#let's try to extract raster value at point locations
#firts, create a raster for pollution
names(PM10) <- c("Easting", "Northing", "concentration", "year")
PM10.grid <- PM10
coordinates(PM10.grid) <- ~Easting+Northing
proj4string(PM10.grid) <- CRS("+init=epsg:27700")
PM10.grid <- PM10.grid[BoroughsMap,]
PM10.grid <- as(PM10.grid, "SpatialPixelsDataFrame")
PM10.grid@data
PM10.raster <- raster(PM10.grid, layer=1)
plot(BrentMap, add=T)
plot(PM10.raster, add=T)

#not in use
pollutants.raster <- pollutants.sp[run_map,]
gridB <-spsample(pollutants.raster, type = 'regular', n = 10000)
idwB <- idw(pollutants.raster$PM10 ~1, pollutants.raster, newdata=gridB)
idw.outputB = as.data.frame(idwB)
names(idw.outputB)[1:3] <- c("long", "lat", "PM10")
spgB <- idw.outputB
coordinates(spgB) <- ~ long + lat
# coerce to SpatialPixelsDataFrame
gridded(spgB) <- TRUE
# coerce to raster
raster_idwB <- raster(spgB)
# sets projection to British National Grid
projection(raster_idwB) <- CRS("+init=epsg:27700")
par(mar = rep(2, 4))
plot(raster_idwB)
masked_idwB <- mask(raster_idwB, pollutants.raster)


#extract the values of those particular pixels points lie within
run.pollution <- data.frame(run.points@coords, run.points@data$ele,  run.points@data$time, extract(PM10.raster, run.points))
sum(is.na(run.pollution))
#now, each of the running point has a raster value (which is pollutant concentration)
#calculate exposure rate (pollutant concentration * ventilation rate *  road position scaling)
names(run.pollution) <- c("Easting", "Northing","Elevation", "Time",  "Pollution")

#exposure rates
run.pollution$exposure.run <- run.pollution$Pollution*1*1
run.pollution$exposure.cycle <- run.pollution$Pollution*1.25*1.91

#calculate average concentration for pollution
names(run.pollution) <- c("Easting", "Northing", "Elevation", "Time", "Pollution", "Run_exposure", "Cycle_exposure")
mean(run.pollution$Pollution)

#let's create a Spatial Point Data Frame
run.pollution.spdf <- SpatialPointsDataFrame(run.pollution[,1:2], run.pollution, 
                                             proj4string = CRS("+init=epsg:27700"))
head(run.pollution.spdf@data)

#map for tracks
run_map <- WardsMap[!is.na(over(WardsMap, geometry(run.points))),]

#elevation
tmap_mode('view')

tm_shape(run_map) + tm_borders(alpha=1) + tm_shape(run.pollution.spdf) + 
  tm_dots(col = "Elevation", style = "cont", size=.03) + tm_layout(basemaps = c('OpenStreetMap'))

