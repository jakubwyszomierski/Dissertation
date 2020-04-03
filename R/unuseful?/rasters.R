setwd("~/Desktop/studia/dissertation/data")
library(GISTools)
library(raster)
library(readxl)
#load files
run.points <- readOGR(dsn="strava/strava+nike/activities/1624798373.gpx", layer="track_points")
run.line <- readOGR(dsn="strava/strava+nike/activities/1624798373.gpx", layer="tracks")
class(run.line)
BoroughsMap <- readOGR("LAEI/1_Supporting_Information/2_GIS_Geographies/ArcGIS/Borough_boundary.shp")
PM10 <- read.csv("LAEI/4_1_Concentrations_LAEI_2013_Update/2013/CSV/PostLAEI2013_2013_PM10.csv")


#transform map
BoroughsMap <- spTransform(BoroughsMap, CRS("+init=epsg:27700"))
BrentMap <- BoroughsMap[BoroughsMap@data$BOROUGH=="Brent",]
BrentMap <- spTransform(BrentMap, CRS("+init=epsg:27700"))
BrentMap@polygons

#cut map
cut <- as(extent(c()), "SpatialPolygons")
proj4string(cut) <- CRS(proj4string(BrentMap))
Brent_cut <- gIntersection(BrentMap, cut, byid=TRUE)
plot(Brent_cut)
plot(BoroughsMap)

#run as points
plot(run.points)
run.points<- spTransform(run.points, CRS("+init=epsg:27700"))
#raster
r1=raster(ncol=20, nrow=20, extent(BrentMap))
run.points <- as(run.points, "SpatialPoints")
r1.points.raster <- rasterize(run.points, r1, fun=max)
plot(r1.points.raster, col="white")
plot(BrentMap, add=T, border="grey")
plot(r1.points.raster, add=T)
res(r1.points.raster)
View(run.points@coords)

#run as line
plot(run.line)
class(run.line)
run.line <- spTransform(run.line, CRS("+init=epsg:27700"))

r2 = raster(nrow=20, ncol=20,ext=extent(Brent_cut))
run.line <- as(run.line, "SpatialLinesDataFrame")
r2.line.raster <- rasterize(run.line, r2)
plot(r2.line.raster, col="white")
plot(BrentMap, add=T, border="grey")
plot(r2.line.raster, add=T)

#pollution
summary(PM10)
nrow(PM10)
head(PM10)
names(PM10) <- c("Easting", "Northing", "concentration", "year")
PM10.Points <- SpatialPointsDataFrame(PM10[,1:2], PM10, proj4string = CRS("+init=epsg:27700"))
PM10.Points<- spTransform(PM10.Points, CRS("+init=epsg:4326"))
PM10.cut <- PM10.Points[Brent_cut,]
r3=raster(nrow=50, ncol=50,ext=extent(Brent_cut))
PM10.cut <- as(PM10.cut, "SpatialPoints")
PM10.cut.raster <- rasterize(PM10.cut, r3, "concentration")
plot(PM10.cut.raster, col="white")
plot(BrentMap, add=T, border="grey")
plot(PM10.cut.raster, add=T)
res(PM10.cut.raster)

#reclassification
raster_result <- PM10.raster * r1.points.raster
spplot(raster_result, col.regions=heat.colors(9), cut=8)
res(raster_result)

#what about pollution for whole London
r4=raster(nrow=20, ncol=20,ext=extent(BoroughsMap))
PM10.Points <- as(PM10.Points, "SpatialPoints")
PM10.Points.raster <- rasterize(PM10.Points, r4)
plot(PM10.Points.raster, col="white")
plot(BoroughsMap, add=T, border="grey")
plot(PM10.Points.raster, add=T)

#Northings and Eastings
PM10.grid <- PM10
coordinates(PM10.grid) <- ~Easting+Northing
proj4string(PM10.grid) <- CRS("+init=epsg:27700")
PM10.grid <- PM10.grid[BrentMap,]
PM10.grid <- as(PM10.grid, "SpatialPixelsDataFrame")
PM10.raster <- raster(PM10.grid, layer=1)
res(PM10.raster)
PM10.raster
spplot(PM10.raster, col.regions=heat.colors(9), cut=8)
View(PM10)

