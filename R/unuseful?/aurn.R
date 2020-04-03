setwd("~/Desktop/studia/dissertation/data")

library(openair)
library(sp)
library(rgdal)
library(spatstat)  
library(maptools)
library(tmap)

WardsMap <- readOGR("statistical-gis-boundaries-london/ESRI/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))

aurn <- importMeta(source="aurn")
aurn_sp <- SpatialPointsDataFrame(aurn[,4:3], aurn, proj4string=CRS("+init=epsg:4236"))
aurn_sp <- spTransform(aurn_sp, CRS("+init=epsg:27700"))
aurn_sp <- aurn_sp[WardsMap,]
aurn_list <- as.data.frame(aurn_sp@data)
aurn_london <- importAURN(site=aurn_list$code, year=2018,meta=T)

aurn_london.sp <- SpatialPointsDataFrame(aurn_london[,c("longitude", "latitude")], aurn_london, proj4string=CRS("+init=epsg:4236"))
aurn_london.sp <- spTransform(aurn_london.sp, CRS("+init=epsg:27700"))
plot(aurn_london.sp)

#LAQN
sites <- as.data.frame(jsonlite::fromJSON("http://api.erg.kcl.ac.uk/AirQuality/Information/MonitoringSiteSpecies/GroupName=London/Json", simplifyVector = T))
head(sites)
sites <- sites[,1:9]
sites[sites==''] <- NA
sites$status <- ifelse(is.na(sites$Sites.Site..DateClosed), "Active", "Closed")
names(sites) <- c("LA_Code", "LA_Name", "Site_Code", "Site_Name", "Site_type", "Date_Closed", "Date_Opened", "Latitude", 
                  "Longitude", "Status")
sites[,8:9] <- lapply(sites[,8:9], as.numeric)
sites <- sites[complete.cases(sites$Latitude),]
sites_sp <- SpatialPointsDataFrame(sites[,9:8], sites, proj4string = CRS("+init=epsg:4326"))
sites_sp <- spTransform(sites_sp, CRS("+init=epsg:27700"))

tm_shape(WardsMap) + tm_borders(alpha=0.4, col="grey70") + tm_shape(aurn_london.sp) + 
  tm_symbols(col="yellow", size=.3) + tm_shape(sites_sp) + 
  tm_symbols(col="Status", shape=4, size=.3, palette = c("blue", "red")) 
