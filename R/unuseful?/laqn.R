setwd("~/Desktop/studia/dissertation/data")

library(httr)
library(jsonlite)
library(spatstat)  
library(maptools) 
library(raster)
library(gstat)
library(xts)

WardsMap <- readOGR("statistical-gis-boundaries-london/ESRI/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))

#list of monitoring stations
sites <- as.data.frame(jsonlite::fromJSON("http://api.erg.kcl.ac.uk/AirQuality/Information/MonitoringSiteSpecies/GroupName=London/Json", simplifyVector = T))
head(sites)
sites <- sites[,1:9]
sites[sites==''] <- NA
sites$status <- ifelse(is.na(sites$Sites.Site..DateClosed), "Active", "Closed")
names(sites) <- c("LA_Code", "LA_Name", "Site_Code", "Site_Name", "Site_type", "Date_Closed", "Date_Opened", "Latitude", 
                  "Longitude", "Status")
sites[,8:9] <- lapply(sites[,8:9], as.numeric)
sites <- sites[complete.cases(sites$Latitude),]
sites_sp <- SpatialPointsDataFrame(sites[,c(9:8)], sites, proj4string = CRS("+init=epsg:4326"))
plot(sites_sp)
summary(as.factor(sites$Status))

#map for LAQN sites
tm_shape(WardsMap) + tm_borders(col="grey70", alpha=0.5) + tm_shape(sites_sp) + 
  tm_symbols(col="Status", shape=4, size=.8, palette = c("blue", "red"))

#list of monitored pollutants
pol <- as.data.frame(jsonlite::fromJSON("http://api.erg.kcl.ac.uk/AirQuality/Information/Species/Json", simplifyVector = T))
pol <- pol[1]
names(pol) <- "Pollutant"
pol <- unlist(pol)

l1 <- list() 
l2 <- list()

#this loop downloads data for choosen sites and pollutants for a given range of date
for (i in sites$Site_Code) {
  for(j in pol){
    rawdata <- jsonlite::fromJSON(paste0("http://api.erg.kcl.ac.uk/AirQuality/Data/SiteSpecies/SiteCode=", i, "/SpeciesCode=", j, "/StartDate=2018-08-12/EndDate=2018-08-13/Json"), simplifyVector = T)
    rawdata$RawAQData$Data$Site <- i
    names(rawdata$RawAQData$Data) <- c("Date", j, "Site")
    rawdata$RawAQData$Data[rawdata$RawAQData$Data==""] <- NA
    l1[[j]] <- rawdata$RawAQData$Data
  }
  l2[[i]] <- Reduce(merge,l1)
}

pollution <- do.call(rbind, l2)
rownames(pollution) <- 1:nrow(pollution)
pollution$Site <- as.factor(pollution$Site)
pollution[3:8] <- lapply(pollution[3:8], as.character)
pollution[3:8] <- lapply(pollution[3:8], as.numeric)
laqn <- merge(pollution, sites, by.x="Site", by.y="Site_Code")

laqn_sp <- SpatialPointsDataFrame(laqn[,c("Longitude", "Latitude")], laqn, proj4string = CRS("+init=epsg:4326"))
laqn_sp <- spTransform(laqn_sp, CRS("+init=epsg:27700"))

#interpolation
# Create a tessellated surface
dat.pp <- as(dirichlet(as.ppp(laqn_sp)), "SpatialPolygons")
dat.pp <- as(dat.pp,"SpatialPolygons")

# Sets the projection to British National Grid
proj4string(dat.pp) <- CRS("+init=EPSG:27700")
proj4string(laqn_sp) <- CRS("+init=EPSG:27700")

# Assign to each polygon the data from House.Points 
int.Z <- over(dat.pp,laqn_sp, fn=mean) 

# Create a SpatialPolygonsDataFrame
thiessen <- SpatialPolygonsDataFrame(dat.pp, int.Z)
thiessen.crop <-crop(thiessen, WardsMap)

# maps the thiessen polygons and House.Points
tm_shape(WardsMap) + tm_fill(alpha=.3, col = "grey") +
  tm_shape(thiessen.crop) +  tm_borders(alpha=.5, col = "black") +
  tm_shape(laqn_sp) + tm_dots(col = "blue", scale = 0.5) 

# define sample grid based on the extent of the House.Points file
grid <-spsample(laqn_sp, type = 'regular', n = 10000)


idw <- idw(laqn_sp$SO2 ~ 1, laqn_sp, newdata= grid)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("long", "lat", "prediction")
library(raster) # you will need to load this package if you have not done so already

# create spatial points data frame
spg <- idw.output
coordinates(spg) <- ~ long + lat

# coerce to SpatialPixelsDataFrame
gridded(spg) <- TRUE
# coerce to raster
raster_idw <- raster(spg)

# sets projection to British National Grid
projection(raster_idw) <- CRS("+init=EPSG:27700")

# we can quickly plot the raster to check its okay
plot(raster_idw)

masked_idw <- mask(raster_idw, WardsMap)

# plots the masked raster
tm_shape(masked_idw) + tm_raster("prediction", style = "quantile", n = 100, legend.show = FALSE) +
  tm_shape(sites_sp) + tm_bubbles(size = "pol", col = "pol", palette = "Blues", style = "quantile", legend.size.show = FALSE, title.col = "Price Paid (?)") +
  tm_layout(legend.position = c("left", "bottom"),  legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)
