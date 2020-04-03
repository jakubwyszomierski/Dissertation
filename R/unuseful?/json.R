#json
setwd("~/Desktop/studia/dissertation/data")

sites <- as.data.frame(jsonlite::fromJSON("http://api.erg.kcl.ac.uk/AirQuality/Information/MonitoringSiteSpecies/GroupName=London/Json", simplifyVector = T))
head(sites)
names(sites) <- c("LA_Code", "LA_Name", "Site_Code", "Site_Name", "Site_type", "Date_Closed", "Date_Opened", "Latitude", "Longitude", "LatitudeWGS84", "LongitudeWGS84", "DataOwner",
                  "Data_Manager", "Site_link", "Species")
sites <- sites[,1:9]
sites[sites==''] <- NA
sites[,8:9] <- lapply(sites[,8:9], as.numeric)
sites <- sites[complete.cases(sites$Latitude),]
sites_sp <- SpatialPointsDataFrame(sites[,9:8], sites, proj4string = CRS("+init=epsg:4326"))

pol <- c("NO2", "SO2", "PM10", "PM25")

jf <- list() 
f <- list()

for (i in sites$Site_Code) {
 for(j in pol){
  rawdata <- jsonlite::fromJSON(paste0("http://api.erg.kcl.ac.uk/AirQuality/Data/SiteSpecies/SiteCode=", i, "/SpeciesCode=", j, "/StartDate=2018-08-12/EndDate=2018-08-13/Json"), simplifyVector = T)
  rawdata$RawAQData$Data$Site <- i
  names(rawdata$RawAQData$Data) <- c("Date", j, "Site")
  rawdata$RawAQData$Data[rawdata$RawAQData$Data==""] <- NA
  jf[[j]] <- rawdata$RawAQData$Data
 }
  f[[i]] <- Reduce(merge,jf)
}
pollution <- do.call(rbind, f)
rownames(pollution) <- 1:nrow(pollution)

pollution$Date <- as.Date(pollution$Date, format="%Y-%m-%d %T")
pollution$Site <- as.factor(pollution$Site)
pollution[3:4] <- lapply(pollution[3:4], as.character)
pollution[3:6] <- lapply(pollution[3:4], as.numeric)

summary(pollution)

sub <- pollution[1:3]
sub <- sub[complete.cases(sub),]

s <- merge(sites, sub ,by.y="Site", by.x="Site_Code")
s_sp <-  SpatialPointsDataFrame(s[,9:8], s, proj4string = CRS("+init=epsg:4326"))
s_sp <- spTransform(s_sp, CRS("+init=epsg:27700"))


# Create a tessellated surface
dat.pp <- as(dirichlet(as.ppp(s_sp)), "SpatialPolygons")
dat.pp <- as(dat.pp,"SpatialPolygons")

# Sets the projection to British National Grid
proj4string(dat.pp) <- CRS("+init=EPSG:27700")
proj4string(s_sp) <- CRS("+init=EPSG:27700")

# Assign to each polygon the data from House.Points 
int.Z <- over(dat.pp,s_sp, fn=mean) 

# Create a SpatialPolygonsDataFrame
thiessen <- SpatialPolygonsDataFrame(dat.pp, int.Z)
thiessen.crop <-crop(thiessen, WardsMap)

# maps the thiessen polygons and House.Points
library(tmap)
tm_shape(WardsMap) + tm_fill(alpha=.3, col = "grey") +
  tm_shape(thiessen.crop) +  tm_borders(alpha=.5, col = "black") +
  tm_shape(s_sp) + tm_dots(col = "blue", scale = 0.5) 

tm_shape(thiessen.crop) + tm_fill(col="NO2", style = "quantile", palette = "Reds") + tm_borders(alpha=.3, col = "black") +
  tm_shape(s_sp) + tm_dots(col = "black", scale = 0.5) 
