#merge pollutants
setwd("~/Desktop/studia/dissertation/data")

library(sp)
library(rgdal)
#modelled data
PM10 <- read.csv("LAEI/PostLAEI2013_2013_PM10.csv")
PM10d <- read.csv("LAEI/PostLAEI2013_2013_PM10d.csv")
PM25 <- read.csv("LAEI/PostLAEI2013_2013_PM25.csv")
NOx <- read.csv("LAEI/PostLAEI2013_2013_NOx.csv")
NO2 <- read.csv("LAEI/PostLAEI2013_2013_NO2.csv")

head(data.frame(PM10, PM10d, PM25, NOx, NO2))

PM10s <- merge(PM10, PM10d, by=c("x", "y", "year"))
PMs<- merge(PM25, PM10s, by=c("x", "y", "year"))
NOs <- merge(NOx, NO2, by=c("x", "y", "year"))
pollutants <- merge(PMs, NOs, by=c("x", "y", "year"))
names(pollutants) <- c("Easting", "Northing","Year", "PM25", "PM10", "PM10d", "NOx", "NO2")
summary(pollutants)
pollutants <- pollutants[,-3]
rm(PMs, PM10s, PM10, PM10d, NO2, NOx,NOs, PM25)

#coordinates in northing and eastings
pollutants.sp <- SpatialPointsDataFrame(pollutants[,1:2], pollutants, proj4string = CRS("+init=epsg:27700"))

output <- readOGR("MyData/output.shp")
output <- spTransform(output, CRS("+init=epsg:27700"))
WardsMap <- readOGR("Shapefiles/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))
run_map <- WardsMap[!is.na(over(WardsMap, geometry(output))),]
run_map <- spTransform(run_map, CRS("+init=epsg:27700"))

library(tmap)
#map for London
for (i in names(pollutants)[-c(1,2)]) {
  #map for run
  save_tmap(tm_shape(pollutants.sp[run_map,]) +
              tm_dots(col = i, style="quantile", n=15,shape=16, palette = "-Spectral", auto.palette.mapping=T, sizes.legend=c(.1,.5)) + 
              tm_layout(inner.margins = c(.03, .03, .02, .02),legend.outside = T, frame=F, legend.text.size = 1, 
                        legend.title.size = 2, legend.stack = "horizontal"),
            filename = paste0("~/Desktop/studia/dissertation/plots/", i, "_run_pollution_div.png"), units = "in", width = 9, height = 9)
}

write.csv(pollutants, file="MyData/modelled_pollutants.csv", row.names = F)