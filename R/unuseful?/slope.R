setwd("~/Desktop/studia/dissertation/data")
library(rgeos)
library(tmap)
library(raster)
library(sp)
library(spatstat)
library(rgeos)
library(osmdata)
library(maptools)
library(chron)
library(sf)
library(rgdal)
library(randomcoloR)
library(stats)
library(rgbif)
library(tmap)
options(scipen=999)

output <- readOGR("strava/output.shp")
names(output@data) <- c("trackID", "pointID", "lon", "lat", "speed_km/h", "spped_m/s", "time", 
                        "time_diff","dist_to_next", "ele", "ele_n1", "slope", "ele_api", "ele_api.n1")


#slope api
output$api_slope <- (output$ele_api.n1-output$ele_api)/output$dist_to_next

output$api_slope <- output$api_slope*100
output$api_slope <- ifelse(is.na(output$api_slope), 0, output$api_slope)
output$api_slope <- ifelse(output$api_slope==Inf, 0, output$api_slope)
output$api_slope <- ifelse(output$api_slope==-Inf, 0, output$api_slope)

#45136 --> weird jump of elevation from 19 to 30; as a result, slope equals -300
nrow(output)
nrow(output[output@data$api_slope>(-50)&output@data$api_slope<50,])

output <- output[output@data$api_slope>(-20)&output@data$api_slope<20,]
summary(output@data)

#lowess to smooth the results 
lowess=NULL
for (i in levels(output$trackID)) {
  output2 <- output@data[output@data$trackID==i,]
  output2$lowess_api_slope <- lowess(output2$api_slope, f=0.001)$y
  output2$abs_api_slope <- abs(output2$api_slope)
  output2$lowess_abs_api_slope <- lowess(output2$abs_api_slope, f=0.001)$y
  lowess=rbind(lowess, output2)
  print(i)
}
summary(lowess)

#api elevation is much more precise
sd(lowess$api_slope, na.rm=T)
sd(lowess$ele, na.rm=T)



dfsp <- SpatialPointsDataFrame(lowess[c("lon", "lat")], lowess, proj4string = CRS("+init=epsg:4326"))
names(dfsp)

tm_shape(dfsp) + tm_dots("api_slope",style="quantile", size=.0125)

library(ggplot2)
ggplot(dfsp@data, aes(lon, lat, group=trackID, color=abs_api_slope)) + 
  geom_path(size = .5, lineend = "round") + 
  scale_color_gradient2(low="black", high="red", breaks = seq(0, 20, by = 2)) 

ggplot(dfsp@data, aes(lon, lat, group=trackID, color=lowess_abs_api_slope)) + 
  geom_path(size = .5, lineend = "round") + 
  scale_color_gradientn(colours = rainbow(7), breaks = seq(0, 20, by = 2))

summary(dfsp@data)

rgdal::writeOGR(dfsp, ".", layer="strava/dfsp", driver = "ESRI Shapefile", morphToESRI=T)

