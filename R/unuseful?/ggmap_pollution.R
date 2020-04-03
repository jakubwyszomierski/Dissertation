setwd("~/Desktop/studia/dissertation/data")
library(tmap)
library(sp)
library(spatstat)
library(gstat)
library(rgdal)
library(raster)
library(ggmap)
library(readxl)

pollutants <- read.csv("modelled_pollutants.csv")

activity_points <- readOGR("strava/activity_points.shp")
activity_points <- spTransform(activity_points, CRS("+init=epsg:27700"))
plot(activity_points)
#load data
WardsMap <- readOGR("statistical-gis-boundaries-london/ESRI/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))
run_map <- WardsMap[!is.na(over(WardsMap, geometry(activity_points))),]
run_map <- spTransform(run_map, CRS("+init=epsg:27700"))
activity_points <- activity_points[WardsMap,]

#Emissions by 1km grid square and source, for each inventory year 
CO2 <- read_xlsx("LAEI/2_Grid Emissions_Summary_Excel_Files/Excel/LAEI2013_Emissions_Summary-CO2_v1.1.xlsx", sheet=2, skip = 2)
CO2.sp <- SpatialPointsDataFrame(CO2[,c(4:5)], CO2, proj4string = CRS("+init=epsg:27700"))
tm_shape(CO2.sp)+tm_dots(col="Total", palette="-Spectral",shape=15, auto.palette.mapping=T, style="quantile", n=20, size=.5)

head(pollutants)

#raster
for (i in names(pollutants)[-c(1,2)]) {
  grid <- pollutants[, c("Easting", "Northing", i)]
  coordinates(grid) <- ~Easting+Northing
  proj4string(grid) <- CRS("+init=epsg:27700")
  grid <- grid[run_map,]
  grid <- as(grid, "SpatialPixelsDataFrame")
  assign(paste0(i, ".raster"), raster(grid, layer=1))
  rm(grid)
}
plot(PM25.raster)

#extract the values of those particular pixels points lie within
names(activity_points@data)
sum(is.na(activity_points@data))
run.pollution <- data.frame(activity_points@data,extract(PM25.raster, activity_points, method="bilinear"),
                            extract(PM10.raster, activity_points, method="bilinear"),extract(PM10d.raster, activity_points, method="bilinear"),
                            extract(NOx.raster, activity_points, method="bilinear"),extract(NO2.raster, activity_points, method="bilinear"))

head(run.pollution)
sum(is.na(run.pollution))
summary(run.pollution)

run.pollution
names(run.pollution) <- c("ID", "Name", "Type", "Elapsed_time", "Distance", "Commute", "Latitude", 
                            "Longitude", "Time", "Latitude.p1", "Longitude.p1", "Dist_to_prev",
                            "Time.p1", "Time_diff_to_prev", "M_per_sec","Km_per_h", "Lowess_speed",
                            "Elevation","Lowess_elevation", "PM25", "PM10","PM10d", "NOx", "NO2")

run.pollution <- run.pollution[complete.cases(run.pollution), ]

#now, each of the running point has a raster value (which is pollutant concentration)
#calculate exposure rate (pollutant concentration * ventilation rate *  road position scaling)

#exposure rates
run.pollution$exposure_run <- run.pollution$PM25*0.8
run.pollution$Exposure_cycle <- run.pollution$PM10*1.25*1.91

#calculate average concentration for pollution
mean(run.pollution$NOx)

#let's create a Spatial Point Data Frame
run.pollution.spdf <- SpatialPointsDataFrame(run.pollution[,c("Longitude", "Latitude")], run.pollution, 
                                             proj4string = CRS("+init=epsg:4326"))
head(run.pollution.spdf@data)

#map for tracks

#elevation
tmap_mode('plot')
tm_shape(run_map) + tm_borders(alpha=1, lty=2) + tm_shape(run.pollution.spdf) + 
  tm_dots(col = "PM10", palette=c("lightgreen", "firebrick1"), style = "quantile",shape=16, size=.03) +
  tm_layout(basemaps = c('OpenStreetMap')) 

#ggplot2
google_map <- get_googlemap(zoom = 11,scale = 1,maptype = "terrain",
                                  style = c(feature = "all", element = "labels", visibility = "off"),
                                  center = c(lon = ((min(run.pollution.spdf@data$Longitude)+max(run.pollution.spdf@data$Longitude))/2), 
                                             lat = ((min(run.pollution.spdf@data$Latitude)+max(run.pollution.spdf@data$Latitude))/2)))

ggmap(google_map, extent="device") +geom_point(aes(x = Longitude,y = Latitude, 
                                                   col=PM25), 
                                                     data=run.pollution.spdf@data,alpha=.08, size=.4)+ 
  scale_colour_gradient(low = "lightskyblue", high="firebrick1",na.value="black")

d <- run.pollution.spdf

google_map <- get_googlemap(zoom = 11,scale = 1,maptype = "terrain",
                            style = c(feature = "all", element = "labels", visibility = "off"),
                            center = c(lon = ((min(d@data$Longitude)+max(d@data$Longitude))/2), 
                                       lat = ((min(d@data$Latitude)+max(d@data$Latitude))/2)))

ggmap(sgoogle_map, extent="device") +geom_point(aes(x = Longitude,y = Latitude, 
                                                   col=PM25), 
                                               data=d@data,alpha=1, size=1)+ 
  scale_colour_gradient(low = "lightskyblue", high="firebrick1",na.value="black")

#create quantiles for ggmap for all
for (i in names(run.pollution.spdf)[20:24]) {
  quantiles <- quantile(run.pollution.spdf@data[,i], probs = seq(0, 1, length.out = 5 + 1))
  labels <- c()
  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], 2), " – ", round(quantiles[idx + 1], 2)))
  }
  # I need to remove the last label because that would be something like "66.62 - NA"
  labels <- labels[1:length(labels)-1]
  # here I actually create a new  variable on the dataset with the quantiles
  run.pollution.spdf@data[, paste0(i, "_quantile")] <-  cut(run.pollution.spdf@data[,i], breaks = quantiles,
                                                            labels = labels, include.lowest = T)
  google_map <- get_googlemap(zoom = 11,scale = 1,maptype = "terrain",
                              style = c(feature = "all", element = "labels", visibility = "off"),
                              center = c(lon = ((min(run.pollution.spdf@data$Longitude)+max(run.pollution.spdf@data$Longitude))/2), 
                                         lat = ((min(run.pollution.spdf@data$Latitude)+max(run.pollution.spdf@data$Latitude))/2)))
  
  g <- ggmap(google_map, extent="device") + geom_point(aes(x = Longitude,y = Latitude, col=run.pollution.spdf@data[, paste0(i, "_quantile")]), 
                                                       data=run.pollution.spdf@data,alpha=.2, size=.5) +
    scale_color_brewer(type = "seq", palette = "OrRd", direction = 1)+ 
    guides(colour = guide_legend(title=i, override.aes = list(size=5, alpha=1)))  + 
    theme(legend.text = element_text(size = 12), legend.title = element_text(size=20))
  ggsave(g, file=paste0("~/Desktop/studia/dissertation/plots/run_output/all/" ,i, "_quantile.png"))
}

#create quantiles for ggmap for each track
for(j in levels(run.pollution.spdf@data$ID)){
  for (i in names(run.pollution.spdf)[20:24]) {
    dataset <- run.pollution.spdf[run.pollution.spdf@data$ID==j,]
    quantiles <- quantile(dataset@data[,i], probs = seq(0, 1, length.out = 5 + 1))
    labels <- c()
    for(idx in 1:length(quantiles)){
      labels <- c(labels, paste0(round(quantiles[idx], 2), " – ", round(quantiles[idx + 1], 2)))
    }
    # I need to remove the last label because that would be something like "66.62 - NA"
    labels <- labels[1:length(labels)-1]
    # here I actually create a new  variable on the dataset with the quantiles
    dataset@data[, paste0(i, "_quantile")] <-  cut(dataset@data[,i], breaks = quantiles,
                                                   labels = labels, include.lowest = T)
    google_map <- get_googlemap(zoom = 13,scale = 1,maptype = "terrain",
                                style = c(feature = "all", element = "labels", visibility = "off"),
                                center = c(lon = ((min(dataset@data[i,"Longitude"])+max(dataset@data[i,"Longitude"]))/2), 
                                           lat = ((min(dataset@data[i, "Latitude"])+max(dataset@data[i, "Latitude"]))/2)))
    g <- ggmap(google_map, extent="device") + geom_point(aes(x = Longitude,y = Latitude, col=dataset@data[, paste0(i, "_quantile")]), 
                                                         data=dataset@data,alpha=.2, size=.5) +
      scale_color_brewer(type = "seq", palette = "OrRd", direction = 1)+ 
      guides(colour = guide_legend(title=i, override.aes = list(size=5, alpha=1)))  + 
      theme(legend.text = element_text(size = 12), legend.title = element_text(size=20))
    ggsave(g, file=paste0("~/Desktop/studia/dissertation/plots/run_output/" ,i,"_",j, "_quantile.png"))
  }
}

#mean pollution
head(run.pollution.spdf)
runs <- run.pollution %>% group_by(ID) %>% 
  summarise(avg_PM25=mean(PM25),avg_PM10=mean(PM10), avg_PM10d=mean(PM10d), avg_NOx=mean(NOx), avg_NO2=mean(NO2),
            Distance=min(Distance), Elapsed_time=min(Elapsed_time)) %>% 
  mutate(Elapsed_pace=Distance/Elapsed_time)
runs

ggmap(google_map, extent="device") +geom_point(aes(x = Longitude,y = Latitude, 
                                                   col=avg_PM25), 
                                               data=r@data,alpha=.08, size=.4)+ 
  scale_colour_gradient(low = "lightgreen", high="firebrick1",na.value="black")

r <- merge(run.pollution, runs, by="ID")
r <- SpatialPointsDataFrame(r[,c("Longitude", "Latitude")], r, 
                                             proj4string = CRS("+init=epsg:4326"))
head(r@data)
tm_shape(run_map) + tm_borders(alpha=1) + tm_shape(r) + 
  tm_dots(col = "avg_PM10", palette=c("lightgreen", "firebrick1"), style = "quantile",shape=16, size=.06) + tm_layout(basemaps = c('OpenStreetMap')) 

tm_shape(run_map) + tm_borders(alpha=1) + tm_shape(r) + 
  tm_dots(col = "avg_PM10d", palette=c("lightgreen", "firebrick1"), style = "quantile",shape=16, size=.06) + tm_layout(basemaps = c('OpenStreetMap')) 

tm_shape(run_map) + tm_borders(alpha=1) + tm_shape(r) + 
  tm_dots(col = "avg_PM25", palette=c("lightgreen", "firebrick1"), style = "quantile",shape=16, size=.06) + tm_layout(basemaps = c('OpenStreetMap')) 

tm_shape(run_map) + tm_borders(alpha=1) + tm_shape(r) + 
  tm_dots(col = "avg_NOx", palette=c("lightgreen", "firebrick1"), style = "quantile",shape=16, size=.06) + tm_layout(basemaps = c('OpenStreetMap')) 

tm_shape(run_map) + tm_borders(alpha=1) + tm_shape(r) + 
  tm_dots(col = "avg_NO2", palette=c("lightgreen", "firebrick1"), style = "quantile",shape=16, size=.06) + tm_layout(basemaps = c('OpenStreetMap')) 

for(j in levels(r@data$ID)){
  for (i in names(r)[26:30]) {
    dataset <- r[r@data$ID==j,]
    quantiles <- quantile(dataset@data[,i], probs = seq(0, 1, length.out = 5 + 1))
    labels <- c()
    for(idx in 1:length(quantiles)){
      labels <- c(labels, paste0(round(quantiles[idx], 2), " – ", round(quantiles[idx + 1], 2)))
    }
    # I need to remove the last label because that would be something like "66.62 - NA"
    labels <- labels[1:length(labels)-1]
    # here I actually create a new  variable on the dataset with the quantiles
    dataset@data[, paste0(i, "_quantile")] <-  cut(dataset@data[,i], breaks = quantiles,
                                                   labels = labels, include.lowest = T)
    g <- ggmap(google_map, extent="device") + geom_point(aes(x = Longitude,y = Latitude, col=dataset@data[, paste0(i, "_quantile")]), 
                                                         data=dataset@data,alpha=.2, size=.5) +
      scale_color_brewer(type = "seq", palette = "OrRd", direction = 1)+ 
      guides(colour = guide_legend(title=i, override.aes = list(size=5, alpha=1)))  + 
      theme(legend.text = element_text(size = 12), legend.title = element_text(size=20))
    ggsave(g, file=paste0("~/Desktop/studia/dissertation/plots/run_output/" ,i,"_",j, "_quantile_average_run.png"))
  }
}