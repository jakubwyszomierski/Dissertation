setwd("~/Desktop/studia/dissertation/data")

library(rgdal)
library(sp)
library(tmap)
library(dplyr)
library(gridExtra)
library(ggmap)


#load data

activity_points <- readOGR("strava/activity_points.shp")
activity_points <- spTransform(activity_points, CRS("+init=epsg:27700"))
names(activity_points) <- c("ID", "Name", "Type", "Elapsed_time", "Distance", "Commute", "Latitude", "Longitude", "Time", 
                            "Latitude.n1", "Longitude.n1", "Dist_to_prev", "Time.n1", "Time_diff_to_prev", "M_per_sec",
                            "Km_per_h", "Lowess_speed",  "Elevation", "Elevation.n1", "Lowess_elevation")

WardsMap <- readOGR("statistical-gis-boundaries-london/ESRI/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))

run_map <- WardsMap[!is.na(over(WardsMap, geometry(activity_points))),]
run_map <- spTransform(run_map, CRS("+init=epsg:27700"))


#maps
tmap_mode('plot')
names(activity_points)

#heatmap
tm_shape(run_map) + tm_borders(alpha=.4, lty=2, lwd=1, col="grey80") + tm_shape(activity_points) + 
  tm_dots(alpha=.06,col="#0d8de8", size=.03, border.col=NA) + tm_layout(frame=F, bg.color="black") + tm_compass(type="8star", size=8) 
dev.copy(png,"~/Desktop/studia/dissertation/plots/heatmap.png",width=16,height=12,units="in",res=900)
dev.off() 

#elevation
tm_shape(run_map) + tm_borders(alpha=.4, lty=2) + tm_shape(activity_points) + 
  tm_dots(alpha=1, col="Elevation", style="cont", size=.05, border.col=NA) + tm_layout(frame=F) 

#speed
tm_shape(run_map) + tm_borders(alpha=.4, lty=2) + tm_shape(activity_points) + 
  tm_dots(alpha=1, col="Lowess_speed", style="cont", size=.03, border.col=NA) + tm_layout(frame=F) 

#gmap
#library(ggmap)
#gmap <- get_map("London", zoom=12)
#head(activity_points@coords)
#activity_points <- spTransform(activity_points, CRS("+init=epsg:4326"))
#ggmap(gmap, extent = "device") + 
 # stat_density2d(data = as.data.frame(activity_points@coords),
    #             aes(x = coords.x1, y = coords.x2,fill = ..level.., alpha = ..level..),
   #              size = 0.1, bins = 16, geom = "polygon") + scale_fill_gradient(low = "red", high = "black") +
  #scale_alpha(range = c(0, 0.25), guide = FALSE)

#groupings
runs <- activity_points@data %>% group_by(ID) %>% summarise(Distance=min(Distance), Elapsed_time=min(Elapsed_time)) %>% 
  mutate(elapsed_pace=Distance/Elapsed_time)
nrow(runs)
runs

#plots
#plots
i=levels(activity_points@data$ID)[90]
for (i in levels(activity_points@data$ID)) {
  layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE))
  plot(activity_points@data$Elevation[activity_points@data$ID==i], type = "l",bty = "n",  xaxt = "n",ylab = "Elevation", xlab = "", col = "grey70")
  lines(activity_points@data$Lowess_elevation[activity_points@data$ID==i], col = "red", lwd = 3)
  abline(h=mean(activity_points@data$Lowess_elevation[activity_points@data$ID==i]), col = "black", lty =2, lwd=2)
  legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation", "Average elevation"),
         col = c("grey40", "red", "black"), lwd = c(1,3,2), lty=c(1,1,2), bty = "n")
  
  plot(activity_points@data$Km_per_h[activity_points@data$ID==i], type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "", col = "grey70")
  lines(activity_points@data$Lowess_speed[activity_points@data$ID==i], col = "blue", lwd = 3)
  abline(h=mean(activity_points@data$Lowess_speed[activity_points@data$ID==i]), col = "black", lty=2, lwd=2)
  
  legend(x="bottom", legend = c("GPS speed", "LOWESS speed", "Average speed"),
         col = c("grey40", "blue", "black"), lwd = c(1,3, 2),lty=c(1,1,2), bty = "n")
  dev.copy(png,paste0("~/Desktop/studia/dissertation/plots/run_output/" ,i, ".png"),width=16,height=12,units="in",res=900)
  dev.off() 
}

for (i in  levels(activity_points@data$ID)) {
  a <- activity_points@data[activity_points@data$ID==i,]
  mapImageData <- get_googlemap(center = c(lon=((min(a$Longitude)+max(a$Longitude))/2),
                                           lat=((min(a$Latitude)+max(a$Latitude))/2)),zoom=14,scale = 1,
                                maptype = "terrain",style = c(feature = "all", element = "labels", visibility = "off"))
  g1 <- ggmap(mapImageData, extent = "device") +
    geom_point(aes(x = Longitude,y = Latitude, colour=PM25),data = a,alpha = .28,size = 1) + 
    scale_color_gradient(low="blue", high = "red", name="Pace") + 
    theme(legend.position="bottom",plot.margin = unit(c(0,0.1,0,0.1), "cm"))
  g2 <- ggmap(mapImageData, extent = "device") +
    geom_point(aes(x = Longitude,y = Latitude, colour=Lowess_elevation),data = a,alpha = .28,size = 1) + 
    scale_color_gradient(low="green", high = "red", name="Elevation") + 
    theme(legend.position="bottom", plot.margin = unit(c(0,0.1,0,0.1), "cm"))
  
  grid.arrange(g2, g1, nrow = 1)
  dev.copy(png,paste0("~/Desktop/studia/dissertation/plots/run_output/" ,i, "_map.png"),width=16,height=12,units="in",res=900)
  dev.off() 
}

#for all

mapImageData_all <- get_googlemap(zoom = 12,scale = 1,maptype = "terrain",
                                  style = c(feature = "all", element = "labels", visibility = "off"),
                                  center = c(lon = ((min(activity_points@data$Longitude)+max(activity_points@data$Longitude))/2), 
                                             lat = ((min(activity_points@data$Latitude)+max(activity_points@data$Latitude))/2)))
ggmap(mapImageData_all, extent = "device")
g1_all <- ggmap(mapImageData_all, extent = "device") +
  geom_point(aes(x = Longitude,y = Latitude, colour=Lowess_speed),data = activity_points@data,alpha = .28,size = 1) + 
  scale_color_gradient(low="yellow", high = "black", name="Pace") + 
  theme(legend.position="bottom",plot.margin = unit(c(0,0.1,0,0.1), "cm"))

g2_all <- ggmap(mapImageData_all, extent = "device") +
  geom_point(aes(x = Longitude,y = Latitude, colour=Lowess_elevation),data = activity_points@data,alpha = .28,size = 1) + 
  scale_color_gradient(low="green", high = "red", name="Elevation") + 
  theme(legend.position="bottom", plot.margin = unit(c(0,0.1,0,0.1), "cm"))

grid.arrange(g1_all, g2_all, nrow = 1)

dev.off()

ggmap(mapImageData_all, extent="device") +geom_point(aes(x = Longitude,y = Latitude), 
                                                     data=activity_points@data, col="#4d4dff", alpha=.04, size=.75)
