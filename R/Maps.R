setwd("~/Desktop/studia/dissertation/data")
#elevation
library(tmap)
library(rgdal)

run.pollution <- read.csv("run.pollution.csv")
library(sp)
run.pollution.spdf <- SpatialPointsDataFrame(run.pollution[,c("lon", "lat")], run.pollution, proj4string = CRS("+init=epsg:4326"))
run.pollution.spdf <- spTransform(run.pollution.spdf, CRS("+init=epsg:27700"))

WardsMap <- readOGR("Shapefiles/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))

run_map <- WardsMap[!is.na(over(WardsMap, geometry(run.pollution.spdf))),]
run_map <- spTransform(run_map, CRS("+init=epsg:27700"))

#heatmap
tm_shape(run_map) + tm_borders(alpha=.4, lty=2, lwd=1, col="grey80") + tm_shape(run.pollution.spdf) + 
  tm_dots(alpha=.06,col="#0d8de8", size=.03, border.col=NA) + tm_layout(frame=F, bg.color="black") +
  tm_compass(type="8star", size=8) 
dev.copy(png,"~/Desktop/studia/dissertation/plots/heatmap.png",width=16,height=12,units="in",res=900)
dev.off() 

library(londonShapefiles)
library(ggplot2)
thames <- load_thames()
thames.proj <- spTransform(thames, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
thames.df <- fortify(thames.proj)
theme_opts<-list(theme(panel.background = element_blank(),
                       plot.background = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_blank(),
                       panel.grid.major = element_line(colour = 'transparent'),
                       legend.position = "bottom"))

library(dplyr)
library(osmdata)

#heatmap ggplot2
parks <- opq(bbox = c(min(run.pollution[,"lon"])-0.01, min(run.pollution[,"lat"])-0.01,
                      max(run.pollution[,"lon"])+0.01,  max(run.pollution[,"lat"])+0.01)) %>%
  add_osm_feature(key = 'leisure',value = "park") %>% osmdata_sf()

st <- opq(bbox = c(min(run.pollution[,"lon"])-.01, min(run.pollution[,"lat"])-0.01,
                   max(run.pollution[,"lon"])+0.01,  max(run.pollution[,"lat"])+0.01)) %>% 
  add_osm_feature(key = 'highway',value=c("secondary", "primary", "tertiary")) %>% osmdata_sf()

g <-  ggplot() + 
  geom_polygon(thames.df, mapping=aes(long, lat, group = group,fill = I("skyblue")),lwd=0, alpha = 0.8) +
  geom_sf(data=parks$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
  geom_sf(data=parks$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3) + 
  geom_sf(st$osm_lines, mapping=aes(fill = I("grey95")),  lwd = 0.1, alpha=0.2)+
  geom_path(data=run.pollution,aes(lon, lat, group=trackID), colour="tomato",alpha = .5, size=.75, 
            lineend = "round", linejoin = "round") + 
  theme_opts+
  coord_sf(xlim = c(min(run.pollution$lon)-.001, max(run.pollution$lon)+.001), crs="+init=epsg:4326",
           ylim = c(min(run.pollution$lat)-.001, max(run.pollution$lat)+.001), clip="on")
ggsave(g, file="~/Desktop/studia/dissertation/plots/heatmap2.png", width=9, height = 9)

#functions
myfunction_allosm <- function(dataset, variable, legend,filename){
  theme_opts<-list(theme(panel.background = element_blank(),
                         plot.background = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_blank(),
                         panel.grid.major = element_line(colour = 'transparent'), legend.position = "bottom"))
  
  parks <- opq(bbox = c(min(dataset[,"lon"])-0.01, min(dataset[,"lat"])-0.01,
                        max(dataset[,"lon"])+0.01,  max(dataset[,"lat"])+0.01)) %>%
    add_osm_feature(key = 'leisure',
                    value = "park") %>% osmdata_sf()
  
  st <- opq(bbox = c(min(dataset[,"lon"])-.01, min(dataset[,"lat"])-0.01,
                     max(dataset[,"lon"])+0.01,  max(dataset[,"lat"])+0.01)) %>% 
    add_osm_feature(key = 'highway') %>% osmdata_sf()
  
  g <-  ggplot() + 
    geom_polygon(thames.df, mapping=aes(long, lat, group = group,fill = I("skyblue")),lwd=0, alpha = 0.8) +
    geom_sf(data=parks$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
    geom_sf(data=parks$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3) + 
    geom_sf(st$osm_lines, mapping=aes(fill = I("grey95")),  lwd = 0.25, alpha=0.2)+
    geom_path(data=dataset,aes(lon, lat, group=trackID,colour=dataset[,variable]),alpha = 1, size=1.5, 
              lineend = "round", linejoin = "round") + 
    scale_color_gradient(low="blue", high = "red", guide="colourbar", name=legend) +
    theme_opts+
    coord_sf(xlim = c(min(dataset$lon)-.001, max(dataset$lon)+.001), crs="+init=epsg:4326",
             ylim = c(min(dataset$lat)-.001, max(dataset$lat)+.001), clip="on")
  print("Saving")
  ggsave(g, file=paste0("~/Desktop/studia/dissertation/plots/", filename, ".png"), width = 9, height = 9)
}
myfunction_subsetosm <- function(dataset, variable, legend, filename){
  theme_opts<-list(theme(panel.background = element_blank(),
                         plot.background = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_blank(),
                         panel.grid.major = element_line(colour = 'transparent'), legend.position = "bottom"))
  
  parks <- osmdata::opq(bbox = c(min(dataset[,"lon"])-0.01, min(dataset[,"lat"])-0.01,
                                 max(dataset[,"lon"])+0.01,  max(dataset[,"lat"])+0.01)) %>%
    add_osm_feature(key = 'leisure',
                    value = "park") %>% osmdata_sf()
  
  st <- osmdata::opq(bbox = c(min(dataset[,"lon"])-0.01, min(dataset[,"lat"])-0.01,
                              max(dataset[,"lon"])+0.01,  max(dataset[,"lat"])+0.01)) %>% 
    add_osm_feature(key = 'highway',value=c("secondary", "primary", "tertiary")) %>% osmdata_sf()
  
  g <-  ggplot() + 
    geom_polygon(thames.df, mapping=aes(long, lat, group = group,fill = I("skyblue")),lwd=0, alpha = 0.8) +
    geom_sf(data=parks$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
    geom_sf(data=parks$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3)+
    geom_sf(st$osm_lines, mapping=aes(fill = I("grey95")),  lwd = 0.25, alpha=0.2)+
    geom_path(data=dataset,aes(lon, lat, group=trackID,colour=dataset[,variable]),alpha = 1, size=.7, 
              lineend = "round", linejoin = "round") + 
    scale_color_gradient(low="blue", high = "red", guide="colourbar", name=legend)  + 
    theme_opts+
    coord_sf(xlim = c(min(dataset$lon)-.001, max(dataset$lon)+.001), crs="+init=epsg:4326",
             ylim = c(min(dataset$lat)-.001, max(dataset$lat)+.001), clip="on")
  print("Saving")
  ggsave(g, file=paste0("~/Desktop/studia/dissertation/plots/", filename, ".png"), width = 9, height = 9)
}

#samples
sample1 <- subset(run.pollution, trackID==976946527) #primrose hill
sample2 <- subset(run.pollution, trackID==1968008352) #Hampstead Heath
sample3 <- subset(run.pollution, trackID==1842960795) #Hyde Park

#all runs -PM25 intake, PM25 concentration, elevation (google), Lowess slope (google), ventilation
myfunction_subsetosm(run.pollution, variable = "PM25_intake", legend=expression(PM[2.5]*" intake"), filename = "all_PM25_intake")
myfunction_subsetosm(dataset=run.pollution, variable = "PM25",legend = "PM2.5 concentration",filename = "All_PM25_concentration")
myfunction_subsetosm(dataset=run.pollution, variable = "ele_api",legend = "Elevation",filename = "All_ele_api")
myfunction_subsetosm(dataset=run.pollution, variable = "lowess_abs_api_slope",legend = "Lowess Absolute Slope",filename = "All_lowess_abs_api_slope")
myfunction_subsetosm(dataset=run.pollution, variable = "ventilation",legend = "Ventilation Rate",filename = "All_ventilation")

#sample1
sample1$lowess_ele_api <- lowess(sample1$ele_api, f = 0.025)$y
sample1$lowess_speed <- lowess(sample1$speed_km.h, f = 0.08)$y

layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
plot(sample1$ele_api, type = "l",bty = "n",  xaxt = "n",ylab = "Elevation", xlab = "", col = "grey70")
lines(lowess(sample1$ele_api, f = 0.025)$y, col = "red", lwd = 1.5)
legend(x="bottom", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "red", "black"), lwd = c(1,3,2), lty=c(1,1,2), bty = "n")

plot(sample1$speed_km.h, type = "l",bty = "n",  xaxt = "n",ylab = "Speed (km/h)", xlab = "", col = "grey70")
lines(lowess(sample1$speed_km.h, f = 0.08)$y, col = "blue", lwd = 1.5)
legend(x="bottom", legend = c("Speed (km/h)", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), lty=c(1,1), bty = "n")

myfunction_allosm(dataset = sample1, variable = "ele_api", legend = "API Elevation", filename = "sample_ele")
myfunction_allosm(dataset = sample1, variable = "speed_km.h",legend="Speed (km/h)", filename = "sample_speed")
myfunction_allosm(dataset = sample1, variable = "PM25_intake", legend = "PM2.5 intake", filename = "new_sample_PM25_intake")

#sample2
sample2$lowess_ele_api <- lowess(sample2$ele_api, f = 0.025)$y
sample2$lowess_speed <- lowess(sample2$speed_km.h, f = 0.08)$y

plot(sample2$ele_api, type = "l",bty = "n",  xaxt = "n",ylab = "Elevation", xlab = "", col = "grey70")
lines(lowess(sample2$ele_api, f = 0.025)$y, col = "red", lwd = 1.5)
legend(x="bottom", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "red", "black"), lwd = c(1,3,2), lty=c(1,1,2), bty = "n")

plot(sample2$speed_km.h, type = "l",bty = "n",  xaxt = "n",ylab = "Speed (km/h)", xlab = "", col = "grey70")
lines(lowess(sample2$speed_km.h, f = 0.08)$y, col = "blue", lwd = 1.5)
legend(x="bottom", legend = c("Speed (km/h)", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), lty=c(1,1), bty = "n")

myfunction_allosm(dataset = sample2, variable = "ele_api", legend = "API Elevation", filename = "sample2_ele")
myfunction_allosm(dataset = sample2, variable = "speed_km.h",legend="Speed (km/h)", filename = "sample2_speed")
myfunction_allosm(dataset = sample2, variable = "lowess_speed",legend="Lowess Speed (km/h)", filename = "sample2_lowess_speed")
myfunction_allosm(dataset = sample2, variable = "PM25_intake",legend = "PM2.5 intake", filename = "new_sample2_PM25_intake")

#sample3
sample3$lowess_ele_api <- lowess(sample3$ele_api, f = 0.025)$y
sample3$lowess_speed <- lowess(sample3$speed_km.h, f = 0.08)$y

plot(sample3$ele_api, type = "l",bty = "n",  xaxt = "n",ylab = "Elevation", xlab = "", col = "grey70")
lines(lowess(sample3$ele_api, f = 0.025)$y, col = "red", lwd = 1.5)
legend(x="bottom", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "red", "black"), lwd = c(1,3,2), lty=c(1,1,2), bty = "n")

plot(sample3$speed_km.h, type = "l",bty = "n",  xaxt = "n",ylab = "Speed (km/h)", xlab = "", col = "grey70")
lines(lowess(sample3$speed_km.h, f = 0.08)$y, col = "blue", lwd = 1.5)
legend(x="bottom", legend = c("Speed (km/h)", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), lty=c(1,1), bty = "n")

myfunction_allosm(dataset = sample3, variable = "ele_api", legend = "API Elevation", filename = "sample3_ele")
myfunction_allosm(dataset = sample3, variable = "speed_km.h",legend="Speed (km/h)", filename = "sample3_speed")
myfunction_allosm(dataset = sample3, variable = "lowess_speed",legend="Lowess Speed (km/h)", filename = "sample3_lowess_speed")
myfunction_allosm(dataset = sample3, variable = "PM25_intake",legend = "PM2.5 intake", filename = "new_sample3_PM25_intake")