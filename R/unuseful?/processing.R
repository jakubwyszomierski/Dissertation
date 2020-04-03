#data processing part 
	#snapping to line
	#calculating slopes, elevations etc. 

#downloading elevation data from API

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


runs <- readOGR("MyData/runs.shp")
runs <- spTransform(runs, CRS("+init=epsg:27700"))

system.time(osm_buf <- readOGR("MyData/buf.shp"))
osm_buf <- spTransform(osm_buf, CRS("+init=epsg:27700"))
identicalCRS(osm_buf, runs)

#snapping
plot(osm_buf, col=randomColor())
plot(runs,add=T, col=randomColor(), cex=0.01)
system.time(ptl <- snapPointsToLines(runs, osm_buf, maxDist = 10))

#negative length vectors are not allowed - it might be becasue these two objects are too big
#let's try one track at a time

snapped=NULL
system.time(for(i in levels(runs@data$trackID)){
  single_run <- runs[runs@data$trackID==i,]
  print(i)
  buf <- gUnaryUnion(gBuffer(single_run, width=50, byid=TRUE)) 
  osm_buf2 <- osm_buf[buf,]
  ptl2 <- snapPointsToLines(single_run, osm_buf2, maxDist = 10)
  single_run@data$X <- single_run@coords[,1]
  single_run@data$Y <- single_run@coords[,2]
  ptl2@data$ptlX <- ptl2@coords[,1]
  ptl2@data$ptlY <- ptl2@coords[,2]
  m <- merge(single_run@data, ptl2@data, by="pointID", all=T)
  m <- m[,c("trackID.x", "pointID", "time.x", "ele.x", "X", "Y", "nearest_line_id", "ptlX", "ptlY")]
  m$ptlX <- ifelse(is.na(m$ptlX), m$X, m$ptlX)
  m$ptlY <- ifelse(is.na(m$ptlY), m$Y, m$ptlY)
  m <- m[,-which(names(m) %in% c("X","Y"))]
  snapped=rbind(snapped, m)
}) #1334s

rm(single_run, buf, osm_buf2, ptl2, m)
names(snapped) <-  c("trackID","pointID", "time", "ele", "nearest_line_id", "E", "N")
snapped$pointID <- as.numeric(as.character(snapped$pointID))
snapped <- snapped[order(snapped$pointID),]
rownames(snapped) <- snapped$pointID

snapped_sp <- SpatialPointsDataFrame(snapped[c("E", "N")], data=snapped, proj4string = CRS("+init=epsg:27700"))
snapped_sp2 <- spTransform(snapped_sp[snapped_sp@data$trackID==1136959124,], CRS("+init=epsg:4326"))
plot(spTransform(snapped_sp2, CRS("+init=epsg:27700")), col="red")
#the plot shows that there are great outliers; to disregard such cases, i will subset the data to contain points with less than 30km/h speed 
library(londonShapefiles)
library(ggplot2)
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


parks <- opq(bbox = c(min(snapped_sp2@coords[,"E"])-0.01, min(snapped_sp2@coords[,"N"])-0.01,
                      max(snapped_sp2@coords[,"E"])+0.01,  max(snapped_sp2@coords[,"N"])+0.01)) %>%
  add_osm_feature(key = 'leisure',
                  value = "park") %>% osmdata_sf()

st <- opq(bbox = c(min(snapped_sp2@coords[,"E"])-0.01, min(snapped_sp2@coords[,"N"])-0.01,
                   max(snapped_sp2@coords[,"E"])+0.01,  max(snapped_sp2@coords[,"N"])+0.01)) %>% 
  add_osm_feature(key = 'highway') %>% osmdata_sf()

g <- ggplot() + 
  geom_sf(data=parks$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
  geom_sf(data=parks$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3) + 
  geom_sf(st$osm_lines, mapping=aes(fill = I("grey95")),  lwd=0.2, alpha=0.2)+
  geom_point(data=snapped_sp2@data,aes(snapped_sp2@coords[,"E"], snapped_sp2@coords[,"N"]), 
             colour="tomato",alpha = 1, size=1.25) + theme_opts +
  coord_sf(xlim = c(min(snapped_sp2@coords[,"E"])-.0025, max(snapped_sp2@coords[,"E"])+.0025), crs="+init=epsg:4326",
           ylim = c(min(snapped_sp2@coords[,"N"])-.0025, max(snapped_sp2@coords[,"N"])+.0025), clip="on")

ggsave(g, file="~/Desktop/studia/dissertation/plots/map1.png")

g2 <- ggplot() + 
  geom_sf(data=parks$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
  geom_sf(data=parks$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3) + 
  geom_sf(st$osm_lines, mapping=aes(fill = I("grey95")),  lwd=0.2, alpha=0.2)+
  geom_path(data=snapped_sp2@data,aes(snapped_sp2@coords[,"E"], snapped_sp2@coords[,"N"]), 
             colour="tomato",alpha = 1, size=1.25, lineend = "round", linejoin = "round") + theme_opts +
  coord_sf(xlim = c(min(snapped_sp2@coords[,"E"])-.0025, max(snapped_sp2@coords[,"E"])+.0025), crs="+init=epsg:4326",
           ylim = c(min(snapped_sp2@coords[,"N"])-.0025, max(snapped_sp2@coords[,"N"])+.0025), clip="on")

ggsave(g2, file="~/Desktop/studia/dissertation/plots/map2.png")


#i will deal with it later

#multiple runs output
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

p=NULL
for (i in levels(snapped_sp$trackID)) {
  print(i)
  snapped_sp <- spTransform(snapped_sp, CRS("+init=epsg:4326"))
  geodf <- snapped_sp[snapped_sp@data$trackID==i,]
  geodf$lon <- geodf@coords[,1]
  geodf$lat <- geodf@coords[,2]  
  geodf <- geodf@data
  geodf$time <- strptime(geodf$time, format = "%Y/%m/%d %H:%M:%OS")
  #Shift vectors for lat and lon so that each row also contains the next position.
  geodf$lat.n1 <- shift.vec(geodf$lat, -1)
  geodf$lon.n1 <- shift.vec(geodf$lon, -1)
  #here we assign the same lat and long for missing n1 coordinates to account for stop
  geodf$lat.n1 <- ifelse(is.na(geodf$lat.n1), geodf$lat, geodf$lat.n1)
  geodf$lon.n1 <- ifelse(is.na(geodf$lon.n1), geodf$lon, geodf$lon.n1)
  geodf$dist.to.next <- apply(geodf, 1, FUN = function (row) {
    pointDistance(c(as.numeric(row["lat.n1"]),
                    as.numeric(row["lon.n1"])),
                  c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                  lonlat = T)
  })
  while(min(geodf$dist.to.next)<(1)|max(geodf$dist.to.next)>50) {
    geodf <- geodf[geodf$dist.to.next>1,]
    rownames(geodf) <- 1:nrow(geodf)
    geodf$pointID <- rownames(geodf)
    geodf$lat.n1 <- shift.vec(geodf$lat, -1)
    geodf$lon.n1 <- shift.vec(geodf$lon, -1)
    geodf$lat.n1 <- ifelse(is.na(geodf$lat.n1), geodf$lat, geodf$lat.n1)
    geodf$lon.n1 <- ifelse(is.na(geodf$lon.n1), geodf$lon, geodf$lon.n1)
  }
    geodf$dist.to.next <- apply(geodf, 1, FUN = function (row) {
      pointDistance(c(as.numeric(row["lat.n1"]),
                      as.numeric(row["lon.n1"])),
                    c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                    lonlat = T) })
    while(min(geodf$dist.to.next)<(1)) {
      geodf <- geodf[geodf$dist.to.next>1,]
      rownames(geodf) <- 1:nrow(geodf)
      geodf$pointID <- rownames(geodf)
      geodf$lat.n1 <- shift.vec(geodf$lat, -1)
      geodf$lon.n1 <- shift.vec(geodf$lon, -1)
      geodf$lat.n1 <- ifelse(is.na(geodf$lat.n1), geodf$lat, geodf$lat.n1)
      geodf$lon.n1 <- ifelse(is.na(geodf$lon.n1), geodf$lon, geodf$lon.n1)
    }
    geodf$dist.to.next <- apply(geodf, 1, FUN = function (row) {
      pointDistance(c(as.numeric(row["lat.n1"]),
                      as.numeric(row["lon.n1"])),
                    c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                    lonlat = T) })
  geodf$time.n1 <- shift.vec(geodf$time, -1)
  geodf$time.diff.to.next <- as.numeric(difftime(geodf$time.n1, geodf$time))
  #speed
  geodf$speed.m.per.sec <- geodf$dist.to.next / geodf$time.diff.to.next
  geodf$speed.m.per.sec <- ifelse(is.na(geodf$speed.m.per.sec), 0, geodf$speed.m.per.sec)
  geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
  geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
  
  #speed is extremely high in some cases (e.g. 500km/h). this indicated GPS outliers. to get rid of them,
  #i will exclude points with speed higher than 40km/h (World recrod - Usain Bolt)
  while(max(geodf$speed.km.per.h)>30){
    geodf <- geodf[geodf$speed.km.per.h<30,]
    rownames(geodf) <- 1:nrow(geodf)
    geodf$pointID <- rownames(geodf)
    #Shift vectors for lat and lon so that each row also contains the next position.
    geodf$lat.n1 <- shift.vec(geodf$lat, -1)
    geodf$lon.n1 <- shift.vec(geodf$lon, -1)
    #here we assign the same lat and long for missing n1 coordinates to account for stop
    geodf$lat.n1 <- ifelse(is.na(geodf$lat.n1), geodf$lat, geodf$lat.n1)
    geodf$lon.n1 <- ifelse(is.na(geodf$lon.n1), geodf$lon, geodf$lon.n1)
    geodf$dist.to.next <- apply(geodf, 1, FUN = function (row) {
      pointDistance(c(as.numeric(row["lat.n1"]),
                      as.numeric(row["lon.n1"])),
                    c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                    lonlat = T)
    })
    geodf$time.n1 <- shift.vec(geodf$time, -1)
    geodf$time.diff.to.next <- as.numeric(difftime(geodf$time.n1, geodf$time))
    #speed
    geodf$speed.m.per.sec <- geodf$dist.to.next / geodf$time.diff.to.next
    geodf$speed.m.per.sec <- ifelse(is.na(geodf$speed.m.per.sec), 0, geodf$speed.m.per.sec)
    geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
    geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
  }
  #geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
  geodf$ele.n1 <- shift.vec(geodf$ele, -1)
  geodf$ele.n1 <- ifelse(is.na(geodf$ele.n1), geodf$ele, geodf$ele.n1)
  geodf$slope <- (geodf$ele.n1-geodf$ele)/geodf$dist.to.next
  geodf$slope <-   geodf$slope*100
  geodf$slope <- ifelse(is.na(geodf$slope), 0, geodf$slope)
  geodf$slope <- ifelse(geodf$slope==Inf, 0, geodf$slope)
  geodf$slope  <- ifelse(geodf$slope==-Inf, 0, geodf$slope)
  
  while(max(geodf$slope)>150 |  min(geodf$slope)<(-150)){
    geodf <- geodf[geodf$slope<150&geodf$slope>(-150),]
    rownames(geodf) <- 1:nrow(geodf)
    geodf$pointID <- rownames(geodf)
    #Shift vectors for lat and lon so that each row also contains the next position.
    geodf$lat.n1 <- shift.vec(geodf$lat, -1)
    geodf$lon.n1 <- shift.vec(geodf$lon, -1)
    #here we assign the same lat and long for missing n1 coordinates to account for stop
    geodf$lat.n1 <- ifelse(is.na(geodf$lat.n1), geodf$lat, geodf$lat.n1)
    geodf$lon.n1 <- ifelse(is.na(geodf$lon.n1), geodf$lon, geodf$lon.n1)
    geodf$dist.to.next <- apply(geodf, 1, FUN = function (row) {
      pointDistance(c(as.numeric(row["lat.n1"]),
                      as.numeric(row["lon.n1"])),
                    c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                    lonlat = T)
    })
    geodf$time.n1 <- shift.vec(geodf$time, -1)
    geodf$time.diff.to.next <- as.numeric(difftime(geodf$time.n1, geodf$time))
    #speed
    geodf$speed.m.per.sec <- geodf$dist.to.next / geodf$time.diff.to.next
    geodf$speed.m.per.sec <- ifelse(is.na(geodf$speed.m.per.sec), 0, geodf$speed.m.per.sec)
    geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
    geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
    #geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
    geodf$ele.n1 <- shift.vec(geodf$ele, -1)
    geodf$ele.n1 <- ifelse(is.na(geodf$ele.n1), geodf$ele, geodf$ele.n1)
    geodf$slope <- (geodf$ele.n1-geodf$ele)/geodf$dist.to.next
    geodf$slope <- geodf$slope*100
    geodf$slope <- ifelse(is.na(geodf$slope), 0, geodf$slope)
    geodf$slope <- ifelse(geodf$slope==Inf, 0, geodf$slope  )
    geodf$slope  <- ifelse(geodf$slope==-Inf, 0, geodf$slope)
  }
  #geodf$lowess_slope <- lowess(geodf$slope, f=0.005)$y
  p=rbind(p, geodf)
}
levels(runs@data$trackID)
head(p)
summary(p)
nrow(p)
rownames(p) <- 1:nrow(p)
p$pointID <- rownames(p)

output <- SpatialPointsDataFrame(p[c("lon", "lat")], p, proj4string = CRS("+init=epsg:4326"))

nrow(runs)-nrow(output)
(nrow(runs)-nrow(output))/nrow(runs)

#comapring raw data with snapped to line points
plot(spTransform(snapped_sp[snapped_sp@data$trackID==1136959124,], CRS("+init=epsg:27700")), col="red")
plot(spTransform(output[output@data$trackID==1136959124,], CRS("+init=epsg:27700")),  col=randomColor(),cex=0.25,add=T)


output@data <- output@data[c("trackID", "pointID","lon", "lat", "speed.km.per.h", "speed.m.per.sec","time", "time.diff.to.next",
                     "dist.to.next", "ele", "ele.n1", "slope")]

library(rgbif)
api_key <- 'AIzaSyDLR4SY0RjHRWVJxV8_GTYzosK7h-PbO-U'
options(scipen = 999) #this is essential for code to run

output$ele_api <- elevation(longitude = output$lon, latitude  = output$lat, key=api_key)$elevation
output$ele_api.n1 <- shift.vec(output$ele_api, -1)
output$ele_api.n1 <- ifelse(is.na(output$ele_api.n1), output$ele_api, output$ele_api.n1)

names(output@data) <- c("trackID", "pointID", "lon", "lat", "speed_km/h", "spped_m/s", "time", 
                        "time_diff","dist_to_next", "ele", "ele_n1", "slope", "ele_api", "ele_api.n1")

#slope api
output$api_slope <- (output$ele_api.n1-output$ele_api)/output$dist_to_next
output$api_slope <- output$api_slope*100
output$api_slope <- ifelse(is.na(output$api_slope), 0, output$api_slope)
output$api_slope <- ifelse(output$api_slope==Inf, 0, output$api_slope)
output$api_slope <- ifelse(output$api_slope==-Inf, 0, output$api_slope)

#45136 --> weird jump of elevation from 19 to 30; as a result, slope equals -300
output <- output[output@data$api_slope>(-150)&output@data$api_slope<150,]
summary(output@data)

#lowess to smooth the results 
lowess=NULL
for (i in levels(output$trackID)) {
  output2 <- output@data[output@data$trackID==i,]
  # not sure if absolute values are the right choice --> it would mean that one have to climb all of the time
  # on the other hand, each hill can be run uphill and downhill
  output2$lowess_api_slope <- lowess(output2$api_slope, f=0.001)$y
  output2$abs_api_slope <- abs(output2$api_slope)
  output2$lowess_abs_api_slope <- lowess(output2$abs_api_slope, f=0.001)$y
  lowess=rbind(lowess, output2)
  print(i)
}
summary(lowess)
View(lowess)

#api elevation is much more precise
sd(lowess$api_slope)
sd(lowess$slope)

sd(lowess$ele_api)
sd(lowess$ele)

lowess_sp <- SpatialPointsDataFrame(lowess[c("lon", "lat")], lowess, proj4string = CRS("+init=epsg:4326"))

rgdal::writeOGR(lowess_sp, ".", layer="MyData/output", driver = "ESRI Shapefile", morphToESRI=T)
