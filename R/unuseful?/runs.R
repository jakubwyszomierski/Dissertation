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

test <- readOGR("strava/test.shp")
test <- spTransform(test, CRS("+init=epsg:27700"))

#
system.time(osm_buf <- readOGR("strava/buf.shp"))
osm_buf <- spTransform(osm_buf, CRS("+init=epsg:27700"))
identicalCRS(osm_buf, test)

#snapping
plot(osm_buf)
plot(test,add=T, col="red", cex=0.01)
system.time(ptl <- snapPointsToLines(test, osm_buf, maxDist = 10))

#negative length vectors are not allowed - it might be possible that these two objects are too big
#let's try one track at a time
out=NULL
system.time(for(i in levels(test@data$trackID)){
  test2 <- test[test@data$trackID==i,]
  print(i)
  buf <- gUnaryUnion(gBuffer(test2, width=40, byid=TRUE))
  osm_buf2 <- osm_buf[buf,]
  ptl2 <- snapPointsToLines(test2, osm_buf2, maxDist = 10)
  test2@data$X <- test2@coords[,1]
  test2@data$Y <- test2@coords[,2]
  ptl2@data$ptlX <- ptl2@coords[,1]
  ptl2@data$ptlY <- ptl2@coords[,2]
  m <- merge(test2@data, ptl2@data, by="pointID", all=T)
  m <- m[,c("trackID.x", "pointID", "time.x", "ele.x", "X", "Y", "nearest_line_id", "ptlX", "ptlY")]
  m$ptlX <- ifelse(is.na(m$ptlX), m$X, m$ptlX)
  m$ptlY <- ifelse(is.na(m$ptlY), m$Y, m$ptlY)
  m <- m[,-which(names(m) %in% c("X","Y"))]
  out=rbind(out, m)
}) #1334s

rm(test2, buf, osm_buf2, ptl2, m)
names(out) <-  c("trackID","pointID", "time", "ele", "nearest_line_id", "E", "N")
out$pointID <- as.numeric(as.character(out$pointID))
out <- out[order(out$pointID),]
rownames(out) <- out$pointID
View(out)

outsp <- SpatialPointsDataFrame(out[c("E", "N")], data=out, proj4string = CRS("+init=epsg:27700"))
plot(o)
plot(outsp, col="red", pch=16, cex=1/4, add=T)

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
for (i in levels(outsp$trackID)) {
  print(i)
  outsp <- spTransform(outsp, CRS("+init=epsg:4326"))
  geodf <- outsp[outsp@data$trackID==i,]
  geodf$lon <- geodf@coords[,1]
  geodf$lat <- geodf@coords[,2]  
  geodf <- geodf@data
  class(geodf)
  geodf$time <- strptime(geodf$time, format = "%Y/%m/%d %H:%M:%OS")
  #Shift vectors for lat and lon so that each row also contains the next position.
  geodf$lat.n1 <- shift.vec(geodf$lat, -1)
  geodf$lon.n1 <- shift.vec(geodf$lon, -1)
  #here we assign the same lat and long for missing n1 coordinates to account for stop
  geodf$lat.n1 <- ifelse(is.na(geodf$lat.n1), geodf$lat, geodf$lat.n1)
  geodf$lon.n1 <- ifelse(is.na(geodf$lon.n1), geodf$lon, geodf$lon.n1)
  geodf$dist.to.prev <- apply(geodf, 1, FUN = function (row) {
    pointDistance(c(as.numeric(row["lat.n1"]),
                    as.numeric(row["lon.n1"])),
                  c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                  lonlat = T)
  })
  geodf$time.n1 <- shift.vec(geodf$time, -1)
  geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.n1, geodf$time))
  #speed
  geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
  geodf$speed.m.per.sec <- ifelse(is.na(geodf$speed.m.per.sec), 0, geodf$speed.m.per.sec)
  geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
  geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
  geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
  #gradient
  geodf$ele.n1 <- shift.vec(geodf$ele, -1)
  geodf$ele.n1 <- ifelse(is.na(geodf$ele.n1), geodf$ele, geodf$ele.n1)
  geodf$slope <- (geodf$ele.n1-geodf$ele)/geodf$dist.to.prev
  geodf$slope <-   geodf$slope*100
  geodf$slope <- ifelse(is.na(geodf$slope), 0, geodf$slope)
  geodf$slope <- ifelse(geodf$slope==Inf, 0, geodf$slope  )
  geodf$slope  <- ifelse(geodf$slope==-Inf, 0, geodf$slope  )
  geodf$lowess_slope <- lowess(geodf$slope, f=0.02)$y
  p=rbind(p, geodf)
}
head(p)
summary(p)

runs <- SpatialPointsDataFrame(p[c("lon", "lat")], p, proj4string = CRS("+init=epsg:4326"))
plot(osm_buf)
plot(spTransform(runs, CRS("+init=epsg:27700")), add=T, col="red",cex=0.25)

rgdal::writeOGR(runs, ".", layer="strava/runs", driver = "ESRI Shapefile", morphToESRI=T)
