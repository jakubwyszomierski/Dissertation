8setwd("~/Desktop/studia/dissertation/data")
library(rgdal)
library(tmap)
library(raster)
library(sp)
library(spatstat)
library(rgeos)
library(chron)

#load data
WardsMap <- readOGR("statistical-gis-boundaries-london/ESRI/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:4326"))

shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

#me
K_info <- read.csv("strava/last_export_just_strava/activities.csv")
info1 <- read.csv("strava/Tomasz/activities.csv")
info2 <- read.csv("strava/Anson/activities.csv")
info3 <- read.csv("strava/Maria/activities.csv")
info4 <- read.csv("strava/Becky/activities.csv")

#merge
info <- rbind(K_info, info1, info2, info3, info4)
file_list <- list.files("strava/all")
file_list

rm(activity_points)

#points
for (i in file_list){
  # if the merged activity doesn't exist, create it
  if (!exists("activity_points")){
    activity_points <- readOGR(dsn=paste0("strava/all/", i), layer = "track_points")
    activity_points$id <-  gsub("*.gpx", "",i)
    geodf <- activity_points@data
    geodf$lat <- activity_points@coords[,2]
    geodf$lon <- activity_points@coords[,1]
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
    #elevation
    geodf$ele.n1 <- shift.vec(geodf$ele, -1)
    geodf$ele.n1 <- ifelse(is.na(geodf$ele.n1), geodf$ele, geodf$ele.n1)
    geodf$slope <- (geodf$ele.n1-geodf$ele)/geodf$dist.to.prev*100
    geodf$slope <- ifelse(is.na(geodf$slope), 0, geodf$slope)
    geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y
    activity_points@data <- geodf
  }
  # if the merged activity does exist, append to it
  if (exists("activity_points")){
    temp_dataset <-readOGR(dsn=paste0("strava/all/", i), layer = "track_points")
    temp_dataset$id <- gsub("*.gpx", "",i)
    geodf <- temp_dataset@data
    geodf$lat <- temp_dataset@coords[,2]
    geodf$lon <- temp_dataset@coords[,1]
    geodf$time <- strptime(geodf$time, format = "%Y/%m/%d %H:%M:%OS")
    geodf$lat.n1 <- shift.vec(geodf$lat, -1)
    geodf$lon.n1 <- shift.vec(geodf$lon, -1)
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
    geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
    geodf$speed.m.per.sec <- ifelse(is.na(geodf$speed.m.per.sec), 0, geodf$speed.m.per.sec)
    geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
    geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
    geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
    geodf$ele.n1 <- shift.vec(geodf$ele, -1)
    geodf$ele.n1 <- ifelse(is.na(geodf$ele.n1), geodf$ele, geodf$ele.n1)
    geodf$slope <- (geodf$ele.n1-geodf$ele)/geodf$dist.to.prev*100
    geodf$slope <- ifelse(is.na(geodf$slope), 0, geodf$slope)
    geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y
    temp_dataset@data <- geodf
    activity_points<-bind(activity_points, temp_dataset)
    rm(temp_dataset)
    rm(geodf)
  }
}
#duplication of the first file
activity_points <- activity_points[-c(1:1895),]

View(activity_points@data[,c("time", "lat", "lon", "lat.n1", "lon.n1", "ele", "ele.n1", "dist.to.prev", "slope")])

#subset to London
activity_points <- spTransform(activity_points, CRS("+init=epsg:4326"))
activity_points <- activity_points[WardsMap,]
plot(activity_points)

activity_points@data$id <- as.character(activity_points@data$id)
activity_points <- merge(activity_points, info, by="id")
names(activity_points)
activity_points <- activity_points[,c("id",  "name.y", "type.y", "elapsed_time", "distance", "commute","lat", "lon", "time", 
                                      "lat.n1", "lon.n1","time.n1", "dist.to.prev" , "time.diff.to.prev", "speed.m.per.sec",
                                      "speed.km.per.h","lowess.speed","ele","ele.n1", "lowess.ele")]
names(activity_points) <- c("ID", "Name", "Type", "Elapsed_time", "Distance", "Commute", "Latitude", "Longitude", "Time", 
                            "Latitude.n1", "Longitude.n1", "Dist_to_prev", "Time.n1", "Time_diff_to_prev", "M_per_sec",
                            "Km_per_h", "Lowess_speed",  "Elevation","Elevation.n1", "Lowess_elevation")
head(activity_points)

rgdal::writeOGR(activity_points, ".", layer="strava/activity_point", driver = "ESRI Shapefile", morphToESRI=T)

#lines
for (i in file_list){
  # if the merged activity doesn't exist, create it
  if (!exists("activity_lines")){
    activity_lines <- readOGR(dsn=paste0("strava/all/", i), layer = "tracks")
  }
  # if the merged activity does exist, append to it
  if (exists("activity_lines")){
    temp_dataset <-readOGR(dsn=paste0("strava/all/", i), layer = "tracks")
    activity_lines<-rbind(activity_lines, temp_dataset)
    rm(temp_dataset)
  }
}

activity_lines <-  activity_lines[-1,]
plot(activity_lines)

#subset to London
plot(WardsMap)
plot(activity_lines,add=T, col="red")
setwd("~/Desktop/studia/dissertation/data")

rgdal::writeOGR(activity_lines, ".", layer="strava/activity_lines", driver = "ESRI Shapefile", morphToESRI=T)
