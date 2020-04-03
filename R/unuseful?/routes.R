setwd("~/Desktop/studia/dissertation/data")
library(plotKML)
library(XML)
library(OpenStreetMap)
library(lubridate)
library(raster)
library(ggplot2)

readGPX("strava/last_export_just_strava/activities/1578337586.gpx")

plots.function <- function(gpx.file){
  pfile <- htmlTreeParse(gpx.file, error = function(...) {}, useInternalNodes = T)
  elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
  times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
  coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
  lats <- as.numeric(coords["lat",])
  lons <- as.numeric(coords["lon",])
  geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
  rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords"))
  head(geodf)
  geodf$time <- strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")
  nrow(geodf)
  range(as.character(geodf$time))
  library(chron)
  geodf$ctime <- as.chron(as.character(geodf$time))
  shift.vec <- function (vec, shift) {
    if(length(vec) <= abs(shift)) {
      rep(NA ,length(vec))
    }else{
      if (shift >= 0) {
        c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
      else {
        c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }
  geodf$lat.p1 <- shift.vec(geodf$lat, -1)
  geodf$lon.p1 <- shift.vec(geodf$lon, -1)
  geodf$dist.to.prev <- apply(geodf, 1, FUN = function (row) {
    pointDistance(c(as.numeric(row["lat.p1"]),
                    as.numeric(row["lon.p1"])),
                  c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                  lonlat = T)
  })
  geodf$time.p1 <- shift.vec(geodf$time, -1)
  geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))
  geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
  geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
  geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
  geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
  geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y
  
  #plots
  par(mfrow = c(1, 2))
  plot(geodf$ele, type = "l",bty = "n",  xaxt = "n",ylab = "Elevation", xlab = "", col = "grey40")
  lines(geodf$lowess.ele, col = "red", lwd = 3)
  legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation"),
         col = c("grey40", "red"), lwd = c(1,3), bty = "n")
  
  plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "", col = "grey40")
  lines(geodf$lowess.speed, col = "blue", lwd = 3)
  legend(x="bottom", legend = c("GPS speed", "LOWESS speed"),
         col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
  abline(h = mean(geodf$speed.km.per.h), lty = 2, col = "blue")
}

plots.function("1578337586.gpx")
# Parse the GPX file
pfile <- htmlTreeParse("1578337586.gpx", error = function(...) {}, useInternalNodes = T)
elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])
geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords"))
head(geodf)
geodf$time <- strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")
nrow(geodf)
range(as.character(geodf$time))
library(chron)
geodf$ctime <- as.chron(as.character(geodf$time))
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }
geodf$lat.p1 <- shift.vec(geodf$lat, -1)
geodf$lon.p1 <- shift.vec(geodf$lon, -1)
geodf$dist.to.prev <- apply(geodf, 1, FUN = function (row) {
  pointDistance(c(as.numeric(row["lat.p1"]),
                  as.numeric(row["lon.p1"])),
                c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                lonlat = T)
})
geodf$time.p1 <- shift.vec(geodf$time, -1)
geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))
geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y

#plots
par(mfrow = c(1, 2))
plot(geodf$ele, type = "l",bty = "n",  xaxt = "n",ylab = "Elevation", xlab = "", col = "grey40")
lines(geodf$lowess.ele, col = "red", lwd = 3)
legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "red"), lwd = c(1,3), bty = "n")

plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "", col = "grey40")
lines(geodf$lowess.speed, col = "blue", lwd = 3)
legend(x="bottom", legend = c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
abline(h = mean(geodf$speed.km.per.h), lty = 2, col = "blue")


    