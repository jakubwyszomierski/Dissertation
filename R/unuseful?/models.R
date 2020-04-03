setwd("~/Desktop/studia/dissertation/data")

library(rgdal)
activity_points <- readOGR("strava/activity_points.shp")
activity_points <- spTransform(activity_points, CRS("+init=epsg:27700"))

#load data
WardsMap <- readOGR("statistical-gis-boundaries-london/ESRI/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))
run_map <- WardsMap[!is.na(over(WardsMap, geometry(activity_points))),]
run_map <- spTransform(run_map, CRS("+init=epsg:27700"))

il <- c("2020", "2025", "2030")
jl <- list("PM10", "PM10d", "PM25", "NOx", "NO2")

for (i in il) {
  for (j in jl) {
  assign(paste0(j,"_",i), read.csv(paste0("LAEI/4_Concentrations/Excel/", i, "_Met2013_LAEI2013_",j,".csv")))
  }
  PM10s <- merge(paste0("PM10_",i), paste0("PM10d_",i), by=c("x", "y", "year"))
  PMs <- merge(paste0("PM25_",i), PM10s, by=c("x", "y", "year"))
  NOs <- merge(NOx, NO2, by=c("x", "y", "year"))
  assign(paste0("pollutants_", i), merge(PMs, NOs, by=c("x", "y", "year")))
  names(paste0("pollutants_i")) <- c("Easting", "Northing", "Year", "PM25", "PM10", "PM10d", "NOx", "NO2")
}

#2020
PM10<- read.csv("LAEI/4_Concentrations/Excel/2020_Met2013_LAEI2013_PM10.csv")
PM10d<- read.csv("LAEI/4_Concentrations/Excel/2020_Met2013_LAEI2013_PM10d.csv")
PM25<- read.csv("LAEI/4_Concentrations/Excel/2020_Met2013_LAEI2013_PM25.csv")
NOx<- read.csv("LAEI/4_Concentrations/Excel/2020_Met2013_LAEI2013_NOx.csv")
NO2<- read.csv("LAEI/4_Concentrations/Excel/2020_Met2013_LAEI2013_NO2.csv")

head(data.frame(PM10, PM10d, PM25, NOx, NO2))

PM10s <- merge(PM10, PM10d, by=c("x", "y", "year"))
PMs<- merge(PM25, PM10s, by=c("x", "y", "year"))
NOs <- merge(NOx, NO2, by=c("x", "y", "year"))
pollutants_2020 <- merge(PMs, NOs, by=c("x", "y", "year"))
names(pollutants_2020) <- c("Easting", "Northing","Year", "PM25", "PM10", "PM10d", "NOx", "NO2")
summary(pollutants_2020)

for (i in names(pollutants_2020)[-c(1,2)]) {
  grid <- pollutants_2020[, c("Easting", "Northing", i)]
  coordinates(grid) <- ~Easting+Northing
  proj4string(grid) <- CRS("+init=epsg:27700")
  grid <- grid[run_map,]
  grid <- as(grid, "SpatialPixelsDataFrame")
  assign(paste0(i, ".2020raster"), raster(grid, layer=1))
  rm(grid)
}

#2025
PM10<- read.csv("LAEI/4_Concentrations/Excel/2025_Met2013_LAEI2013_PM10.csv")
PM10d<- read.csv("LAEI/4_Concentrations/Excel/2025_Met2013_LAEI2013_PM10d.csv")
PM25<- read.csv("LAEI/4_Concentrations/Excel/2025_Met2013_LAEI2013_PM25.csv")
NOx<- read.csv("LAEI/4_Concentrations/Excel/2025_Met2013_LAEI2013_NOx.csv")
NO2<- read.csv("LAEI/4_Concentrations/Excel/2025_Met2013_LAEI2013_NO2.csv")

head(data.frame(PM10, PM10d, PM25, NOx, NO2))


PM10s <- merge(PM10, PM10d, by=c("x", "y", "year"))
PMs<- merge(PM25, PM10s, by=c("x", "y", "year"))
NOs <- merge(NOx, NO2, by=c("x", "y", "year"))
pollutants_2025 <- merge(PMs, NOs, by=c("x", "y", "year"))
names(pollutants_2025) <- c("Easting", "Northing","Year", "PM25", "PM10", "PM10d", "NOx", "NO2")
summary(pollutants_2025)

for (i in names(pollutants_2025)[-c(1,2)]) {
  grid <- pollutants_2025[, c("Easting", "Northing", i)]
  coordinates(grid) <- ~Easting+Northing
  proj4string(grid) <- CRS("+init=epsg:27700")
  grid <- grid[run_map,]
  grid <- as(grid, "SpatialPixelsDataFrame")
  assign(paste0(i, ".2025raster"), raster(grid, layer=1))
  rm(grid)
}

#2030
PM10<- read.csv("LAEI/4_Concentrations/Excel/2030_Met2013_LAEI2013_PM10.csv")
PM10d<- read.csv("LAEI/4_Concentrations/Excel/2030_Met2013_LAEI2013_PM10d.csv")
PM25<- read.csv("LAEI/4_Concentrations/Excel/2030_Met2013_LAEI2013_PM25.csv")
NOx<- read.csv("LAEI/4_Concentrations/Excel/2030_Met2013_LAEI2013_NOx.csv")
NO2<- read.csv("LAEI/4_Concentrations/Excel/2030_Met2013_LAEI2013_NO2.csv")

head(data.frame(PM10, PM10d, PM25, NOx, NO2))


PM10s <- merge(PM10, PM10d, by=c("x", "y", "year"))
PMs<- merge(PM25, PM10s, by=c("x", "y", "year"))
NOs <- merge(NOx, NO2, by=c("x", "y", "year"))
pollutants_2030 <- merge(PMs, NOs, by=c("x", "y", "year"))
names(pollutants_2030) <- c("Easting", "Northing","Year", "PM25", "PM10", "PM10d", "NOx", "NO2")
summary(pollutants_2030)

for (i in names(pollutants_2030)[-c(1,2)]) {
  grid <- pollutants_2030[, c("Easting", "Northing", i)]
  coordinates(grid) <- ~Easting+Northing
  proj4string(grid) <- CRS("+init=epsg:27700")
  grid <- grid[run_map,]
  grid <- as(grid, "SpatialPixelsDataFrame")
  assign(paste0(i, ".2030raster"), raster(grid, layer=1))
  rm(grid)
}

p <- data.frame(activity_points@data,extract(PM25.2020raster, activity_points, method="bilinear"),
                extract(PM25.2025raster, activity_points, method="bilinear"),
                extract(PM25.2030raster, activity_points, method="bilinear"),
                extract(PM10.2020raster, activity_points, method="bilinear"),
                extract(PM10.2025raster, activity_points, method="bilinear"),
                extract(PM10.2030raster, activity_points, method="bilinear"),
                extract(PM10d.2020raster, activity_points, method="bilinear"),
                extract(PM10d.2025raster, activity_points, method="bilinear"),
                extract(PM10d.2030raster, activity_points, method="bilinear"),
                extract(NOx.2020raster, activity_points, method="bilinear"),
                extract(NOx.2025raster, activity_points, method="bilinear"),
                extract(NOx.2030raster, activity_points, method="bilinear"),
                extract(NO2.2020raster, activity_points, method="bilinear"),
                extract(NO2.2025raster, activity_points, method="bilinear"),
                extract(NO2.2030raster, activity_points, method="bilinear"))
names(p)
names(p) <- c("ID", "Name", "Type", "Elapsed_time", "Distance", "Commute", "Latitude", 
                          "Longitude", "Time", "Latitude.p1", "Longitude.p1", "Dist_to_prev",
                          "Time.p1", "Time_diff_to_prev", "M_per_sec","Km_per_h", "Lowess_speed",
                          "Elevation","Lowess_elevation", "PM25_2020", "PM25_2025", "PM25_2030",
              "PM10_2020", "PM10_2025", "PM10_2030","PM10d_2020", "PM10d_2025", "PM10d_2030",
              "NOx_2020", "NOx_2025", "NOx_2030", "NO2_2020", "NO2_2025", "NO2_2030")
head(p)

p_spdf <- SpatialPointsDataFrame(p[,c("Longitude", "Latitude")], p, 
                                             proj4string = CRS("+init=epsg:4326"))
head(p_spdf@data)

#map for tracks

#elevation
tmap_mode('plot')
tm_shape(run_map) + tm_borders(alpha=1) + tm_shape(p_spdf) + 
  tm_dots(col = "NO2_2030", palette=c("lightgreen", "firebrick1"), style = "quantile",shape=16, size=.06) + tm_layout(basemaps = c('OpenStreetMap')) 

write.csv(pollutants_2020, file="modelled_pollutants_2020.csv", row.names = F)
write.csv(pollutants_2025, file="modelled_pollutants_2025.csv", row.names = F)
write.csv(pollutants_2030, file="modelled_pollutants_2030.csv", row.names = F)
