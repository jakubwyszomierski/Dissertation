setwd("~/Desktop/studia/dissertation/data")
library(rgdal)
library(randomcoloR)
library(raster)
library(Hmisc)
library(dplyr)
library(osmdata)
library(londonShapefiles)
library(rgeos)


dfsp <- readOGR("strava/dfsp.shp")
dfsp <- spTransform(dfsp, CRS("+init=epsg:27700"))

WardsMap <- readOGR("statistical-gis-boundaries-london/ESRI/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))

run_map <- WardsMap[!is.na(over(WardsMap, geometry(dfsp))),]
run_map <- spTransform(run_map, CRS("+init=epsg:27700"))

pollutants <- read.csv("modelled_pollutants.csv")

plot(run_map)
plot(dfsp, col=randomColor(),cex=0.05,add=T)

#pollution rasters
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
plot(dfsp,add=T, col=randomColor(),cex=0.02)

#extract the values of those particular pixels points lie within
names(dfsp@data)
sum(is.na(dfsp@data))
run.pollution <- data.frame(dfsp@data,extract(PM25.raster, dfsp, method="bilinear"),
                            extract(PM10.raster, dfsp, method="bilinear"),extract(PM10d.raster, dfsp, method="bilinear"),
                            extract(NOx.raster, dfsp, method="bilinear"),extract(NO2.raster, dfsp, method="bilinear"))

head(run.pollution)
sum(is.na(run.pollution))
names(run.pollution) <- c("trackID", "pointID","lon", "lat","speed_km/h","speed_m/s",
                          "time", "time_diff",  "dist.to.next", "ele", "ele.n1", "slope",
                          "ele_api", "ele_api.n1", "api_slope","lowess_api_slope", "abs_api_slope", 
                          "lowess_abs_api_slope", "PM25", "PM10", "PM10d", "NOx", "NO2")
run.pollution$round <- round(run.pollution$lowess_api_slope, 0)

#f --> the smoother span. This gives the proportion of points in the plot which influence the smooth at each value. 
# Larger values give more smoothness.
names(run.pollution)
rcorr(as.matrix(run.pollution[,c(10,12,13,15:23)]))

#very weak (-0.07 <-> -0.09), although significant, negative correlation between abs_google_slope and each pollutant 
#moderate (-0.22 <-> -0.3), negative and significant correlation between elevation and pollutants

#in this sense, increased ventilation will be slightly correlated with air quality of inhaled air

#according to 

s1 <- c(0,2)
v1 <- c(1,1.1)
d1 <- data.frame(s1,v1)
plot(d1, type="b")
summary(m1 <- lm(v1~s1, data=d1))

s2 <- c(2,7)
v2 <- c(1.1,1.19)
d2 <- data.frame(s2,v2)
plot(d2, type="b")
summary(m2 <- lm(v2~s2, data=d2))

s <- c(0,2,7)
v <- c(1,1.1,1.19)
d <- data.frame(s,v)
plot(d, type="b")
summary(m <- lm(v~s+I(s^2), data=d))
m$coefficients[1] + 7*m$coefficients[2] + 7^2*m$coefficients[3]

#i'll use two linear regression models
summary(run.pollution$lowess_abs_api_slope)
#when lowess abs api slope is between 0 and 2 than use m1
m1$coefficients[1]+run.pollution[2,18]*m1$coefficients[2]

#when lowess abs api slope is between 2 and 7 than use m2
m2$coefficients[1]+run.pollution[200,18]*m2$coefficients[2]

nrow(run.pollution[run.pollution$lowess_abs_api_slope>15,])
names(run.pollution)
dat <- run.pollution

dat$ventilation <- ifelse(dat$lowess_abs_api_slope<2, m1$coefficients[1]+dat[,18]*m1$coefficients[2], m2$coefficients[1]+dat[,18]*m2$coefficients[2])


dat$PM25_intake <- dat$PM25*dat$ventilation

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

thames <- load_thames()
thames.proj <- spTransform(thames, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
thames.df <- fortify(thames.proj)

#one track
dat2 <- dat[dat$trackID==1033379718,]

parks <- opq(bbox = c(min(dat2[,"lon"])-0.01, min(dat2[,"lat"])-0.01,
                       max(dat2[,"lon"])+0.01,  max(dat2[,"lat"])+0.01)) %>%
  add_osm_feature(key = 'leisure',
                  value = "park") %>% osmdata_sf()

st <- opq(bbox = c(min(dat2[,"lon"])-0.01, min(dat2[,"lat"])-0.01,
                        max(dat2[,"lon"])+0.01,  max(dat2[,"lat"])+0.01)) %>% 
  add_osm_feature(key = 'highway') %>% osmdata_sf()

#save to desktop
g <-  ggplot() + 
  geom_polygon(thames.df, mapping=aes(long, lat, group = group,fill = I("skyblue")),lwd=0, alpha = 0.8) +
  geom_sf(data=parks$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
  geom_sf(data=parks$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3) + 
  geom_sf(st$osm_lines, mapping=aes(fill = I("grey95")),  lwd=0.25, alpha=0.2)+
  geom_path(data=dat2,aes(lon, lat, group=trackID,colour=PM25_intake),alpha = 1, size=1.25, 
            lineend = "round", linejoin = "round") + 
  scale_color_gradient(low="blue", high = "red", guide="colourbar", name="PM25 intake") +
  theme_opts+
  coord_sf(xlim = c(min(dat2$lon)-.0025, max(dat2$lon)+.0025), crs="+init=epsg:4326",
           ylim = c(min(dat2$lat)-.0025, max(dat2$lat)+.0025), clip="on")

ggsave(g, file="~/Desktop/map.png")

#all runs
parks2 <- opq(bbox = c(min(dat[,"lon"])-0.01, min(dat[,"lat"])-0.01,
                       max(dat[,"lon"])+0.01,  max(dat[,"lat"])+0.01)) %>%
  add_osm_feature(key = 'leisure',
                  value = "park") %>% osmdata_sf()

st <- opq(bbox = c(min(dat[,"lon"])-.01, min(dat[,"lat"])-0.01,
                   max(dat[,"lon"])+0.01,  max(dat[,"lat"])+0.01)) %>% 
  add_osm_feature(key = 'highway', value=c("secondary", "primary", "tertiary", "residential")) %>% osmdata_sf()


#save to desktop
g <-  ggplot() + 
  geom_polygon(thames.df, mapping=aes(long, lat, group = group,fill = I("skyblue")),lwd=0, alpha = 0.8) +
  geom_sf(data=parks2$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
  geom_sf(data=parks2$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3) + 
  geom_sf(st$osm_lines, mapping=aes(fill = I("grey95")),  lwd = 0.25, alpha=0.2)+
  geom_path(data=dat,aes(lon, lat, group=trackID,colour=PM25_intake),alpha = 1, size=1.5, 
            lineend = "round", linejoin = "round") + 
  scale_color_gradient(low="blue", high = "red", guide="colourbar", name="PM25 intake") +
  theme_opts+
  coord_sf(xlim = c(min(dat$lon)-.001, max(dat$lon)+.001), crs="+init=epsg:4326",
           ylim = c(min(dat$lat)-.001, max(dat$lat)+.001), clip="on")

ggsave(g, file="~/Desktop/studia/dissertation/plots/map.png")


#if we assume that the relationship between slope and ventilation is linear, than increase of 1% in slope is associated with 2.53 increase

#only 0, 2 and 7 slopes
#slopes3 <- run.pollution[run.pollution$round==0|run.pollution$round==2|run.pollution$round==7,]
#nrow(slopes3)/nrow(run.pollution)

#ggplot() + 
 # geom_path(data = slopes3, aes(x=lon, y=lat,color = round, group=trackID), size = 1) + 
  #scale_color_gradientn(colours = rainbow(3), breaks = seq(0, max(slopes3$round)+1, by = 3), 
  #                      guide="colourbar", name="round_lowess")
#inhalation PM25
#if round=0, then, PM25*1
#if round=2, then PM25*1.1
#if round=7, then PM25*1.19

#summary(slopes3)

#slopes3$increase[slopes3$round==0] <- 1
#slopes3$increase[slopes3$round==2] <- 1.1
#slopes3$increase[slopes3$round==7] <- 1.19

#slopes3$PM25_intake <- slopes3$PM25*slopes3$increase
#summary(slopes3)

#ggplot() + 
 # geom_point(data = slopes3, aes(x=lon, y=lat,color = PM25_intake, group=trackID), size = 1) + 
  #scale_color_gradientn(colours = rainbow(3), breaks = seq(0, max(slopes3$PM25_intake, na.rm=T), by = 3), 
#                        guide="colourbar", name="leg")