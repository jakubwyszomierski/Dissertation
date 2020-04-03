#segments

run.pollution <- read.csv("mydata/run.pollution.csv")
summary(run.pollution)

sample2 <- subset(run.pollution, trackID==1968008352) #Hampstead Heath
sample2_sp <- SpatialPointsDataFrame(sample2[,c("lon", "lat")], sample2, 
                                               proj4string = CRS("+init=epsg:4326"))
sample2_sp <- spTransform(sample2_sp, CRS("+init=epsg:27700"))
plot(sample2_sp)

seg1 <- sample2[c(900:1100),]
seg1


parks <- opq(bbox = c(min(seg1[,"lon"])-0.05, min(seg1[,"lat"])-0.05,
                      max(seg1[,"lon"])+0.05,  max(seg1[,"lat"])+0.05)) %>%
  add_osm_feature(key = 'leisure',
                  value = "park") %>% osmdata_sf()

st <- opq(bbox = c(min(seg1[,"lon"])-.05, min(seg1[,"lat"])-0.05,
                   max(seg1[,"lon"])+0.05,  max(seg1[,"lat"])+0.05)) %>% 
  add_osm_feature(key = 'highway') %>% osmdata_sf()
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

ggplot() + 
  geom_sf(data=parks$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
  geom_sf(data=parks$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3) + 
  geom_sf(st$osm_lines, mapping=aes(fill = I("grey95")),  lwd = 0.1, alpha=0.2)+
  geom_path(data=seg1,aes(lon, lat, group=trackID, colour="PM25_intake"), alpha = .5, size=.75, 
            lineend = "round", linejoin = "round") + 
  theme_opts# +
  #coord_sf(xlim = c(min(seg1$lon)-.01, max(seg1$lon)+.01), crs="+init=epsg:4326",
   #        ylim = c(min(seg1$lat)-.01, max(seg1$lat)+.01), clip="on")

ggsave(g, file="~/Desktop/lol.png", width=9, height = 9)

s1 <- sample2_sp[c(850:1000),]
plot(s1)
s2 <- sample2_sp[c(150:300),]
plot(s2)

s3 <- sample2_sp[c(1250:1400),]
plot(s3)
summary(s1@data)
summary(s2@data)
t.test(s1@data$PM25_intake, s2@data$PM25_intake)
t.test(s1@data$PM25_intake, s3@data$PM25_intake)
plot(sample2_sp)
