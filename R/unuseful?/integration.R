setwd("~/Desktop/studia/dissertation/data")
options(digits=10)

#libraries
library(osmdata)
library(ggplot2)
library(londonShapefiles)

#functions
num_integ_function <- function(dataset,variable, t1,t2){
  pol <- dataset[t1:t2,]
  sum_integ <- sum(pol[,paste0("integration_", variable)], na.rm=T)
  sumtime_diff <- sum(pol[,"time_diff"], na.rm=T)
  intake_persec <- sum_integ/sumtime_diff
  weighted.mean <- weighted.mean(dataset[t1:t2,variable], w=dataset[t1:t2,"time_diff"], na.rm=T)
  sumdist <- sum(pol[,"dist.to.next"], na.rm=T)
  maxtime_diff <- max(pol[,"time_diff"], na.rm=T)
  data.frame(sum_integ,sumtime_diff, maxtime_diff, intake_persec,weighted.mean, sumdist)
}
myfunction_allosm <- function(dataset, variable, legend,filename){
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
  plot(g)
  print("Saving")
  ggsave(g, file=paste0("~/Desktop/studia/dissertation/plots/", filename, ".png"), width = 9, height = 9)
}

#data
run.pollution <- read.csv("MyData/run.pollution.csv")
sample1 <- subset(run.pollution, trackID==976946527) #primrose hill 
sample2 <- subset(run.pollution, trackID==1968008352) #hampstead
sample4 <- subset(run.pollution, trackID==1805834272)
sample6 <- subset(run.pollution, trackID==1847410744)
sample7 <- subset(run.pollution, trackID==1304635980)
sample8 <- subset(run.pollution, trackID==1315025232)
sample9 <- subset(run.pollution, trackID==1437611571)


#time and PM25 intake
plot(strptime(sample1$time,format = "%Y-%m-%d %H:%M:%OS"), sample1$PM25_intake, type="l", col="red")
lines(strptime(sample1$time,format = "%Y-%m-%d %H:%M:%OS"), sample1$PM25, type="l", col="blue")
plot(strptime(sample1$time,format = "%Y-%m-%d %H:%M:%OS"), sample1$PM25_intake, type="b", col="red")

#example
ggplot(data=sample1[40:47,], mapping = aes(x=time, y=PM25_intake, group=1)) + geom_line(colour="gray70", size=1) + 
  geom_area(data=sample1[41:46,],mapping = aes(x = time),  alpha=0, size=3, colour="black") + 
   geom_area(data=sample1[41:42,],mapping = aes(x = time), fill = "tomato1") + 
  geom_area(data=sample1[42:43,],mapping = aes(x = time), fill = "skyblue2") + 
  geom_area(data=sample1[44:45,],mapping = aes(x = time), fill = "pink1") + 
  geom_area(data=sample1[45:46,],mapping = aes(x = time), fill = "seagreen2") + 
  geom_area(data=sample1[43:44,],mapping = aes(x = time), fill = "gold1", colour="red", size=3) + 
  coord_cartesian(ylim = c(20.5, 22.7)) +
  scale_x_time()+ labs(y = expression(PM[2.5]*" intake"),x = "Timestamp")+
  theme(panel.background = element_blank(), axis.title = element_text(size=rel(1.2)), 
        axis.text = element_text(size = 15))  

#run
ggplot(data=sample1[90:340,], mapping = aes(x=time, y=PM25_intake, group=1)) + geom_line() + 
 # geom_area(data=sample1[20:39,],mapping = aes(x = time), fill = "tomato1") + 
  #geom_area(data=sample1[57:67,],mapping = aes(x = time), fill = "skyblue2") + 
  geom_area(data=sample1[101:162,],mapping = aes(x = time), fill = "gold1") + 
  geom_area(data=sample1[200:327,],mapping = aes(x = time), fill = "pink1") + 
  coord_cartesian(ylim = c(min(sample1$PM25_intake), max(sample1$PM25_intake))) +
  scale_x_time()+ labs(y = expression(PM[2.5]*" intake"), x = "Timestamp")+
  theme(panel.background = element_blank(), axis.title = element_text(size=rel(1.2)))

#7 seconds difference (34-27 seconds)
num_integ_function(dataset=sample1, variable="PM25_intake", t1=20, t2=39)-
num_integ_function(dataset=sample1, variable="PM25_intake", t1=57, t2=67)

#65 seconds difference (208 seconds - 143 seconds)
num_integ_function(dataset=sample1, variable="PM25_intake", t1=101, t2=162)-
num_integ_function(dataset=sample1, variable="PM25_intake", t1=200, t2=327)

#map for examples of integrration
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

parks1 <- opq(bbox = c(min(sample1[,"lon"])-0.01, min(sample1[,"lat"])-0.01,
                      max(sample1[,"lon"])+0.01,  max(sample1[,"lat"])+0.01)) %>%
  add_osm_feature(key = 'leisure',value = "park") %>% osmdata_sf()

st1 <- opq(bbox = c(min(sample1[,"lon"])-.01, min(sample1[,"lat"])-0.01,
                   max(sample1[,"lon"])+0.01,  max(sample1[,"lat"])+0.01)) %>% 
  add_osm_feature(key = 'highway') %>% osmdata_sf()

g1 <- ggplot() + 
  geom_polygon(thames.df, mapping=aes(long, lat, group = group,fill = I("skyblue")),lwd=0, alpha = 0.8) +
  geom_sf(data=parks1$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
  geom_sf(data=parks1$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3) + 
  geom_sf(st1$osm_lines, mapping=aes(fill = I("grey95")),  lwd = 0.25, alpha=0.2)+
  geom_path(data=sample1[101:162,],aes(lon, lat, group=trackID, colour=PM25_intake), alpha = .5, size=1.75, 
            lineend = "round", linejoin = "round") +
  geom_path(data=sample1[200:327,],aes(lon, lat, group=trackID,colour=PM25_intake),alpha = .5, size=1.75, 
            lineend = "round", linejoin = "round") + 
  scale_color_gradient(low="blue", high = "red", guide="colourbar",
                       name=expression(PM[2.5]*" intake")) +
  theme_opts+
  coord_sf(xlim = c(min(sample1[101:327,"lon"])-.001, max(sample1[101:327,"lon"])+.001), crs="+init=epsg:4326",
           ylim = c(min(sample1[101:327,"lat"])-.001, max(sample1[101:327,"lat"])+.001), clip="on")
print(g1)

ggsave(g1, file=paste0("~/Desktop/studia/dissertation/plots/sample1_example.png"), width = 9, height = 9)

#sample1
myfunction_allosm(sample1,"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample1")


#sample2
myfunction_allosm(sample2,"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample2")
myfunction_allosm(sample2[1189:1757,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample2_left")
myfunction_allosm(sample2[210:703,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample2_right")

myfunction_allosm(sample2[c(1189:1757,210:703),],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample2_both")

myfunction_allosm(sample2[c(205:1759),],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample2_both")

parks2 <- opq(bbox = c(min(sample2[,"lon"])-0.01, min(sample2[,"lat"])-0.01,
                      max(sample2[,"lon"])+0.01,  max(sample2[,"lat"])+0.01)) %>%
  add_osm_feature(key = 'leisure',value = "park") %>% osmdata_sf()

st2 <- opq(bbox = c(min(sample2[,"lon"])-.01, min(sample2[,"lat"])-0.01,
                   max(sample2[,"lon"])+0.01,  max(sample2[,"lat"])+0.01)) %>% 
  add_osm_feature(key = 'highway') %>% osmdata_sf()

g2 <- ggplot() + 
  geom_polygon(thames.df, mapping=aes(long, lat, group = group,fill = I("skyblue")),lwd=0, alpha = 0.8) +
  geom_sf(data=parks2$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
  geom_sf(data=parks2$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3) + 
  geom_sf(st2$osm_lines, mapping=aes(fill = I("grey95")),  lwd = 0.25, alpha=0.2)+
  geom_path(data=sample2[205:1759,],aes(lon, lat, group=trackID, colour=PM25_intake), alpha = .5, size=1.75, 
            lineend = "round", linejoin = "round") +
  geom_point(data=sample2[c(930,205),], aes(lon, lat,  group=trackID), col="black", shape=8, size=6)+
  scale_color_gradient(low="blue", high = "red", guide="colourbar", name=expression(PM[2.5]*" intake")) +
  theme_opts+
  coord_sf(xlim = c(min(sample2[205:1759,"lon"])-.001, max(sample2[205:1759,"lon"])+.001), crs="+init=epsg:4326",
           ylim = c(min(sample2[205:1759,"lat"])-.001, max(sample2[205:1759,"lat"])+.001), clip="on")
ggsave(g2, file=paste0("~/Desktop/studia/dissertation/plots/sample2_both.png"), width = 9, height = 9)

num_integ_function(dataset=sample2, variable="PM25_intake", t1=205, t2=929)-
  num_integ_function(dataset=sample2, variable="PM25_intake", t1=931, t2=1749) #point 930 is a stoppage point
#736 meters difference, 0.90 PM per sec, 100 seconds difference, 1124 integration difference
#the difference is extremely marginal


#sample 1 and 6
#regent's park vs primrose hill
num_integ_function(dataset=sample1, variable="PM25_intake", t1=532, t2=1115) -#park
num_integ_function(dataset=sample6, variable="PM25_intake", t1=2401, t2=3125) #park
#over 1 kilometers shorter, but associated with more time spent (2 minutes) and 2000 integration value

#sample 4 and 6
#regents park right side - park vs road
myfunction_allosm(sample4[1100:1570,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample4_road")#road
myfunction_allosm(sample6[213:646,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample6_park")#park

num_integ_function(dataset=sample4, variable="PM25_intake", t1=1100, t2=1570)- #road
num_integ_function(dataset=sample6, variable="PM25_intake", t1=213, t2=646) #park
 #road was 40 longer, but surprisingly, it took almost minute and a half longer and was 
# associated with a higher pollution intake of 1223

#on the other hand, both tracks could be undertaken by two different persons whose speed moght differ 


#sample 6
#road to regent park and regents park
#początek
num_integ_function(dataset=sample6, variable="PM25_intake", t1=1, t2=213)-
num_integ_function(dataset=sample6, variable="PM25_intake", t1=214, t2=482)
myfunction_allosm(sample6[1:482,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample6_begin")

#only 2 meters and 13 seconds differencce, but almost 1500 in pollution intake
#this shows that the same time spend on run to Regent's Park is associated with higher
#pollution intake of 1646, compared to the same amount of time spent in the park

#koniec ---> dobra to jest pierwsza ciekawa rzecz
myfunction_allosm(sample6[2930:3125,],"PM25_intake", legend=expression(PM[2.5]*" intake"), "sample6_koniecroad")
myfunction_allosm(sample6[2627:2929,],"PM25_intake", legend=expression(PM[2.5]*" intake"), "sample6_koniecpark")

#adjust breaks
parks3 <- opq(bbox = c(min(sample6[,"lon"])-0.01, min(sample6[,"lat"])-0.01,
                       max(sample6[,"lon"])+0.01,  max(sample6[,"lat"])+0.01)) %>%
  add_osm_feature(key = 'leisure',value = "park") %>% osmdata_sf()

st3 <- opq(bbox = c(min(sample6[,"lon"])-.01, min(sample6[,"lat"])-0.01,
                    max(sample6[,"lon"])+0.01,  max(sample6[,"lat"])+0.01)) %>% 
  add_osm_feature(key = 'highway') %>% osmdata_sf()

g3 <- ggplot() + 
  geom_polygon(thames.df, mapping=aes(long, lat, group = group,fill = I("skyblue")),lwd=0, alpha = 0.8) +
  geom_sf(data=parks3$osm_polygons,mapping=aes(fill = I("springgreen3")),lwd=0, alpha = 0.3) +
  geom_sf(data=parks3$osm_multipolygons,mapping=aes(fill = I("springgreen3")), lwd=0, alpha = 0.3) + 
  geom_sf(st3$osm_lines, mapping=aes(fill = I("grey95")),  lwd = 0.25, alpha=0.2)+
  geom_path(data=sample6[2627:2929,],aes(lon, lat, group=trackID, colour=PM25_intake), alpha = .5, size=1.75, 
            lineend = "round", linejoin = "round") +
  scale_color_gradient(low="blue", high = "red", guide="colourbar", breaks=c(18,19,20),
                       name=expression(PM[2.5]*" intake")) +
  theme_opts+
  coord_sf(xlim = c(min(sample6[2627:2929,"lon"])-.001, max(sample6[2627:2929,"lon"])+.001), crs="+init=epsg:4326",
           ylim = c(min(sample6[2627:2929,"lat"])-.001, max(sample6[2627:2929,"lat"])+.001), clip="on")
print(g3)

ggsave(g3, file=paste0("~/Desktop/studia/dissertation/plots/sample6_koniecpark.png"), width = 9, height = 9)

myfunction_allosm(sample6[2627:3125,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample6_koniec")


num_integ_function(dataset=sample6, variable="PM25_intake", t1=2930, t2=3125)
  num_integ_function(dataset=sample6, variable="PM25_intake", t1=2627, t2=2929)
#after running 90 seconds and 611 meters longer than on the road, our pollution intake is similar 
#in other words, after running from UCL to regents park, we can run for 90 seconds until the amount
#of inhaled pollution will reach the amount inhaled on the road
#you can run for almost minute and a half longer and 611 meters further 


#sample 6 and 7
#left regents park vs right camden town (corrected)
num_integ_function(dataset=sample6, variable="PM25_intake", t1=2290, t2=3125)-
  num_integ_function(dataset=sample7, variable="PM25_intake", t1=1170, t2=1304) - 
  num_integ_function(dataset=sample7, variable="PM25_intake", t1=1349, t2=1978) 

myfunction_allosm(sample6[c(2290:3125),],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample6_left")
myfunction_allosm(sample7[c(1170:1304, 1349:1978),],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample6_right")

#sample9
myfunction_allosm(sample9,"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample9")
myfunction_allosm(sample9[1:209,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample9_A")
myfunction_allosm(sample9[210:922,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample9_B")
myfunction_allosm(sample9[923:1122,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample9_C")

num_integ_function(dataset=sample9, variable="PM25_intake", t1=1, t2=209) + 
  num_integ_function(dataset=sample9, variable="PM25_intake", t1=923, t2=1122) - 
  num_integ_function(dataset=sample9, variable="PM25_intake", t1=210, t2=922)
#4 minutes, over 1km, 1600 integration difference
#mean intake per sec is 4 PM25 lower in regents park, which means that after running more than 84 seconds 
#than the time spend in the major road it is much better

(num_integ_function(dataset=sample9, variable="PM25_intake", t1=1, t2=209) + 
    num_integ_function(dataset=sample9, variable="PM25_intake", t1=923, t2=1122) - 
    num_integ_function(dataset=sample9, variable="PM25_intake", t1=210, t2=922)) [1]/
  num_integ_function(dataset=sample9, variable="PM25_intake", t1=210, t2=922)[4]


#from UCL to Regents Park - less and more crowded roads
num_integ_function(sample1, "PM25_intake", t1=1, t2=185) -#more crowded
  num_integ_function(sample9, "PM25_intake", t1=1, t2=209) #less crowded 

myfunction_allosm(sample9[1:209,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample9_A")
myfunction_allosm(sample1[1:185,],"PM25_intake",  legend=expression(PM[2.5]*" intake"), "sample1_A")

#this shows that minor roads are characterized by lower air pollution concentration, 
#but it takes more time to run there

#synthetic run
syn1 <- rbind(sample1[1:185,],sample9[209:923,],sample1[1664:1824,])
num_integ_function(dataset=sample9, variable="PM25_intake", t1=209, t2=923) + 
  num_integ_function(sample1, "PM25_intake", t1=1, t2=185) +
  num_integ_function(sample1, "PM25_intake", t1=1664, t2=1824)

myfunction_allosm(syn1,"PM25_intake", legend =  expression(PM[2.5]*" intake"), "syn1")
myfunction_allosm(sample9[209:923,],"PM25_intake", legend =  expression(PM[2.5]*" intake"), "syn1_mid")
myfunction_allosm(sample1[c(1:185,1664:1824),],"PM25_intake", legend =  expression(PM[2.5]*" intake"), "syn1_commute")


num_integ_function(dataset=sample9, variable="PM25_intake", t1=209, t2=923) -
  num_integ_function(sample1, "PM25_intake", t1=1, t2=185) -
  num_integ_function(sample1, "PM25_intake", t1=1664, t2=1824)

#run in park was 734 meters and almost 2 minutes longer than a commute to the park, 
#but was characterized by much lower amount of inhaled pollutants
