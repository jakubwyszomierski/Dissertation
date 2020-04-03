d = read.csv('routes_all.csv', stringsAsFactor=F)

#d <- d %>% filter(from%in%l & to%in%l)

d <- d %>% filter(lon1 > (-15) & lon1 < 30 & lat1>30 & lat1<90 &
                    lon2 > (-15) & lon2 < 30 & lat2>30 & lat2<90)

xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

ggplot(d[which((d$count>1)),], aes(lon1, lat1))+
  geom_segment(aes(x=lon1, y=lat1, xend=lon2, yend=lat2, alpha=count), col="white")+
  scale_alpha_continuous(range = c(0.03, 0.3))+  
  theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal() + wrld


r=gcIntermediate(d[,c('lon1', 'lat1')], d[,c('lon2', 'lat2')], sp=TRUE)
fortify.SpatialLinesDataFrame = function(model, data, ...){
  ldply(model@lines, fortify)
}
fortifiedroutes = fortify.SpatialLinesDataFrame(r)
fortifiedroutes
routes_count = data.frame('count'=d$count, 'id'=1:nrow(d), 'Countries'=d$from)
greatcircles = merge(fortifiedroutes, routes_count, all.x=T,by="id")

ggplot(greatcircles, aes(long, lat))+wrld+
  geom_line(aes(x=long, y=lat, alpha=count, group=group), col="white")+
  scale_alpha_continuous(range = c(0.03, 0.2))+  
  theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()


nrow(m3[which(m3$sum>30),])
View(m3)

s <- sample_n(m3, 700)

ggplot(s, aes(source_long, source_lat))+wrld+
  geom_segment(aes(x=source_long, y=source_lat, xend=dest_long, yend=dest_lat, alpha=sum), col="white")+
  scale_alpha_continuous(range = c(0.01, 0.06))+  scale_color_gradient(low="yellow", high="white")+
  theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal() 
