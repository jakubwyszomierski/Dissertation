#run and pollution
setwd("~/Desktop/studia/dissertation/data")
options( digits=10,scipen = 999)

pollutants <- read.csv("MyData/modelled_pollutants.csv")

output <- readOGR("MyData/output.shp")
output <- spTransform(output, CRS("+init=epsg:27700"))

WardsMap <- readOGR("Shapefiles/London_Ward.shp")
WardsMap <- spTransform(WardsMap, CRS("+init=epsg:27700"))
run_map <- WardsMap[!is.na(over(WardsMap, geometry(output))),]
run_map <- spTransform(run_map, CRS("+init=epsg:27700"))

#pollution rasters
library(raster)
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

ggplot() +  
  geom_tile(data=pollutants, aes(x=Easting, y=Northing, fill=PM25), alpha=0.5) +
  scale_fill_gradient(low="blue", high = "red", guide="colourbar") 

library(randomcoloR)
plot(output,add=T, col=randomColor(),cex=0.01)
#extract the values of those particular pixels points lie within
run.pollution <- data.frame(output@data,extract(PM25.raster, output, method="bilinear"),
                            extract(PM10.raster, output, method="bilinear"),
                            extract(PM10d.raster, output, method="bilinear"),
                            extract(NOx.raster, output, method="bilinear"),
                            extract(NO2.raster, output, method="bilinear"))

names(run.pollution) <- c("trackID", "pointID","lon", "lat","speed_km/h","speed_m/s",
                          "time", "time_diff",  "dist.to.next", "ele", "ele.n1", "slope",
                          "ele_api", "ele_api.n1", "api_slope","lowess_api_slope", "abs_api_slope", 
                          "lowess_abs_api_slope", "PM25", "PM10", "PM10d", "NOx", "NO2")

options(scipen=999)
library(Hmisc)
rcorr(as.matrix(run.pollution[,c(10,12,13,15:23)]))[c(1,3)]

#very weak (-0.11 <-> -0.13) although significant, negative correlation between lowess_abs_api_slope and pollutants

#according to Padulo and his colleagues (2013), an increase of 2% in slope is associated with a 10% increase in ventilation, 
#while an increase of 7% in slope is associated with an increase of 19% in ventilation
s1 <- c(0,2)
v1 <- c(1,1.1)
d1 <- data.frame(s1,v1)
summary(m1 <- lm(v1~s1, data=d1))

s2 <- c(2,7)
v2 <- c(1.1,1.19)
d2 <- data.frame(s2,v2)
summary(m2 <- lm(v2~s2, data=d2))

m2$coefficients[1]+52*m2$coefficients[2]
summary(run.pollution)

#quadratic term
s <- c(0,2,7, 52, 150)
v <- c(1,1.1,1.19, 2,2)
d <- data.frame(s,v)
plot(d, type="b", xlab="Slope in %", ylab="Increase in ventilation in %")
summary(m <- lm(v~s+I(s^2), data=d))
#i'll use two regression models
#when lowess abs api slope is between 0 and 2 than use m1, when it is above 2, use m2
run.pollution$ventilation <- ifelse(run.pollution$lowess_abs_api_slope<2, 
                          m1$coefficients[1]+run.pollution[,"lowess_abs_api_slope"]*m1$coefficients[2], 
                          m2$coefficients[1]+run.pollution[,"lowess_abs_api_slope"]*m2$coefficients[2])

run.pollution$ventilation <- ifelse(run.pollution$ventilation>2, 2, run.pollution$ventilation)
summary(run.pollution)

names(run.pollution)
run.pollution$PM25_intake <- run.pollution$PM25*run.pollution$ventilation
run.pollution$PM10_intake <- run.pollution$PM10*run.pollution$ventilation
run.pollution$PM10d_intake <- run.pollution$PM10d*run.pollution$ventilation
run.pollution$NOx_intake <- run.pollution$NOx*run.pollution$ventilation
run.pollution$NO2_intake <- run.pollution$NO2*run.pollution$ventilation

library(dplyr)
agg <- run.pollution %>% group_by(trackID) %>% 
  summarise(avg_PM25=mean(PM25, na.rm = T),avg_PM10=mean(PM10, na.rm = T), avg_PM10d=mean(PM10d, na.rm = T), 
            avg_NOx=mean(NOx, na.rm = T), avg_NO2=mean(NO2, na.rm = T), avg_PM25_intake=mean(PM25_intake, na.rm = T),
            avg_PM10_intake=mean(PM10_intake, na.rm = T),avg_PM10d_intake=mean(PM10d_intake, na.rm = T), 
            avg_NOx_intake=mean(NOx_intake, na.rm = T), avg_NO2_intake=mean(NO2_intake, na.rm = T))

t.test(agg$avg_PM25, agg$avg_PM25_intake)
t.test(agg$avg_PM10, agg$avg_PM10_intake)
t.test(agg$avg_PM10d, agg$avg_PM10d_intake)
t.test(agg$avg_NOx, agg$avg_NOx_intake)
t.test(agg$avg_NO2, agg$avg_NO2_intake)

run.pollution_merge <- merge(run.pollution, agg, by="trackID")

#integration
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

t=NULL
for(j in levels(as.factor(run.pollution_merge$trackID))){
  sub <- subset(run.pollution_merge, trackID==j)
  print(j)
  for(i in names(sub)[25:29]){
    sub[,paste0(i, ".n1")] <- shift.vec(sub[,i], -1)
    sub[,paste0("integration_", i)] <- ((sub[,i] +  sub[,paste0(i, ".n1")])*sub[, "time_diff"])/2
  }
  t=rbind(t, sub)
}

run.pollution_merge <- t

num_integ_function <- function(dataset,variable, t1,t2){
  pol <- dataset[t1:t2,]
  sum_integ <- sum(pol[,paste0("integration_", variable)], na.rm=T)
  sumtime_diff <- sum(pol[,"time_diff"], na.rm=T)
  intake_persec <- sum_integ/sumtime_diff
  weighted.mean <- weighted.mean(dataset[t1:t2,variable], w=dataset[t1:t2,"time_diff"], na.rm=T)
  sumdist <- sum(pol[,"dist.to.next"], na.rm=T)
  data.frame(sum_integ,sumtime_diff, intake_persec,weighted.mean, sumdist)
}
num_integ_function(subset(run.pollution_merge, trackID==976946527),"PM25", 1,20)

#correlation plot
library(Hmisc)
names(run.pollution)
summary(run.pollution, digits = 4)
sd(run.pollution$speed_km.h)
sd(run.pollution$ele)
sd(run.pollution$slope)
sd(run.pollution$ele_api)
sd(run.pollution$api_slope)
sd(run.pollution$lowess_api_slope)
sd(run.pollution$lowess_abs_api_slope)
sd(run.pollution$ventilation)
sd(run.pollution$time_diff, na.rm=T)

#a few caes in which time_diff is extremely high - time of making a stop
t.test(run.pollution$PM25, run.pollution$PM25_intake)

#there is a statisticall difference betweeen mean concentration and mean inhalation 

library(corrplot)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(run.pollution[,c(13,15:16,18, 24,19,25)])
round(p.mat,5)
mat.pol <- as.matrix(run.pollution[,c(13,15:16,18,24,19,25)])
colnames(mat.pol) <- c("Elevation (API)", "Slope (API)","Lowess slope (API)",
                       "Lowess absolute slope (API)","Ventilation", "$ PM[2.5]",
                       "$PM[2.5]* intake")
colnames(mat.pol)
library(Hmisc)
corrplot(rcorr(mat.pol, type="pearson")$r, method="color",  
         type="lower", order="original",
         addCoef.col = "black",  p.mat = p.mat,
         tl.col="black", tl.srt=45, number.digits = 3,
         sig.level = 0.01, diag = F)

#correlation of pollutants
rcorr(mat.pol2, type="pearson")

write.csv(run.pollution_merge, file="MyData/run.pollution.csv", row.names = F)

