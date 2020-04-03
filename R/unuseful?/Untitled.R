
#sample1
myfunction_allosm(sample1,"PM25_intake", "PM25 intake", "sample1")
myfunction_allosm(sample1[1:185,],"PM25_intake", "PM25 intake", "sample1_A")

myfunction_allosm(sample1[532:1115,],"PM25_intake", "PM25 intake", "y")
num_integ_function(dataset=sample1, variable="PM25_intake", t1=532, t2=1115)

myfunction_allosm(sample1[1665:1824,],"PM25_intake", "PM25 intake", "sample1_B")
num_integ_function(sample1, "PM25_intake", t1=1660, t2=1824)

#sample2
myfunction_allosm(sample2,"PM25_intake", "PM25 intake", "sample2")
myfunction_allosm(sample2[1189:1757,],"PM25_intake", "PM25 intake 2", "y")
myfunction_allosm(sample2[210:710,],"time_diff", "PM25 intake 2", "x")

num_integ_function(dataset=sample2, variable="PM25_intake", t1=1050, t2=1727)
num_integ_function(dataset=sample2, variable="PM25_intake", t1=210, t2=780)
num_integ_function(dataset=sample2, variable="PM25_intake", t1=781, t2=1049)

num_integ_function(dataset=sample2, variable="PM25_intake", t1=210, t2=710)-
  num_integ_function(dataset=sample2, variable="PM25_intake", t1=1189, t2=1757)
#704 meters difference, 0.90 PM per sec, 1.5 minutes difference, 936 integration difference
#the difference is extremely marginal

#sample3
myfunction_allosm(sample3,"PM25_intake", "PM25 intake", "sample3")

#sample4
myfunction_allosm(sample4,"PM25_intake", "PM25 intake", "sample4")
myfunction_allosm(sample4[1100:1748,],"PM25_intake", "PM25 intake", "y")

#sample6
myfunction_allosm(sample6,"PM25_intake", "PM25 intake", "sample6")
myfunction_allosm(sample6[1:540,],"PM25_intake", "PM25 intake", "y")
myfunction_allosm(sample6[553:2398,],"PM25_intake", "PM25 intake", "y")
myfunction_allosm(sample6[2401:3125,],"PM25_intake", "PM25 intake", "y")#left side of the regents park 

num_integ_function(dataset=sample6, variable="PM25_intake", t1=1, t2=552)
num_integ_function(dataset=sample6, variable="PM25_intake", t1=553, t2=2398) 
num_integ_function(dataset=sample6, variable="PM25_intake", t1=2401, t2=3125)

num_integ_function(dataset=sample6, variable="PM25_intake", t1=690, t2=2110) 
num_integ_function(dataset=sample6, variable="PM25_intake", t1=1, t2=551) 
num_integ_function(dataset=sample6, variable="PM25_intake", t1=2401, t2=3125)


#running to primrose hill, even through regent's park pays off when you spend there more than 2 minutes
num_integ_function(dataset=sample6, variable="PM25_intake", t1=552, t2=2400) -
  num_integ_function(dataset=sample6, variable="PM25_intake", t1=1, t2=551) - 
  num_integ_function(dataset=sample6, variable="PM25_intake", t1=2401, t2=3125)

#sample7
myfunction_allosm(sample7,"PM25_intake", "PM25 intake", "sample7")
myfunction_allosm(sample7[1:800,],"PM25_intake", "PM25 intake", "y")#left side of the regents park -road
myfunction_allosm(sample7[230:800,],"PM25_intake", "PM25 intake", "y")
myfunction_allosm(sample7[801:1270,],"PM25_intake", "PM25 intake", "y")
myfunction_allosm(sample7[1271:1978,],"PM25_intake", "PM25 intake", "x")

num_integ_function(dataset=sample7, variable="PM25_intake", t1=1, t2=800)
num_integ_function(dataset=sample7, variable="PM25_intake", t1=801, t2=1270)
num_integ_function(dataset=sample7, variable="PM25_intake", t1=1271, t2=1978)

#sample8
myfunction_allosm(sample8,"PM25_intake", "PM25 intake", "sample8")

#sample9
myfunction_allosm(sample9,"PM25_intake", "PM25 intake", "sample9")
myfunction_allosm(sample9[1:209,],"PM25_intake", "PM25 intake", "sample9_A")
myfunction_allosm(sample9[210:922,],"PM25_intake", "PM25 intake", "sample9_B")
myfunction_allosm(sample9[923:1122,],"PM25_intake", "PM25 intake", "sample9_C")

num_integ_function(dataset=sample9, variable="PM25_intake", t1=1, t2=209)
num_integ_function(dataset=sample9, variable="PM25_intake", t1=210, t2=922)
num_integ_function(dataset=sample9, variable="PM25_intake", t1=923, t2=1122)

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


#synthetic run
syn1 <- rbind(sample1[1:185,],sample9[209:923,],sample1[1664:1824,])

myfunction_allosm(syn1,"time_diff", "PM25 intake", "syn1")

num_integ_function(dataset=sample9, variable="PM25_intake", t1=209, t2=923) - 
  num_integ_function(sample1, "PM25_intake", t1=1, t2=185) -
  num_integ_function(sample1, "PM25_intake", t1=1664, t2=1824)


#run in park was 734 meters and almost 2 minutes longer than a commute to the park, but was characterized by lower amount of inhaled pollutants

#from UCL to Regents Park - less and more crowded roads
num_integ_function(sample1, "PM25_intake", t1=1, t2=185) -#more crowded
  num_integ_function(sample9, "PM25_intake", t1=1, t2=209) #less crowded 

myfunction_allosm(sample9[1:209,],"PM25_intake", "PM25 intake", "sample9_A")
myfunction_allosm(sample1[1:185,],"PM25_intake", "PM25 intake", "sample1_A")

#this shows that minor roads are characterized by lower air pollution concentration, but it takes more time to run there

#compare synthetic with sample9
num_integ_function(dataset=syn1, variable="PM25_intake", t1=1, t2=1061) - 
  num_integ_function(dataset=sample9, variable="PM25_intake", t1=1, t2=1122)  



#compare
num_integ_function(dataset=sample6, variable="PM25_intake", t1=1, t2=540)
num_integ_function(dataset=sample7, variable="PM25_intake", t1=1271, t2=1978)

myfunction_allosm(sample7[c(1349:1978),],"PM25_intake", "PM25 intake", "x")

num_integ_function(dataset=sample7, variable="PM25_intake", t1=1271, t2=1304)
num_integ_function(dataset=sample7, variable="PM25_intake", t1=1349, t2=1978)

#left side of regents park
num_integ_function(dataset=sample6, variable="PM25_intake", t1=2401, t2=3125) #park
num_integ_function(dataset=sample7, variable="PM25_intake", t1=1, t2=800) #road

#regent's park vs primrose hill
num_integ_function(dataset=sample1, variable="PM25_intake", t1=532, t2=1115) #park
num_integ_function(dataset=sample6, variable="PM25_intake", t1=2401, t2=3125) #park

#regents park left or right
num_integ_function(dataset=sample4, variable="PM25_intake", t1=1100, t2=1748) #park
num_integ_function(dataset=sample6, variable="PM25_intake", t1=2401, t2=3125) #park

#regents park right side - park vs road
myfunction_allosm(sample6[220:645,],"PM25_intake", "PM25 intake 6", "y")
num_integ_function(dataset=sample6, variable="PM25_intake", t1=220, t2=645)
myfunction_allosm(sample4[1100:1570,],"PM25_intake", "PM25 intake 4", "y")
num_integ_function(dataset=sample4, variable="PM25_intake", t1=1100, t2=1570) #park

#road to regent park and regents park
#początek
myfunction_allosm(sample6[220:500,],"PM25_intake", "PM25 intake 6", "y")
num_integ_function(dataset=sample6, variable="PM25_intake", t1=1, t2=219)-
  num_integ_function(dataset=sample6, variable="PM25_intake", t1=220, t2=520)

#koniec ---> dobra to jest pierwsza ciekawa rzecz
num_integ_function(dataset=sample6, variable="PM25_intake", t1=2930, t2=3125)-
  num_integ_function(dataset=sample6, variable="PM25_intake", t1=2627, t2=2929)
myfunction_allosm(sample6[2930:3125,],"PM25_intake", "PM25 intake 6", "y")
myfunction_allosm(sample6[2627:2929,],"PM25_intake", "PM25 intake 6", "y")
myfunction_allosm(sample6[2627:3125,],"PM25_intake", "PM25 intake 6", "sample6_koniec")

#you can run for almost minute and a half longer and 611 meters further 


#
run.pollution.spdf <- SpatialPointsDataFrame(run.pollution[,c("lon", "lat")], run.pollution, proj4string = CRS("+init=epsg:4326"))
run.pollution.spdf <- spTransform(run.pollution.spdf, CRS("+init=epsg:27700"))

sam <- run.pollution.spdf[run.pollution.spdf@data$trackID==1968008352,]
plot(sam)
plot(sam[210:781,])
plot(sam[1000:1757,], col="red")


1050:1727


