#tmap - pollution
tmap_mode('plot')
#pollution
tm_shape(run_map) + tm_borders(alpha=1, lty=2) + tm_shape(run.pollution.spdf) + 
  tm_dots(col = "PM25", palette=c("lightgreen", "firebrick1"), style = "quantile",shape=16, size=.03) +
  tm_layout(basemaps = c('OpenStreetMap')) 

#intake
tm_shape(run_map) + tm_borders(alpha=1, lty=2) + tm_shape(run.pollution.spdf) + 
  tm_dots(col = "PM25_intake", palette=c("lightgreen", "firebrick1"), style = "quantile",shape=16, size=.03) +
  tm_layout(basemaps = c('OpenStreetMap')) 

#average intake
tm_shape(run_map) + tm_borders(alpha=1, lty=2) + tm_shape(run.pollution.spdf) + 
  tm_dots(col = "avg_PM25_intake", palette=c("lightgreen", "firebrick1"), style = "quantile",shape=16, size=.03) +
  tm_layout(basemaps = c('OpenStreetMap')) 

#elevation
names(run.pollution)
myfunction_subsetosm(dataset=run.pollution, variable = "ele_api",
                     filename = "all_ele_api")
#speed
myfunction_subsetosm(dataset=run.pollution, variable = "speed_km.h",
                     filename = "speed_km/h")

