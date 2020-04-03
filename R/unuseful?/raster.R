#rasteress
#raster roads

lines_raster <- raster(extent(activity_lines), ncol=100, nrow=100, crs = projection(activity_lines))

lengths <- sapply(1:ncell(lines_raster), function(i) {
  tmp_rst <- lines_raster
  tmp_rst[i] <- 1
  tmp_shp <- rasterToPolygons(tmp_rst)
  
  if (gIntersects(activity_lines, tmp_shp)) {
    roads_utm_crp <- crop(activity_lines, tmp_shp)
    roads_utm_crp_length <- gLength(roads_utm_crp)
    return(roads_utm_crp_length)
  } else {
    return(0)
  }
})

lines_raster[] <- lengths / 1000

library(RColorBrewer)
spplot(lines_raster, scales = list(draw = TRUE), xlab = "x", ylab = "y", 
       col.regions = colorRampPalette(brewer.pal(9, "YlOrRd")), 
       sp.layout = list("sp.lines", activity_lines), 
       par.settings = list(fontsize = list(text = 15)), at = seq(1, 10, 1))
