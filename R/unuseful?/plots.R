layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))


#sample
sample$lowess_ele_api <- lowess(sample$ele_api, f = 0.025)$y
sample$lowess_speed <- lowess(sample$speed_km.h, f = 0.08)$y

plot(sample$ele_api, type = "l",bty = "n",  xaxt = "n",ylab = "Elevation", xlab = "", col = "grey70")
lines(lowess(sample$ele_api, f = 0.025)$y, col = "red", lwd = 1.5)
legend(x="bottom", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "red", "black"), lwd = c(1,3,2), lty=c(1,1,2), bty = "n")

plot(sample$speed_km.h, type = "l",bty = "n",  xaxt = "n",ylab = "Speed (km/h)", xlab = "", col = "grey70")
lines(lowess(sample$speed_km.h, f = 0.08)$y, col = "blue", lwd = 1.5)
legend(x="bottom", legend = c("Speed (km/h)", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), lty=c(1,1), bty = "n")

myfunction_allosm(dataset = sample, variable = "ele_api", legend = "API Elevation", filename = "sample_ele")
myfunction_allosm(dataset = sample, variable = "speed_km.h",legend="Speed (km/h)", filename = "sample_speed")
myfunction_allosm(dataset = sample, variable = "lowess_speed",legend="Lowess Speed (km/h)", filename = "sample_lowess_speed")

#sample2
sample2$lowess_ele_api <- lowess(sample2$ele_api, f = 0.025)$y
sample2$lowess_speed <- lowess(sample2$speed_km.h, f = 0.08)$y

plot(sample2$ele_api, type = "l",bty = "n",  xaxt = "n",ylab = "Elevation", xlab = "", col = "grey70")
lines(lowess(sample2$ele_api, f = 0.025)$y, col = "red", lwd = 1.5)
legend(x="bottom", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "red", "black"), lwd = c(1,3,2), lty=c(1,1,2), bty = "n")

plot(sample2$speed_km.h, type = "l",bty = "n",  xaxt = "n",ylab = "Speed (km/h)", xlab = "", col = "grey70")
lines(lowess(sample2$speed_km.h, f = 0.08)$y, col = "blue", lwd = 1.5)
legend(x="bottom", legend = c("Speed (km/h)", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), lty=c(1,1), bty = "n")

myfunction_allosm(dataset = sample2, variable = "ele_api", legend = "API Elevation", filename = "sample2_ele")
myfunction_allosm(dataset = sample2, variable = "speed_km.h",legend="Speed (km/h)", filename = "sample2_speed")
myfunction_allosm(dataset = sample2, variable = "lowess_speed",legend="Lowess Speed (km/h)", filename = "sample2_lowess_speed")

#sample3
sample3$lowess_ele_api <- lowess(sample3$ele_api, f = 0.025)$y
sample3$lowess_speed <- lowess(sample3$speed_km.h, f = 0.08)$y

plot(sample3$ele_api, type = "l",bty = "n",  xaxt = "n",ylab = "Elevation", xlab = "", col = "grey70")
lines(lowess(sample3$ele_api, f = 0.025)$y, col = "red", lwd = 1.5)
legend(x="bottom", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "red", "black"), lwd = c(1,3,2), lty=c(1,1,2), bty = "n")

plot(sample3$speed_km.h, type = "l",bty = "n",  xaxt = "n",ylab = "Speed (km/h)", xlab = "", col = "grey70")
lines(lowess(sample3$speed_km.h, f = 0.08)$y, col = "blue", lwd = 1.5)
legend(x="bottom", legend = c("Speed (km/h)", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), lty=c(1,1), bty = "n")

myfunction_allosm(dataset = sample3, variable = "ele_api", legend = "API Elevation", filename = "sample3_ele")
myfunction_allosm(dataset = sample3, variable = "speed_km.h",legend="Speed (km/h)", filename = "sample3_speed")
myfunction_allosm(dataset = sample3, variable = "lowess_speed",legend="Lowess Speed (km/h)", filename = "sample3_lowess_speed")