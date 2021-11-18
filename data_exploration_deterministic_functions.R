require(here)

dat_habitat <- read.csv(here("data","hab.sta.csv"))

head(dat_habitat)

par(mfrow = c(3, 1))
hist(dat_habitat$elev, xlab = "Elevation", main = "Elevation histogram")
hist(dat_habitat$aspect, xlab = "Aspect", main = "Aspect histogram", breaks = c(0,60,120,180,240,300,360))
hist(dat_habitat$slope, xlab = "Slope", main = "Slope histogram")
dev.off()

par(mfrow = c(3, 1))
plot(dat_habitat$elev, dat_habitat$ba.tot, xlab = "Elevation (m)", ylab = "Total basal area (m2/ha)", main = "Elevation vs. Total basal area", cex = 0.7)
plot(dat_habitat$aspect, dat_habitat$ba.tot, xlab = "Aspect (degrees)", ylab = "Total basal area (m2/ha)", main = "Aspect vs. Total basal area", cex = 0.7)
plot(dat_habitat$slope, dat_habitat$ba.tot, xlab = "Slope (%)", ylab = "Total basal area (m2/ha)", main = "Slope vs. Total basal area", cex = 0.7)

range(dat_habitat$elev)

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

lm(dat_habitat$ba.tot ~ dat_habitat$elev)
curve(line_point_slope(x, x1 = 0, y1 = 20.82, slope = 0.004), add = TRUE, col = "blue")

lm(dat_habitat$ba.tot ~ dat_habitat$aspect)
curve(line_point_slope(x, x1 = 0, y1 = 19.96, slope = 0.013), add = TRUE, col = "red")

lm(dat_habitat$ba.tot ~ dat_habitat$slope)
curve(line_point_slope(x, x1 = 0, y1 = 19.79, slope = 0.052), add = TRUE, col = "green")
