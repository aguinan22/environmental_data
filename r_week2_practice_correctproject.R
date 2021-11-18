data(iris)
head(iris)
iris$Sepal.Width

mean(iris$Sepal.Length)
sd(iris$Sepal.Width)
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)

data_center_x = mean(iris$Sepal.Width)
data_center_y = mean(iris$Sepal.Length)
c(data_center_x, data_center_y)

points(x = data_center_x, y = data_center_y, col = "red")

#what in the lord's name am i doing
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

plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -0.1), 
  add = TRUE)

library(MASS)
data(Animals)
head(Animals)
str(Animals)

#absolutely filthy code as i reuse the same object for different things
data_center_x = mean(Animals$body)
data_center_y = mean(Animals$brain)

plot(x = Animals$body, y = Animals$brain, main = "Brain size vs body mass (unknown units)")
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x,
    data_center_x, 
    data_center_y,
    0.1), 
  add = TRUE)
  
  #let's do this again without stinky dipliodocus
  View(Animals)
  
  noMoredinos <- subset(Animals, body<7000)
  str(noMoredinos)
  
  plot(x = noMoredinos$body, y = noMoredinos$brain, main = "begone dinosaurs") 
  data_center_x = mean(noMoredinos$body)
  data_center_y = mean(noMoredinos$brain)
  curve(
    line_point_slope(
      x, 
      data_center_x, 
      data_center_y,
      1), 
    add = TRUE)
  