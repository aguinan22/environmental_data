#random walk ----

x_start = 0
y_start = 0

n_steps = 100

step_data = matrix(0, ncol = 2, nrow = n_steps + 1)
step_data[1,1] = x_start
step_data[1,2] = y_start

for(i in 1:n_steps){
  
  x_current = step_data[i, 1]
  y_current = step_data[i, 2]
  
  x_next = x_current + runif(n = 1, min = -1, max = 1)
  y_next = y_current + runif(n = 1, min = -1, max = 1)
  
  step_data[i+1, 1] = x_next
  step_data[i+1, 2] = y_next
  
}

plot(step_data[,1], step_data[,2], type = "l")
#lmaoo why did this make europe

#Random walk function v1

r_walk_1 <- function(x_start, y_start, n_steps){
  step_data = matrix(0, ncol = 2, nrow = n_steps + 1)
  step_data[1,1] = x_start
  step_data[1,2] = y_start
  
  for(i in 1:n_steps){
    
    x_current = step_data[i, 1]
    y_current = step_data[i, 2]
    
    x_next = x_current + runif(n = 1, min = -1, max = 1)
    y_next = y_current + runif(n = 1, min = -1, max = 1)
    
    step_data[i+1, 1] = x_next
    step_data[i+1, 2] = y_next
    
    return(step_data)
  }
  
}

plot(step_data[,1], step_data[,2], type = "l")
#Why Do I Keep Making Europe


#linear function silly time ----

slope = 1
intercept = 1

x = 1:5
#this whole time.. i didn't even have to type c()
class(x)
#wait how is it an integer..
#..wtf

y = slope*x + intercept

#oh that is a fuckload of x and y stuff

linear = function(slope, intercept, xmin, xmax){
  
  x = xmin:xmax
  
  y = slope*x + intercept
  
  return(y)
}


yvals <- linear(12, 5, 0, 100)
plot(yvals)

#alternatively we will cut out the middle man

linear2 = function(slope, intercept, xmin, xmax){
  
  x = xmin:xmax
  
  y = slope*x + intercept
  
  plot(y)
}

linear2(12, 5, 0, 100)
#heck yeah


#plot slave ----

require(palmerpenguins)

boxplot(body_mass_g ~ sex, data = penguins)
boxplot(body_mass_g ~ sex, data = droplevels(subset(penguins, penguins$sex == "male")))

bp_sex = function(dat){
  
  boxplot(body_mass_g ~ sex, data = droplevels(dat))
}

bp_sex(penguins)

bp_sex(subset(penguins, penguins$species == "Gentoo"))
