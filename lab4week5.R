#heehoo

require(here)

meanvalue <- 10.4
sdvalue <- 2.4

norm_17 <- rnorm(17, mean = meanvalue, sd = sdvalue)
norm_30 <- rnorm(30, mean = meanvalue, sd = sdvalue)
norm_300 <- rnorm(300, mean = meanvalue, sd = sdvalue)
norm_3000 <- rnorm(3000, mean = meanvalue, sd = sdvalue)


histofactory = function(imagename){
  hist(imagename, main = paste("Histogram containing ",length(imagename),"points"), xlab = "point value")
}

png(here("images", "lab_04_hist_01.png"),
    width = 1500,
    height = 1600,
    res = 180)

par(mfrow = c(2,2))

histofactory(norm_17)
histofactory(norm_30)
histofactory(norm_300)
histofactory(norm_3000)

dev.off()

#questions 7-8 time

require(here)

x = seq(-100,100, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)

svg(here("images","norm_1.svg"),)

plot(x, y, main = "Normal distribution with mean = 10.4 and sd = 2.4", type = "l", xlim = c(2,18))
abline(h = 0)
dev.off()

#now 9-10

#silly plot 1
set.seed(100)
n_pts = 100

x = rpois(n = n_pts,lambda = 400)
dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(x, col = "blue", pch = 42, main = "Chaos plot 1 (Poisson edition)", xlab = "None of it matters", ylab = "Why am I making this")


#silly plot 2
x = seq(-6,6, length.out = 1000)
y = dnorm(rnorm(x), mean = 10, sd = 2)

plot(x, y, main = "AAAAAAA", type = "b", xlab = "let the chaos consume you", ylab = "hhehehe")

#silly plot 3 where we mutilate the data

x = sample(rnorm(1200),size = 940, replace = FALSE)
y = dnorm(rnorm(x))

plot(x,y, type = "l", col = adjustcolor("red", alpha.f = 0.5), main = "demon demon demon", xlab = "silly time", ylab = "w")


#little more normal
x = rgamma(2000,shape = 2)
y = rbeta(2000, 1, 2)

plot(x,y, main = "fireworks time")

for(i in 1:100){
points(sample(x),sample(y),col = sample(x))
}

par(mfrow = c(2,2))
#silly plot 1
set.seed(100)
n_pts = 100

x = rpois(n = n_pts,lambda = 400)
dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(x, dat$y_observed, col = "blue", pch = 42, main = "Chaos plot 1 (Poisson edition)", xlab = "None of it matters", ylab = "Why am I making this")

#silly plot 2
x = seq(-6,6, length.out = 1000)
y = dnorm(rnorm(x), mean = 10, sd = 2)

plot(x, y, main = "AAAAAAA", type = "b", xlab = "let the chaos consume you", ylab = "hhehehe")

#silly plot 3 where we mutilate the data

x = sample(rnorm(1200),size = 940, replace = FALSE)
y = dnorm(rnorm(x))

plot(x,y, type = "l", col = adjustcolor("red", alpha.f = 0.5), main = "demon demon demon", xlab = "silly time", ylab = "w")


#little more normal
x = rgamma(2000,shape = 2)
y = rbeta(2000, 1, 2)

plot(x,y, main = "fireworks time")

for(i in 1:100){
  points(sample(x),sample(y),col = sample(x))
}


#11 and 12

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

lm(dat$y_observed~dat$x)

dev.off()

curve(line_point_slope(x,0,2.129297,-0.004888), add = TRUE)

#time for 13 i think

dat$y_predicted <- dat$x*-0.004888 + 2.129297
#now you might be saying those are some very specific slope and intercept numbers that is because i did not feel like guessing so i just plugged the variables into lm() and let it do the dirty work

dat$resids <- dat$y_observed - dat$y_predicted

#nice now for 14

hist(dat$resids, main = "Residuals of fitted data for plot 1", xlab = "Residuals", col = "steelblue")
plot(dat$y_predicted,dat$resids, main = "Predicted y-values vs residuals", xlab = "Predicted y-values", ylab = "Residuals")
#not fully sure about why I had to make this
#gonna do another one with x bc i feel like that might be more meaningful

plot(dat$x, dat$resids, main = "x-values vs residuals for random plot 1", xlab = "Residuals", ylab = "x-values")
#oh  wait i get it now if it's really nice the first plot should be a diagonal line