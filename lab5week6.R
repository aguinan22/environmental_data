

#make ricker function ----

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

#make exponential function ----

exp_fun = function(x, a, b)
{
    return(a*exp(-b*x))
}

curve(
  exp_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Sample exponential function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

#make stochastic data ----

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred)

#add some funky stochasticity

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed)


#make error increase with increasing values of x
error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)
plot(x_sim, y_observed_2)

#try exponential function
set.seed(1234567)
y_observed_3 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd*rexp(x_sim,rate = 1.2))
plot(x_sim, y_observed_3)
#mike i have tried so many options and I have no idea what you want


par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)

par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")

dev.off()

#the actual lab stuff ----

require(here)

dat_salamander <- read.csv(here("data", "salamander_dispersal.csv"))

exp_fun = function(x, a, b)
{
  return(a*exp(-b*x))
}

curve(
  exp_fun(x, a = 1.9, b = 0.1), col = "black", add = TRUE, lty = "solid", from = 0, to = 20, ylab = "f(x)", main = "Q2: Negative Exponential curves"
)

curve(
  exp_fun(x, a = 1.9, b = 0.3), col = "black", add = TRUE, lty = "dotted", from = 0, to = 20, ylab = "f(x)", main = "Q2: Negative Exponential curves"
)

curve(
  exp_fun(x, a = 1.2, b = 0.2), col = "red", add = TRUE, lty = "solid", from = 0, to = 20, ylab = "f(x)", main = "Q2: Negative Exponential curves"
)

curve(
  exp_fun(x, a = 1.2, b = 0.4), col = "red", add = TRUE, lty = "dotted", from = 0, to = 20, ylab = "f(x)", main = "Q2: Negative Exponential curves"
)

#now for ricker curves

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, a = 25, b = 0.1), col = "black", lty = "solid", from = 0, to = 20, ylab = "f(x)", main = "Q5: Ricker Functions", add = FALSE
)

curve(
  ricker_fun(x, a = 20, b = 0.2), col = "black", lty = "dotted", from = 0, to = 20, ylab = "f(x)", main = "Q5: Ricker Functions", add = TRUE
)

curve(
  ricker_fun(x, a = 10, b = 0.2), col = "black", lty = "dotted", from = 0, to = 20, ylab = "f(x)", main = "Q5: Ricker Functions", add = TRUE
)

curve(
  ricker_fun(x, a = 75, b = 0.3), col = "red", lty = "solid", from = 0, to = 20, ylab = "f(x)", main = "Q5: Ricker Functions", add = TRUE
)

curve(
  ricker_fun(x, a = 50, b = 0.3), col = "red", lty = "dotted", from = 0, to = 20, ylab = "f(x)", main = "Q5: Ricker Functions", add = TRUE
)

curve(
  ricker_fun(x, a = 40, b = 0.3), col = "red", lty = "dotted", from = 0, to = 20, ylab = "f(x)", main = "Q: Ricker Functions", add = TRUE
)

#extra ricker curves for my own selfish purposes
curve(
  ricker_fun(x, a = 20, b = 0.2), col = "black", lty = "solid", from = 0, to = 20, ylab = "f(x)", main = "Q5: Ricker Functions", add = FALSE
)

curve(
  ricker_fun(x, a = 20, b = 0.4), col = "black", lty = "dotted", from = 0, to = 20, ylab = "f(x)", main = "Q5: Ricker Functions", add = TRUE
)
#distance class on x-axis, dispersal distance on y-axis

plot(dat_salamander$dist.class, dat_salamander$disp.rate.ftb, xlab = "Distance class (m)", ylab = "Dispersal rate (FTB)", main = "Distance class (m) vs Dispersal rate (FTB)")

#make up some sample lines
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

curve(line_point_slope(x, 0, 0.5, -0.00028), add = TRUE)

#now make up some sample exponential lines
curve(exp_fun(x, a = 5, b = 0.005), add = TRUE)

#and a ricker line
curve(ricker_fun(x, a = 0.01, b = 0.005), add = TRUE)

resids_linear <- c((dat_salamander$disp.rate.ftb - (-0.00028*dat_salamander$dist.class + 0.5)))

resids_exp <-c(dat_salamander$disp.rate.ftb - (5 *exp(-0.005 * dat_salamander$dist.class)))

resids_ricker <- c(dat_salamander$disp.rate.ftb - (0.01 * dat_salamander$dist.class * exp(-0.005 * dat_salamander$dist.class)))

resids_containment <- data.frame(resids_linear, resids_exp, resids_ricker)


hist(resids_linear, xlab = "Linear model residuals", main = "Histogram of linear residuals", col = "steel blue")

hist(resids_exp, xlab = "Exponential model residuals", main = "Histogram of exponential residuals", col = "steel blue")

hist(resids_ricker, xlab = "Ricker model residuals", main = "Histogram of Ricker residuals", col = "steel blue")


