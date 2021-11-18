require(palmerpenguins)
require(here)


#initial png foolishness ----

pngslave = function(image_file)
  {
  require(here)
  png(
    here("images",image_file),
    width = 1200, height = 1000
  )

}

pngslave("nightmarehistogram.png")

dev.off()






#more goofiness with histograms ----

dat_vec <- penguins$body_mass_g

hist(dat_vec, col = "steelblue", main = "blueboy")

histoslave = function(dat_vec, my_title, x_label)
{
  hist(dat_vec, col = "steelblue", main = my_title, xlab = x_label)
}

sample(x = 1:100, size = 1000, replace = TRUE)

histoslave(dat_vec = sample(x = 1:100, size = 1000, replace = TRUE), my_title = "Suffer More", x_label = "woohoo")



#i do my actual lab now please ----

#example normal curve
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

#example penguin histogram
require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

dat_1 = rnorm(n = 344, mean = 4202, sd = 802)
dat_2 = rnorm(n = 344, mean = 4202, sd = 802)
dat_3 = rnorm(n = 344, mean = 4202, sd = 802)
dat_4 = rnorm(n = 344, mean = 4202, sd = 802)

par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

set.seed(12)
dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)

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

set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)



set.seed(42069)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)