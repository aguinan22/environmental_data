#Q1
require(palmerpenguins)
require(simpleboot)

adelie <- na.omit(subset(penguins, species == "Adelie"))
head(adelie)
chinstrap <- na.omit(subset(penguins, species == "Chinstrap"))
head(chinstrap)

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

pen_boot <- two.boot(adelie$flipper_length_mm, chinstrap$flipper_length_mm, boot_mean, R = 10000)

sd(pen_boot$t)

#Q2
hist(pen_boot$t, main = "Chinstrap and Adelie flipper length differences",xlab = "Difference in flipper length (mm)")

#Q3

quantile(pen_boot$t, c(0.025, 0.975))

#Q5

pen_ecdf <- ecdf(pen_boot$t)

#Q6

1 - pen_ecdf(-4.5)

#Q7

pen_ecdf(-8)

#Q9

require(here)

dat_veg <- read.csv(here("data", "vegdata.csv"))

veg = droplevels(subset(dat_veg, treatment %in% c("control", "clipped")))
veg = veg[-c(5,7)]
wilcox.test(pine ~ treatment, data = veg)

#Q10

require(simpleboot)

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

controlpine <- subset(veg, veg$treatment == "control")
clippedpine <- subset(veg, veg$treatment == "clipped")

tree_boot <- two.boot(clippedpine$pine, controlpine$pine, boot_mean, R = 10000)
head(tree_boot$t)

quantile(tree_boot$t, c(0.025,0.975))

#Q13

require(here)

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

s.sidimu <- mean(dat_all$s.sidi)
s.sidisigma <- sd(dat_all$s.sidi)

s.sidi.standardized <- c((dat_all$s.sidi-s.sidimu)/s.sidisigma)

dat_all$s.sidi.standardized <- s.sidi.standardized

#Q14

m = 10000 
result = numeric(m)

for(i in 1:m)
{
  index_1 = sample(nrow(dat_all), replace = TRUE)
  index_2 = sample(nrow(dat_all), replace = TRUE)
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_all$b.sidi[index_1],
      s.sidi = dat_all$s.sidi[index_2]
    )
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2]
}

#Q15
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
criticalvalue <- quantile(result, c(.05))

abline(v = criticalvalue, lty = 3, col = "red", lwd = 2)




