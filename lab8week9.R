require(here)

dat_veg <- read.csv(here("data", "vegdata.csv"))
dat_bird <- read.csv(here("data", "bird.sub.csv"))
dat_hab <- read.csv(here("data", "hab.sub.csv"))

#thanos snap the penguins
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

#learn about two.boot()
require(simpleboot)

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

adelie <- subset(penguins, species == "Adelie", flipper_length_mm != "NA")
head(adelie)
chinstrap <- subset(penguins, species == "Chinstrap", flipper_length_mm != "NA")
head(chinstrap)

twoboot <- two.boot(adelie$flipper_length_mm, chinstrap$flipper_length_mm, boot_mean, R = 10000)
hist(twoboot)

#do actual tree data

boxplot(pine ~ treatment, dat = dat_veg)

dat_tree = droplevels(subset(dat_veg, treatment %in% c("control", "clipped")))

boxplot(pine ~treatment, dat = dat_tree)

tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)

require(boot)

boot.ci(boot.out = tree_boot)

hist(tree_boot$t, main = "Bootstrap sampling distribution")

quantile(tree_boot$t, 0.025)

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

#monte carlo tomfoolery

index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#monte carlo tomfoolery

m = 10000 
result = numeric(m)

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2]
} 

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

quantile(result, c(.05))

#actual assignment stuff

adelie <- subset(penguins, species == "Adelie", flipper_length_mm != "NA")
head(adelie)
chinstrap <- subset(penguins, species == "Chinstrap", flipper_length_mm != "NA")
head(chinstrap)

pen_boot <- two.boot(adelie$flipper_length_mm, chinstrap$flipper_length_mm, boot_mean, R = 10000)

hist(pen_boot$t, main = "Chinstrap and Adelie flipper length differences",xlab = "Difference in flipper length (mm)")

quantile(pen_boot$t, c(0.025, 0.975))

mean(pen_boot$t)

median(pen_boot$t)

sd(pen_boot$t)

pen_ecdf <- ecdf(pen_boot$t)
1 - pen_ecdf(-4.5)

pen_ecdf(-8)

#Q9
require(here)

dat_veg <- read.csv(here("data", "vegdata.csv"))

veg = droplevels(subset(dat_veg, treatment %in% c("control", "clipped")))
veg = veg[-c(5,7)]
t.test(pine ~ treatment, data = veg)

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

mean(clippedpine$pine) - mean(controlpine$pine)

#Q12-Q17
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

b.sidimu <- mean(dat_all$b.sidi)
b.sidisigma <- sd(dat_all$b.sidi)

b.sidi.standardized <- c((dat_all$b.sidi-b.sidimu)/b.sidisigma)

dat_all$s.sidi.standardized <- s.sidi.standardized
dat_all$b.sidi.standardized <- b.sidi.standardized

#fricken loop it again

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

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]
slope_observed

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
criticalvalue <- quantile(result, c(.05))

abline(v = criticalvalue, lty = 3, col = "red", lwd = 2)
