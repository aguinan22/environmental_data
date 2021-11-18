require(palmerpenguins)

sse_mean = function(x){
 x <- na.omit(x)
 stdev <- sd(x, na.rm = TRUE)
  sqrtn <- sqrt(length(x))
  SSE <- stdev/sqrtn
  return(SSE)
}

penguins_SSE <- sse_mean(penguins$bill_depth_mm)
#noice


boxplot(flipper_length_mm ~ species, data = penguins)

dat_pen <- subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))


#uhhh cooking up a function

two_group_resample = function(x, n_1, n_2) {

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

return(diff_simulated)

}


set.seed(54321)

mikehavemercy <- two_group_resample(dat_pen$flipper_length_mm, n_1 = 68, n_2 = 152)
mikehavemercy
#hhhuhuhhhhhh NICE

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed)

str(t_test)

#actual lab questions

#Q1

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

#Q4
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences > 1))


#q7
boxplot(body_mass_g ~ species, data = dat_pen, ylab = "Body mass (g)", xlab = "Species", main = "Species vs Body mass (g)")

penguinmasses <- aggregate(body_mass_g ~ species, data = dat_pen, FUN = mean, na.rm = TRUE)
diff_crit <- diff(penguinmasses[,2])
diff_crit

t_test = t.test(dat_pen$body_mass_g ~ dat_pen$species)
t_test

two_group_resample(dat_pen$flipper_length_mm, 68, 152)

n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$body_mass_g, 68, 152)
  )
}

hist(mean_differences, main = "Histogram of simulated mean differences", xlab = "Mean differences in body mass (g) between penguin species")

sum(abs(mean_differences) > diff_crit)


#make sure my q1 code actually works

rm(list = ls())

require(palmerpenguins)

sse_mean = function(x){
  x <- na.omit(x)
  stdev <- sd(x, na.rm = TRUE)
  sqrtn <- sqrt(length(x))
  SSE <- stdev/sqrtn
  return(SSE)
}

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

#it do >:)


boxplot(flipper_length_mm ~ species, data = dat_pen)

fakepenguindata <- two_group_resample(dat_pen$flipper_length_mm, 68, 152)


n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}

hist(mean_differences)
