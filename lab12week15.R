#TIME FOR MAKE SILLY

require(here)

dat_bird <- read.csv(here("data", "bird.sub.csv"))
dat_hab <- read.csv(here("data", "hab.sub.csv"))
birdhab <- merge(dat_bird, dat_hab)

fit_1 <- lm(birdhab$BRCR ~ birdhab$ls)

fit_1_coefs <- coefficients(fit_1)

fit_1_summary <- summary(fit_1)

int_obs <- fit_1_coefs[1]
slope_obs <- fit_1_coefs[2]
sd_obs <- fit_1_summary$sigma

alpha = 0.05
n_sims = 30
p_vals = numeric(n_sims)

linear_simulator <- function(x, y_int, slope, st_dev){
  y = y_int + slope*x
  var = rnorm(length(x), mean(x), st_dev)
  return(y + var)
}

linear_sim_fit = function(x, y, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}
  


# specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_power = numeric(n_sds)

for(j in 1:length(pop_sds))
{
  pop_sd_j = pop_sds[j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(x = x_vals,
                             y_int = int_obs,
                             slope = slope_obs,
                             st_dev = pop_sd_j)
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_power[j] = sum(p_vals < alpha) / n_sims
}

sim_output_dispersion = data.frame(
  sd = pop_sds,
  power = pop_sd_power)


#Q1
sim_sample_size = sim_output_dispersion

fit_lowess_50 = loess(power ~ sample_size, data = sim_sample_size, span = 0.5)

newdata_sample_size = data.frame(sample_size = seq(2, 20, length.out = 100)) 

plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_50, newdata = newdata_sample_size),
  type = "l",
  ylab = "Statistical Power", xlab = "Sample Size")

require(here)
dat_dispersal <- read.csv(here("data", "salamander_dispersal.csv"))
#you know maybe i don't want to do this right now