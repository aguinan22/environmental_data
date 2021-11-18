require(here)

dat_bird <- read.csv(here("data", "bird.sta.csv"))
dat_habitat <- read.csv(here("data", "hab.sta.csv"))

head(dat_bird)

wiwa_counts = c(2, 6)
sum(log(dpois(x = wiwa_counts, lambda = 4)))

wiwr_counts <- dat_bird$WIWR

hist(wiwr_counts, main = "Histogram of Winter Wren Counts", xlab = "Winter Wren Counts", breaks = 0:7 - 0.5)

sum(log(dpois(x = wiwr_counts, lambda = 1.46)))

sum(log(dbinom(wiwr_counts, 1046, 0.001)))

wiwr_counts


#i will not forget this mike give us the free labor automation
prob_guess = seq(0.01, 0.99, length.out = 1000)
bin_fun <- function(prob_guess){
  sum(log(dbinom(x = dat_all$WIWR, size = n, prob = prob_guess)))
}