install.packages("psych")
require(psych)
require(here)

pairs.panels(iris)

dat_bird <- data.frame(read.csv(here("data","bird.sta.csv")))
dat_habitat <- data.frame(read.csv(here("data","hab.sta.csv")))
head(dat_bird)
head(dat_habitat)

dat_all <- merge(dat_bird, dat_habitat, all = TRUE)
plot(ba.tot ~ elev, data = dat_all)
#it did work

cewa_bool <- c(dat_bird$CEWA >= 1)
cewa_present_absent <- as.numeric(cewa_bool)

plot(x = dat_all$elev, y = cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

#question 2 pair plots

pairs.panels(dat_habitat[,c("ba.tot","slope", "aspect","elev")])

#bird species presence/absence (Violet-green swallow)

vgsw_bool <- c(dat_bird$VGSW >= 1)
vgsw_present_absent <- as.numeric(vgsw_bool)

plot(x = dat_all$ba.tot, y = vgsw_present_absent, main = "Violet-green Swallow Presence/Absence by Basal Area", ylab = "Presence vs. Absence", xlab = "Total basal area (m2/ha)")
curve(logistic_midpoint_slope(x, midpoint = 50, slope = -1), add = TRUE)

#bird species presence/absence (Red Crossbill)

recr_bool <- c(dat_bird$RECR >= 1)
recr_present_absent <- as.numeric(recr_bool)

plot(x = dat_all$ba.tot, y = recr_present_absent, main = "Red Crossbill Presence/Absence by Basal Area", ylab = "Presence vs. Absence", xlab = "Total basal area (m2/ha)")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.5), add = TRUE)

sum(dat_all$GRJA)

grja_presence <- c(dat_all$GRJA > 0)
sum(as.numeric(grja_presence))
