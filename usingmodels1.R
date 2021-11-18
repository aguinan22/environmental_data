#catrate stuff

require(here)

catrate <- read.csv(here("data", "catrate.csv"))
head(catrate)

summary(catrate)
hist(catrate$cat.rate, main = "Histogram of Catastrophic Rate Frequencies", xlab = "Catastrophic Rate", col = "steel blue")

shapiro.test(catrate$cat.rate)

t.test(catrate$cat.rate, mu = 2/7)
t.test(catrate$cat.rate, mu = 2/7, alternative = "l")

#back to penguins
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)

boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")

adelies <- subset(penguin_dat, species == "Adelie")
chinstraps <- subset(penguin_dat, species == "Chinstrap")

shapiro.test(adelies$flipper_length_mm)
shapiro.test(chinstraps$flipper_length_mm)

t.test(flipper_length_mm ~ species, data = penguin_dat)

wilcox.test(flipper_length_mm ~ species, data = penguin_dat)

levels(penguin_dat$species)

#q11 on the assignment

wilcox.test(catrate$cat.rate, mu = 2/7)

#q18
par(mfrow = c(1,2))
hist(adelies$flipper_length_mm, main = "Adelie flipper length histogram", xlab = "Flipper length (mm)")
hist(adelies$flipper_length_mm, main = "Chinstrap flipper length histogram", xlab = "Flipper length (mm)")
