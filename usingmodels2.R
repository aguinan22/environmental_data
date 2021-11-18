require(palmerpenguins)

t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)

t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

boxplot(body_mass_g ~ species, data = penguins)

dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)

shapiro.test(dat_chinstrap$body_mass_g)

aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

fit_species = lm(body_mass_g ~ species, data = penguins)

summary(fit_species)

anova(fit_species)

fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

fit_interactive = lm(body_mass_g ~ sex * species, data = penguins)

summary(fit_interactive)

lm(bill_length_mm ~ body_mass_g, data = penguins)

boxplot(formula = body_mass_g ~ sex*species, data = penguins, main = "Penguin body mass (g) vs sex and species", xlab = "", ylab = "Body mass (g)",
        names = c("Female \n Adelie", "Male \n Adelie", "Female \n Chinstrap", "Male \n Chinstrap", "Female \n Gentoo", "Male \n Gentoo"), las = 2)

fit_both <- lm(body_mass_g ~ sex*species, data = penguins)

summary(fit_both)

3368.84 + 158.37
head(penguins)

chosenones <- subset(dat_chinstrap, sex == "female")

mean(chosenones$body_mass_g)
