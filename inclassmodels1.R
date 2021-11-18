require(here)
require(palmerpenguins)

dat_ade <- droplevels(subset(penguins,species == "Adelie"))

hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

head(dat_ade)
boxplot(body_mass_g ~ sex, dat = dat_ade, main = "Body Mass of Adelie Penguins", xlab = "Sex", ylab = "Body Mass (g)")

ade_fem <- subset(dat_ade, dat_ade$sex == "female")

t.test(ade_fem$body_mass_g, alternative = "two.sided", mu = 0)

ade_male <- subset(dat_ade, dat_ade$sex == "male")

t.test(ade_male$body_mass_g, alternative = "greater", mu = 4000)

t.test(ade_fem$body_mass_g, ade_male$body_mass_g, alternative = "two.sided")

mean(ade_fem$body_mass_g) - mean(ade_male$body_mass_g)

t.test(ade_male$body_mass_g, ade_fem$body_mass_g, alternative = "greater")

t.test(ade_male$body_mass_g, ade_fem$body_mass_g, alternative = "less")

t.test(body_mass_g ~ sex, dat_ade, alternative = "g")
#wowee