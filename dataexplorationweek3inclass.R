install.packages("palmerpenguins")
install.packages("here")

require(palmerpenguins)
require(here)

class(penguins)
head(penguins)

mean(penguins$bill_length_mm, na.rm = TRUE)

summary(penguins)

boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

par(mfrow = c(1, 2))
    
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

coplot(body_mass_g ~ bill_depth_mm | species, data = penguins, xlab = "Bill depth (mm)", ylab = "Body mass (g)")
plot(penguins$body_mass_g, penguins$bill_depth_mm, main = "Body mass (g) vs Beak depth (mm)", xlab = "Body mass (g)", ylab = "Beak depth (mm)")

plot(penguins[species = "gentoo","bill_depth_mm"], penguins[species = "gentoo", "body_mass_g"])

plot(penguins$body_mass_g)

png(filename = here("penguins_beak_coplot.png"), width = 800, height = 600)
coplot(body_mass_g ~ bill_depth_mm | species, data = penguins, xlab = "Bill depth (mm)", ylab = "Body mass (g)")
dev.off()

png(filename = here("penguins_beak_mass_conspiracytheory.png"), width = 800, height = 600)
hist(penguins$bill_depth_mm/penguins$body_mass_g, xlab = "Bill depth (mm) / Body mass (g)", main = "Propotion of bill depth (mm) to body mass (g)")
dev.off()