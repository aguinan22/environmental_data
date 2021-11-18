require(here)

#catrate the return
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

#represent success as binomial function
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)

binom.test(n_success, n_years, p = 5/7) 
#okay what if it's less than the pond late-fill rate
binom.test(
  n_success,
  n_years,
  p = 5/7,
  alternative='less')

veg = read.csv(here("data", "vegdata.csv"))
head(veg)

boxplot(pine ~ treatment, data = veg)

var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

shapiro.test(veg$pine[veg$treatment=="control"])
shapiro.test(veg$pine[veg$treatment=="clipped"])
#welp clipped is not quite normally distributed

fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))
#and the variances between the control and clipped plots are not homogeneous

#homogeneity of variance among all four samples??
bartlett.test(pine ~ treatment, data=veg)
#no

fligner.test(pine ~ treatment, data = veg)
#yeah variances are not homogeneous

t.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)
#don't trust this hoe we know the variances are not homogeneous and one of these distributions is non normal

wilcox.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control, clipped, paired=TRUE)
wilcox.test(control, clipped, paired=TRUE)

#okay time for some salamander data
disp = read.csv(here("data", "salamander_dispersal.csv"))
disp            
plot(disp$disp.rate.ftb, disp$disp.rate.eb)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)

plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

prop.test(c(4,16),c(40,250))

owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)

fisher.test(owls)

birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

#Q2
chisq.test(br_creeper_table)

#Q3
require(palmerpenguins)
fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)

fit_species

#Q4
fit_sex = 
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)
fit_sex

#Q5
fit_both = 
  lm(
    formula = body_mass_g ~ species*sex,
    data = penguins)

fit_both


#Q6

boxplot(formula = body_mass_g ~ species, data = penguins, main = "Penguin body mass (g) vs species", xlab = "Species", ylab = "Body mass (g)")

#Q7
boxplot(formula = body_mass_g ~ sex, data = penguins, main = "Penguin body mass (g) vs sex", xlab = "Sex", ylab = "Body mass (g)")

#Q8
boxplot(formula = body_mass_g ~ sex*species, data = penguins, main = "Penguin body mass (g) vs sex and species", xlab = "", ylab = "Body mass (g)",
        names = c("Female \n Adelie", "Male \n Adelie", "Female \n Chinstrap", "Male \n Chinstrap", "Female \n Gentoo", "Male \n Gentoo"), las = 2)

#Q11
bartlett.test(body_mass_g ~ species, data=penguins)

#Q12
bartlett.test(body_mass_g ~ sex, data=penguins)

#Q13
bartlett.test(penguinpanic$body_mass_g)

penguinpanic <- aggregate(body_mass_g ~ sex*species, data=penguins, FUN = c)
class(penguinpanic)


#SECRET MIKE KNOWLEDGE
sneakystore <- boxplot(formula = body_mass_g ~ sex*species, data = penguins, plot = FALSE)
str(sneakystore)

sneakystore$names
gsub("\\.", "\n", sneakystore$names)

boxplot(formula = body_mass_g ~ sex*species, data = penguins, names = gsub("\\.", "\n", sneakystore$names), las = 2, xlab = "", axes = TRUE)
