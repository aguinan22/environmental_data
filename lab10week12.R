
#the homemade stuff
require(here)
rope <- read.csv(here("data", "rope.csv"))
rope$rope.type = as.factor(rope$rope.type)
#welp that wasn't the factor function but it looks like it worked
levels(rope$rope.type)

n_obs <- nrow(rope)
n_groups <- length(unique(rope$rope.type))

#dropping the other columns because they are annoying
rope <- rope[,c(1:3)]
#nice

ss_tot = sum((rope$p.cut - mean(rope$p.cut))^2)

agg_resids <- aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type), 
  FUN = function(x) x - mean(x))
str(agg_resids)

agg_sq_resids <- aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type), 
  FUN = function(x) sum((x - mean(x))^2))
str(agg_sq_resids)

head(agg_sq_resids)

ss_within <- sum(agg_sq_resids$x)

ss_among = ss_tot - ss_within

df_tot <- n_obs - 1

df_within <- n_obs - n_groups

df_among <- n_groups - 1

ms_among  =  ss_among / (n_groups - 1)

ms_within = ss_within / (n_obs - n_groups)

f_ratio = ms_among / ms_within

f_pval = 1 - pf(f_ratio, df_among, df_within)

#the automated way

fit_1 = lm(p.cut ~ rope.type, data=rope)
anova_fit_1 = anova(fit_1)

anova_fit_1$"Sum Sq"

#Q3
head(rope)
bartlett.test(p.cut ~ rope.type, data=rope)

#Q5-7

fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

levels(rope$rope.type)

0.36714 + 1*-0.10164


#run the test

# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)
