require(here)
delomys <- read.csv(here("data", "delomys.csv"))
head(delomys)

summary(delomys$body_mass)
summary(delomys$body_length)
shapiro.test(delomys$body_mass)
shapiro.test(delomys$body_length)

#cool so neither of those are normally distributed

plot(delomys$body_mass, delomys$body_length)

hist(delomys$body_mass)

hist(delomys$body_length)

boxplot(body_mass ~ binomial, data = delomys)

boxplot(body_mass ~ sex, data = delomys)

boxplot(body_mass ~ sex*binomial, data = delomys)

fit1 <- lm(body_length ~ body_mass, data = delomys)
fit2 <- lm(body_mass ~ sex, data = delomys)
fit3 <- lm(body_mass ~ binomial, data = delomys)
fit4 <- lm(body_mass ~ sex + binomial, data = delomys)
fit5 <- lm(body_mass ~ sex*binomial, data = delomys)

hist(residuals(fit1))
hist(residuals(fit2))
hist(residuals(fit3))
hist(residuals(fit4))
hist(residuals(fit5))

shapiro.test(residuals(fit1))
shapiro.test(residuals(fit2))
shapiro.test(residuals(fit3))
shapiro.test(residuals(fit4))
shapiro.test(residuals(fit5))

summary(lm(body_length ~ body_mass, data = delomys))

abline(fit1)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)

anova(fit1)
anova(fit2)
anova(fit3)
anova(fit4)
anova(fit5)

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)
