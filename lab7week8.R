require(here)

#fun times with the apply function

dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

apply(dat, MARGIN = 1, FUN = min)

apply(dat, MARGIN = 1, FUN = max)

apply(dat, MARGIN = 2, FUN = mean)


#summon the moths

moths = read.csv(here("data", "moths.csv"))
head(moths)

m = 10000

# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)

for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}

mean(result)


quantile(result,c(0.025,0.975))

#summon bootstrap

require(boot)

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = moths$anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)

str(myboot)

mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

quantile(
  myboot$t,
  c(0.025, 0.975))

moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)


#moth testing grounds----

# This clears the current R session's environment
rm(list = ls())

#IT WORKS NOW

require(here)

moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    for(j in 1:n_input_rows)
    {
      rows_j = sample(n_input_rows, size = j, replace=TRUE)

      t1 = input_dat[rows_j, ]

      t2 = apply(t1, 2, sum)

      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 10000)

rare_mean = apply(rarefact, 2, mean, na.rm = TRUE)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975), na.rm = TRUE)
rare = t(rbind(rare_mean, rare_quant))
head(rare)


matplot(
  rare,
  type='l',
  col = c("black", "red", "blue"),
  xlab='Number of sampling plots',
  ylab='Moth species richness',
  main='Moth species richness vs sampling plots')


legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))

#Q1

require(palmerpenguins)


gentoos <- subset(penguins, penguins$species == "Gentoo")
gentoos <- subset(gentoos, gentoos$bill_length_mm != "NA")
length(gentoos$bill_length_mm)

#Q2

sd(gentoos$bill_length_mm)

#Q3

df <- length(gentoos$bill_length_mm) - 1
qt(0.05, df, lower.tail = FALSE)

#Q4

sse_mean = function(x){
  x <- na.omit(x)
  stdev <- sd(x, na.rm = TRUE)
  sqrtn <- sqrt(length(x))
  SSE <- stdev/sqrtn
  return(SSE)
}


sse_mean(gentoos$bill_length_mm)


#Q5

xbar <- mean(gentoos$bill_length_mm)
gen2sse <- sse_mean(gentoos$bill_length_mm)
tcrit <- qt(0.05/2, df, lower.tail = FALSE)

CI <- c(xbar + gen2sse*tcrit, xbar - gen2sse*tcrit)

CI

#Q6-7

require(boot)

boot_mean <- function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

penguinboot <- boot(data = gentoos$bill_length_mm, statistic = boot_mean, R = 10000)

#Q7-Q8

quantile(
  penguinboot$t,
  c(0.025, 0.975))

#Q9

require(here)

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n_input_rows)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

moths <- read.csv(here("data", "moths.csv"))
moth_dat <- moths[,-1]
