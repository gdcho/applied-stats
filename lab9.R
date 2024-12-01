## Lab9
## David Cho
## A01351217

# let x = standard normal variable (mu=0, sigma=1)
X.vals <- seq(-4, 4, by=0.01)
d.vals <- dnorm(X.vals, mean = 0, sd = 1)
plot(X.vals, d.vals, type="l", lwd=2)

# simulate 10^5 samples of size n = 10
n <- 10
m.trials <- 10^5
Xbar.vals <- double(m.trials)

for (i in 1:m.trials){
  Xbar.vals[i] <- mean(rnorm(n, mean = 0, sd=1))
}
Xbar.vals

hist(Xbar.vals, breaks = 100, freq=FALSE)
lines(X.vals, d.vals, type="l", col="red", lwd=2)
mean(Xbar.vals)
sd(Xbar.vals)
1/sqrt(10)

# Q.1 Create a function called NormalMeans that simulates taking m.trials samples of
# size n.sample from a normally distributed population with 𝜇 = 0 and 𝜎 = 1. For
# each sample, compute and store the sample mean, 𝑋̅ . Your function should output:
#- The grand mean of the m.trials sample means, which is an estimate of 𝜇𝑋̅
#- The standard deviation of the m.trials sample means, which is an estimate of 𝜎𝑋̅
#- An appropriately labelled probability histogram of the m.trials sample means
#(use breaks=100 to get 100 classes)
#Run your function for m.trials = 10^5 and each of n.sample = 1, 2, 10, 50, 100.
#For each n.sample, record the estimated value of 𝜇𝑋̅ and 𝜎𝑋̅ and the histogram.
#How do the means and standard deviations compare as n.sample increases?
#How do the shapes of the probability histograms compare to one another?


NormalMeans <- function(m.trials, n.sample) {
  Xbar.vals <- double(m.trials)
  for (i in 1:m.trials) {
    Xbar.vals[i] <- mean(rnorm(n.sample, mean = 0, sd = 1))
  }
  
  grand_mean <- mean(Xbar.vals)
  grand_sd <- sd(Xbar.vals)
  
  hist(Xbar.vals, breaks = 100, freq = FALSE, 
       main = paste("Probability Density of Sample Means (n = ", n.sample, ")", sep = ""),
       xlab = "Sample Means")
  
  x_vals <- seq(min(Xbar.vals), max(Xbar.vals), length.out = 1000)
  d_vals <- dnorm(x_vals, mean = grand_mean, sd = grand_sd)
  lines(x_vals, d_vals, col = "red", lwd = 2)
  
  return(c(grand_mean, grand_sd))
}

NormalMeans(10^5, 1)
NormalMeans(10^5, 2)
NormalMeans(10^5, 10)
NormalMeans(10^5, 50)
NormalMeans(10^5, 100)


## Q.2
## Create a function called ExponentialMeans that simulates taking m.trials samples
# of size n.sample from an exponentially distributed population with mean 𝛽 = 1.
# Your function should output the following:
#- The grand mean of the m.trials sample means, which is an estimate of 𝜇𝑋̅
#- The standard deviation of the m.trials sample means, which is an estimate of 𝜎𝑋̅
#- An appropriately labelled probability histogram of the m.trials sample means
#(use breaks=100 to get 100 classes)
#Run your function for m.trials = 10^4 and for each of n.sample = 1, 2, 10, 50, 100.
#For each n.sample, record the estimated value of 𝜇𝑋̅ and 𝜎𝑋̅ and the histogram.
#How do the means and standard deviations compare as n.sample increases?
#How do the shapes of the probability histograms compare to one another?

ExponentialMeans <- function(m.trials, n.sample) {
  Xbar.vals <- numeric(m.trials)
  for (i in 1:m.trials) {
    Xbar.vals[i] <- mean(rexp(n.sample, rate = 1)) 
  }
  grand_mean <- mean(Xbar.vals)
  grand_sd <- sd(Xbar.vals)
  hist(Xbar.vals, breaks = 100, freq = FALSE,
       main = paste("Probability Density of Sample Means (n = ", n.sample, ")", sep = ""),
       xlab = "Sample Means")
  x_vals <- seq(min(Xbar.vals), max(Xbar.vals), length.out = 1000)
  d_vals <- dnorm(x_vals, mean = grand_mean, sd = grand_sd)
  lines(x_vals, d_vals, col = "red", lwd = 2)
  return(c(grand_mean, grand_sd))
}
ExponentialMeans(10^4, 1)
ExponentialMeans(10^4, 2)
ExponentialMeans(10^4, 10)
ExponentialMeans(10^4, 50)
ExponentialMeans(10^4, 100)

## Q.3 If the sample mean 𝑋̅ comes from samples of size 𝑛 drawn from an exponential
#distribution, does 𝑋̅ obey the Empirical Rule? To answer this question, write a
#function called ExponentialMeansProb that takes three arguments:
#• m.trials (number of simulation trials)
#• beta (the mean 𝛽)
#• n.sample (sample size)
#• z (Z – score)
#The function should simulate and return the probability that
#𝜇 − 𝑧 ⋅ 𝜎
#√𝑛 < 𝑋̅ < 𝜇 + 𝑧 ⋅ 𝜎
#√𝑛
#(Recall that 𝜇 = 𝛽 and 𝜎 = 𝛽 for an exponential distribution.)
#Use m.trials = 105 and beta = 1. Complete the table below for the given values of
#n.sample and z. Do your results agree with the Empirical Rule? Why or why not?


ExponentialMeansProb <- function(m.trials, beta, n.sample, z) {
  Xbar.vals <- numeric(m.trials)
  for (i in 1:m.trials) {
    Xbar.vals[i] <- mean(rexp(n.sample, rate = 1 / beta))
  }
  
  mu <- beta
  sigma <- beta
  
  lower_bound <- mu - z * sigma / sqrt(n.sample)
  upper_bound <- mu + z * sigma / sqrt(n.sample)
  
  prob <- mean(Xbar.vals > lower_bound & Xbar.vals < upper_bound)
  
  return(prob)
}

ExponentialMeansProb(10^5, 1, 1, 1)
ExponentialMeansProb(10^5, 1, 1, 2)
ExponentialMeansProb(10^5, 1, 1, 3)
ExponentialMeansProb(10^5, 1, 5, 1)
ExponentialMeansProb(10^5, 1, 5, 2)
ExponentialMeansProb(10^5, 1, 5, 3)
ExponentialMeansProb(10^5, 1, 50, 1)
ExponentialMeansProb(10^5, 1, 50, 2)
ExponentialMeansProb(10^5, 1, 50, 3)



## qq plots
qnorm(0.01, mean = 100, sd = 15)
X.percentiles <- qnorm(seq(0.01, 0.99, by=0.01), mean = 100, sd = 15)
Z.percentiles <- qnorm(seq(0.01, 0.99, by =0.01))
plot(Z.percentiles, X.percentiles, type = "p")

normal.data = rnorm(30, mean=4, sd=2)
qqnorm(normal.data, pch=20, main="QQ Plot of a Sample (n=30)")
qqline(normal.data, col="red", lty="dashed")

normal.data = rnorm(300, mean=4, sd=2)
qqnorm(normal.data, pch=20, main="QQ Plot of a Sample (n=300)")
qqline(normal.data, col="red", lty="dashed")

qnorm(0.300)
qnorm(0.01)
qnorm(0.02)
qnorm(0.99)

library(mosaic)
# load TenMileRace
data(TenMileRace)
race.times <- TenMileRace$net
# display histogram of race.times with 100 breaks
hist(race.times)
## qq plot
race.times.sample <- TenMileRace$net[sample(1:nrow(TenMileRace),30)]
qqnorm(race.times.sample, main="QQ Plot for Net Race Times (n=30)",pch=20)
qqline(race.times.sample,lty="dashed",col="red")
                                                                      
## load the genotype dataframe with is part of MASS library
library(MASS)
data("genotype")
str(genotype)
summary(genotype)

## Q.4 Filter your data to obtain four data sets, grouped by Litter (genotype). Note that
#these data sets are small, so we require them to be normally distributed if we wish
#to find a confidence interval. Create QQ plots for weight for each genotype.
#Do the four samples appear to be normal?

genotype_A <- subset(genotype, Litter == "A")
genotype_B <- subset(genotype, Litter == "B")
genotype_I <- subset(genotype, Litter == "I")
genotype_J <- subset(genotype, Litter == "J")
genotype_A <- subset(genotype_A, !is.na(Wt))
genotype_B <- subset(genotype_B, !is.na(Wt))
genotype_I <- subset(genotype_I, !is.na(Wt))
genotype_J <- subset(genotype_J, !is.na(Wt))

par(mfrow = c(2, 2)) 

qqnorm(genotype_A$Wt, main = "QQ Plot for Genotype A", pch=20)
qqline(genotype_A$Wt)

qqnorm(genotype_B$Wt, main = "QQ Plot for Genotype B", pch=20)
qqline(genotype_B$Wt)

qqnorm(genotype_I$Wt, main = "QQ Plot for Genotype I", pch=20)
qqline(genotype_I$Wt)

qqnorm(genotype_J$Wt, main = "QQ Plot for Genotype J", pch=20)
qqline(genotype_J$Wt)
