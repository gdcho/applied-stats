## Lab10
## David Cho
## A01351217

library(mosaicData)
library(dplyr)

data(TenMileRace)

# net time variable
hist(TenMileRace$net)
qqnorm(TenMileRace$net)


# Q.1 Generate 100 random samples of size 15 from the population of net race times and
# construct a confidence interval for 𝜇 for each sample. Generate a visualization like
# the following to show how many such intervals contain 𝜇. Record how many of your
# confidence intervals out of 100 contain 𝜇.

# pop mean
mu <- mean(TenMileRace$net)

plot(NULL, xlim=c(4000, 7500), ylim=c(0, 101),
     xlab="100 Confidence Intervals for mu", ylab="interval (i)")
lines(c(5000, 7000), c (20, 60), col="red", lwd=20)

# example confidence interval
x.sample <- sample(TenMileRace$net, 20)
x.sample
t.test(x.sample)$conf.int[1]

mu <- mean(TenMileRace$net)

plot(NULL,  xlim=c(4000, 7500), ylim=c(0, 101),
     xlab="Net Time", ylab="interval (i)", main="100 Confidence Intervals for mu")

for (i in 1:100) {
  x.sample <- sample(TenMileRace$net, 15)
  
  ci <- t.test(x.sample)$conf.int
  ci.lower <- ci[1]
  ci.upper <- ci[2]
  
  segments(ci.lower, i, ci.upper, i, col="red")
}

abline(v=mu, col="blue", lwd=1, lty="dashed")

# load cats data frame, which is part of MASS library
library(MASS)
data(cats)

# Q.2 Find 95% confidence intervals for body weight and heart weight for male and female
# cats (i.e, you will find four confidence intervals). Note that these samples are large
# enough that we don’t have to worry about whether they are normally distributed.
# Include sentences interpreting your results. Based on your intervals, does there
# appear to be a significant difference between the heart weights of male cats versus
# female cats? How about body weights?

cats_male <- subset(cats, Sex == "M")
cats_female <- subset(cats, Sex == "F")

male_Bwt_CI <- t.test(cats_male$Bwt)$conf.int
male_Hwt_CI <- t.test(cats_male$Hwt)$conf.int
female_Bwt_CI <- t.test(cats_female$Bwt)$conf.int
female_Hwt_CI <- t.test(cats_female$Hwt)$conf.int

male_Bwt_CI
male_Hwt_CI
female_Bwt_CI
female_Hwt_CI

x.sample <- sample(TenMileRace$net, 50)
xbar <- mean(x.sample)
s <- sd(x.sample)
t.crit <- qt(1-0.025, df=50) # table - 2.0086


# Q.3 Go back to the TenMileRace dataframe and calculate a 95% confidence interval
# using the formula on page 1 (not using t.test) using one sample of each sample size:
# a. 𝑛 = 50
# b. 𝑛 = 100
# c. 𝑛 = 150
# For each 𝑛, write a one sentence conclusion regarding 𝜇. Also, write a sentence to
# observe how the confidence interval widths change as 𝑛 increases

x.sample <- sample(TenMileRace$net, 50)
xbar <- mean(x.sample)
s <- sd(x.sample)
t.crit <- qt(1-0.025, df=50) # table - 2.0086
# calculate standard error
se <- s/sqrt(50)
# calculate margin of error
me <- t.crit * se
# determine confidence interval
xbar - me
xbar + me

x.sample <- sample(TenMileRace$net, 100)
xbar <- mean(x.sample)
s <- sd(x.sample)
t.crit <- qt(1-0.025, df=100) # table - 1.984
# calculate standard error
se <- s/sqrt(100)
# calculate margin of error
me <- t.crit * se
# determine confidence interval
xbar - me
xbar + me

x.sample <- sample(TenMileRace$net, 150)
xbar <- mean(x.sample)
s <- sd(x.sample)
t.crit <- qt(1-0.025, df=150) # table - 1.976
# calculate standard error
se <- s/sqrt(150)
# calculate margin of error
me <- t.crit * se
# determine confidence interval
xbar - me
xbar + me

# automating the interpretation step
View(cats)
t.test(cats_male$Bwt)$conf.int

# extract attribute using:
attr(t.test(cats_male$Bwt)$conf.int, "conf.level")

ttest.res <- t.test(cats_male$Bwt)
limit.lower <- ttest.res$conf.int[1]
limit.upper <- ttest.res$conf.int[2]
conf.level <- attr(ttest.res$conf.int, "conf.level")
cat("We are", conf.level*100, "percent confident that the mean net time is between",
    round(limit.lower, 2), "and", round(limit.upper, 2), "seconds. ")


# Q.4 Write a function confidence that takes inputs: data (vector of sample data), var
# (variable name), unit (variable units), and conf.level (confidence level). The
# function returns an appropriate conclusion in the form of a sentence containing the
# confidence interval (as in the following examples). Use t.test if you like. If the user
# does not specify a confidence level, a default of 0.95 must be applied.
# Hint: you may find the missing function useful.
# Record at least three examples showing the various use cases of your function.

data("survey")

confidence <- function(data, var, unit = "", conf.level=0.95) {
  result <- t.test(data, conf.level = conf.level)
  ci <- result$conf.int
  sentence <- sprintf("We are %.0f%% confident that the mean %s is between %.2f and %.2f %s.", 
                      conf.level * 100, var, ci[1], ci[2], unit)
  return(sentence)
}

cat(confidence(survey$Age, "Age", "years", 0.90), "\n")
cat(confidence(survey$Age, "Age", "years"), "\n")
cat(confidence(survey$Height, "Height", "cm", 0.99), "\n")

# 𝜇  = 6.5 for a fair 12-sided dice
# n = 5 with values 3, 11, 5, 6, 11
# find 95% confidence interval
t.test(c(3, 11, 5, 6, 11), conf.level=0.95)
mean(c(3, 11, 5, 6, 11))
sd(c(3, 11, 5, 6, 11))
# t-table area
qt(1-0.025, df=4)
# E
2.776 * 2.8284 / sqrt(5)
# lower bound
x.sample <- sample(3, 11, 5, 6, 11)
ci <- t.test(c(3, 11, 5, 6, 11))$conf.int
ci.lower <- ci[1]
ci[2]


for (i in 1:10^6) {
  x.sample <- sample(3, 1,, 5, 6, 11)
  
  ci <- t.test(x.sample)$conf.int
  ci.lower <- ci[1]
  ci.upper <- ci[2]
  
}

# variable X is not normal
# perform 10^6 simulations of rolling fair 12 sided dice n = 5 times
# for each simulation find 95% confidence intervals
# 𝜇  = 6.5, what is the actual percentage of simulated intervals that contain 𝜇 for this sample size

roll_dice <- function(n) {
  return(sample(3, 1, 5, 6, 11))
}

calculate_ci <- function(x.sample) {
  result <- t.test(x.sample)
  return(result$conf.int)
}

contains_mu <- function(ci) {
  return(ci[1] <= 6.5 && ci[2] >= 6.5)
}

num_contain_mu <- 0
for (i in 1:10^6) {
  x.sample <- roll_dice(5)
  ci <- calculate_ci(x.sample)
  if (contains_mu(ci)) {
    num_contain_mu <- num_contain_mu + 1
  }
}

# Set parameters
n <- 5           # Sample size
nsim <- 10^6      # Number of simulations
mu <- 6.5        # True mean of the die rolls

# Simulate the die rolls
set.seed(123)    # For reproducibility
data <- matrix(sample(1:12, nsim, replace = TRUE), ncol = n)

# Compute sample means and standard deviations
xbar <- rowMeans(data)
s <- apply(data, 1, sd)

# Compute the standard errors
se <- s / sqrt(n)

# Compute the t critical value for 95% confidence interval
t_crit <- qt(0.975, df = n - 1)  # Degrees of freedom = n - 1

# Compute the confidence intervals
lower <- xbar - t_crit * se
upper <- xbar + t_crit * se

# Check if the true mean is within the confidence intervals
coverage <- (lower <= mu) & (mu <= upper)

# Calculate the percentage of intervals containing the true mean
coverage_percentage <- mean(coverage) * 100

# Print the result
print(paste("Percentage of intervals containing the true mean:", coverage_percentage, "%"))


# let 

