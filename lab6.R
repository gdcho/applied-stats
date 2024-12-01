## lab 6
## David Cho (A01351217)

# Q1
## testing 300 chips
n <- 300
# 1/3 prob of "not thick enough coating"
p <- 1/3

# X = number of chips that have not thick enough coating
# This is a binomial variable
m.trials <- 10^5
X.vals <- integer(m.trials)

for (i in 1:m.trials){
  chips <- sample(c("thick", "not.thick"), n, replace = TRUE, prob = c(1-p, p))
  X.vals[i] <- sum(chips == "not.thick")
}

hist(X.vals, breaks=seq(-0.5, 300, 5), xlab="X", probability = TRUE)

# Q1.a What is the probability of X = 100?
dbinom(100, n, p)
# 0.0488

# Q1.b What is the probability of X <= 100?
pbinom(100, n, p) # cumulative probability
# 0.5271

# Q1.c What is the probability of X < 100?
pbinom(99, n, p)
# 0.4783

# Q1.d What is the probability of X >= 110?
1 - pbinom(109, n, p)
# 0.1228

# Q1.e What is the probability of 90 <= X <= 110?
pbinom(110, n, p) - pbinom(89, n, p)
# 0.8017

# R allows us to generate random numbers that follow a binomial distribution
# For example, suppose we roll 10 dice and let X = the number of times we get a 1
# This X is a binomial random variable with n = 10 and p = 1/6
rbinom(1, 10, 1/6)
# stimulates rolling a set of 10 dice 100 times
X.vals <- rbinom(100, 10, 1/6)
table(X.vals)
cbind(table(X.vals))
# get relative frequencies
table(X.vals)/100

# Q2. In this question, you will generate the probability distribution for 
# 𝑋 = the number of 1s obtained when rolling 10 dice

# Q2.a Write a function RollDice that simulates rolling n.dice dice r.reps times, using the
# sample function and techniques from Lab 4. Your function RollDice should output
# a table giving the relative frequencies of the different values of 𝑋 along with a
# probability histogram of 𝑋. (Hint: call hist with probability=TRUE and use breaks=
# to make exactly one rectangle for each integer value of 𝑋.) Run your function for
# n.dice=10, r.reps=10000. Record the table and probability histogram.

RollDice <- function(n.dice, r.reps){
 Dice.vals <- replicate(r.reps, sum(sample(1:6, n.dice, replace=TRUE) == 1))
 hist(Dice.vals, breaks = seq(-0.5, n.dice+0.5, 1))
 print(table(Dice.vals)/r.reps)
}

RollDice(10, 10000)

# Q2.b Write a function RollDiceBinom that simulates rolling n.dice dice r.reps times,
# using the rbinom function. As before, your function should output a table giving the
# relative frequencies, as well as a probability histogram. Run your function for
# n.dice=10, r.reps=10000 and provide a table and a probability histogram.

RollDiceBinom <- function(n.dice, r.reps){
 Dice.vals <- rbinom(r.reps, n.dice, 1/6)
 hist(Dice.vals, breaks = seq(-0.5, n.dice+0.5, 1))
 print(table(Dice.vals)/r.reps)
}

RollDiceBinom(10, 10000)


# Distribution of Sample Mean in a Uniformly Distributed Population
# 𝜇𝑋̅ = mean of all sample means
# 𝜎𝑋̅ = standard deviation of all sample means

# Q.3 Create a new function called
# DiceMeans that will be based on your RollSomeDice function. 
# Calling DiceMeans(n,m) will simulate rolling n dice m times. Each time it rolls n dice, 
# you should compute and store 𝑋̅ for that sample (i.e., the mean value of those n dice rolls).
# Your function should return the following:
# - The overall mean 𝜇𝑋̅ of the m sample means
# - The overall standard deviation 𝜎𝑋̅ of the m sample means
# - A relative frequency histogram (drawn using barplot with space=0) of the m
# sample means. The title should be “Distribution of Xbar for n Dice Rolls, Based
# on m Trials” where m and n are the values in the function call. Label the axes
# appropriately.

# Run your function for m=10000 and for the following
# values of n: 1 (i.e, roll a single die), 2 (roll 2 dice), 10, 50,
# and 100. Part of the histogram for the case n=2 is
# shown to the right – yours should look the same.
# Submit five outputs (histograms + corresponding 𝜇𝑋̅ and 𝜎𝑋̅. )
# How do the means and standard deviations
# change as n increases? How do the shapes of the
# histograms compare?

DiceMeans <- function(n, m){
  Dice.vals <- replicate(m, mean(sample(1:6, n, replace=TRUE)))
  mu <- mean(Dice.vals)
  sigma <- sd(Dice.vals)
  barplot(table(Dice.vals)/m, space=0, main=paste("Distribution of Xbar for", n, "Dice Rolls, Based on", m, "Trials"), 
          xlab="Xbar", ylab="P(Xbar)")
  return(c(mu, sigma))
}

DiceMeans(1, 10000)
DiceMeans(2, 10000)
DiceMeans(10, 10000)
DiceMeans(50, 10000)
DiceMeans(100, 10000)

# Means: The mean of the sample means stays approximately the same around 3.5 as n increases.

# Standard Deviations: The standard deviation of the sample means decreases as n increases, as per the 1/root n rule.

# Histogram Shape: The histogram becomes more bell-shaped and approaches a normal distribution as n increases due to the Central Limit Theorem.


RollDice <- function(n.dice, r.reps){
  Dice.vals <- replicate(r.reps, sum(sample(1:6, n.dice, replace=TRUE) == 1))
  hist(Dice.vals, breaks = seq(-0.5, n.dice+0.5, 1))
  print(table(Dice.vals)/r.reps)
}


library(mosaic)
# you are applying for jobs, and you send out twenty applications. there is 15% chance of receiving an offer
# let X = the number of offers you received
X.vals <- replicate(10^5, sum(sample(c("offer", "no.offer"), 20, replace=TRUE, prob=c(0.15, 0.85)) == "offer"))

dbinom(1, 20, 0.15)
dbinom(2, 20, 0.15)

# Q1.d What is the probability of X >= 110?
1 - pbinom(109, n, p)

1-pbinom(1, 20, 0.15)

histogram(X.vals, breaks=seq(-0.5, 20+0.5, by=1), type="density", xlab="X", ylab="P(X)")



