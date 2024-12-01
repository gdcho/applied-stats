## Lab8
## David Cho
## A01351217

library(lattice)

runif(n=1, min=0, max=20)
options(digits=20)
runif(n=1, min=0, max=20)

# the time T between incoming requests to a web server is exponentially distributed with mean = 0.0125 seconds.
# find the probability that T < 0.0125
pexp(0.0125, rate=1/0.0125) 
# find median value of T
qexp(0.5, rate=1/0.0125)
## find probability that T > 0.50
1 - pexp(0.50, rate=1/0.0125)

# let x = amount of time (in hours) a student spends studying
# Suppose x is normally distributed with mean of 20.0 and standarad deviation of 5.0

# find the probability that x>35.0
1 - pnorm(35.0, mean=20.0, sd=5.0)
# find the iqr range of x
qnorm(0.75, mean=20.0, sd=5.0) - qnorm(0.25, mean=20.0, sd=5.0)


## Q1 Generate an appropriately labelled histogram showing the probability density of
## waiting times 𝑋 based on 𝑚 = 100 trials. Use 1-minute class widths starting from
## 𝑋 = 0. Use the simulated 𝑋 values to estimate the probability that a person waits
## less than 6 minutes for the bus.


waiting_times1 <- runif(n=100, min=0, max=20)

# waiting_times1_filtered <- waiting_times1[waiting_times1 < 6]

hist(waiting_times1, breaks=seq(0, 20, by=1),
     probability=TRUE, 
     xlab="Waiting Time (mins)", 
     ylab="Density", 
     main="Probability Density of Waiting Times 20 Minutes (100 Trials)")

prob_less_6_1 <- mean(waiting_times1 < 6)
print(prob_less_6_1)


## Q2 Now make a second histogram using 𝑚 = 10^5 trials. Explain how and why the two
## histograms differ. Using the simulated X values to estimate the probability that a
## person waits less than 6 minutes for the bus.
waiting_times1 <- runif(n=100000, min=0, max=20)


# waiting_times1_filtered <- waiting_times1[waiting_times1 < 6]

# Create histogram
hist(waiting_times1, breaks=seq(0, 20, by=1),
     probability=TRUE, 
     xlab="Waiting Time (minutes)", 
     ylab="Density", 
     main="Probability Density of Waiting Times 20 Minutes (100 Trials)")

prob_less_6_1 <- mean(waiting_times1 < 6)
print(prob_less_6_1)

## Q3 Determine the theoretical probability that a person waits less than 6 minutes for the bus
theoretical_prob <- 1 - exp(-6/20)
print(theoretical_prob)

print(1-exp(-0.0125/0.0125))
print(1-exp(-0.5/0.0125))

## Q4 Use pexp determine the probability that a person waits less than 6 minutes,
## assuming that the waiting time 𝑋 follows an exponential distribution with mean 𝛽 =
## 20 minutes.
pexp(6, rate=1/20,lower.tail = TRUE, log.p = FALSE)

## Q5 Assuming the mean waiting time is 20 minutes and that 𝑋 follows an exponential
## distribution, use qexp to determine the median waiting time. 
## Hint: 50%. How does the median compare to the mean? Explain why in terms of skewness
qexp(0.5, rate=1/20, lower.tail = TRUE, log.p = FALSE)

## Q6.a
## The probability of waiting less than 10 minutes for a bus. [0.3934693]
pexp(10, rate=1/20, lower.tail = TRUE, log.p = FALSE)

## Q6.b
## The probability of waiting more than 15 minutes for a bus. [0.4723666]
1 - pexp(15, rate=1/20, lower.tail = TRUE, log.p = FALSE)

## 6.c
## The probability of waiting between 5 and 10 minutes for a bus. [0.1722701]
pexp(10, rate=1/20, lower.tail = TRUE, log.p = FALSE) - pexp(5, rate=1/20, lower.tail = TRUE, log.p = FALSE)


## Q7. Use rexp to generate appropriately-labelled histograms showing the frequency of
## waiting times (𝑋) between buses where 𝑋 is exponential distributed with mean 
## 𝛽 = 20. Use the same classes (i.e., breaks) for each, and use:
##      i. 𝑚 = 100 trials
##      ii. 𝑚 = 105 trials
## Do both distributions look exponential? Explain.
waiting_times1 <- rexp(n=100, rate=1/20)

histogram(waiting_times1, right=FALSE,
          breaks=seq(0, 500, by=5),
          type="density",
          xlim=c(-1,101),
          xlab="Waiting Time (minutes)", 
          ylab="Density", 
          main="Probability Density of Waiting Times 20 Minutes (100 Trials)")

waiting_times2 <- rexp(n=100000, rate=1/20)
histogram(waiting_times2, right=FALSE,
          breaks=seq(0, 500, by=5),
          type="density",
          xlim=c(-1,101),
          xlab="Waiting Time (minutes)", 
          ylab="Density", 
          main="Probability Density of Waiting Times 20 Minutes (100000 Trials)")

## Q.8 Using 𝑚 = 10^5 trials, find the probability that a person waits less than 6 minutes
## (assuming wait times are exponentially distributed with mean 𝛽 = 20 minutes).
## Note: this is the simulation version of question 4, so your result should be very similar.
waiting_times2 <- rexp(n=100000, rate=1/20)
print(mean(waiting_times2 < 6))

## Q.9 Use a formula from Lecture 5 to find the probability that a person will wait less than
## 6 minutes for a bus when the waiting times for buses follow an exponential
## distribution with mean 20 minutes.
print(pexp(6, rate=1/20, lower.tail = TRUE, log.p = FALSE))

## Q.10 he measured voltage, 𝑋, of batteries manufactured by a company follows a normal
## distribution with mean 9.01 V and standard deviation 0.05 V. Find the following

## Q.10a. The probability that 𝑋 is less than 9.03V [0.6554217]
options(digits=7)
pnorm(9.03, mean=9.01, sd=0.05)

## Q.10b. The probability that 𝑋 exceeds 9.02V [0.4207403]
options(digits=7)
1 - pnorm(9.02, mean=9.01, sd=0.05)

## Q.10-c. The probability that 𝑋 is between 8.9V and 9.1V [0.9501662]
options(digits=7)
pnorm(9.1, mean=9.01, sd=0.05) - pnorm(8.9, mean=9.01, sd=0.05)

## Q.11 The measured voltage 𝑋 of batteries manufactured by a company follow a normal
# distribution with mean 9.01 V and standard deviation 0.05 V. Find the following.

## Q.11a The voltage that is larger than 95% of measured voltages [9.092243]
qnorm(0.95, mean=9.01, sd=0.05)

## Q.11b The voltage that is lower than 95% of measured voltages [8.927757]
qnorm(0.05, mean=9.01, sd=0.05)

## Q.11c The 25th percentile voltage [8.976276]
qnorm(0.25, mean=9.01, sd=0.05)

## Q.12 Suppose the battery manufacturer will ship batteries whose measured voltages are
## between 8.9V and 9.1V. Give a command that returns the probability that a
## randomly selected battery can be shipped, based on:
## a. 𝑚 = 100 simulated trials
## b. 𝑚 = 100000 simulated trials
## [Note: this a simulation of the question in 10c, so your answers should be very similar.]

voltages1 <- rnorm(n=100, mean=9.01, sd=0.05)
voltages1_filtered <- voltages1[voltages1 > 8.9 & voltages1 < 9.1]
prob_voltages1 <- length(voltages1_filtered) / length(voltages1)
print(prob_voltages1)

voltages2 <- rnorm(n=100000, mean=9.01, sd=0.05)
voltages2_filtered <- voltages2[voltages2 > 8.9 & voltages2 < 9.1]
prob_voltages2 <- length(voltages2_filtered) / length(voltages2)
print(prob_voltages2)


## Q.13 Now use the formulas and tables from lecture to find the proportion of
## manufactured batteries that can be shipped – again your numbers should be very similar.

pnorm(9.1, mean=9.01, sd=0.05) - pnorm(8.9, mean=9.01, sd=0.05)



