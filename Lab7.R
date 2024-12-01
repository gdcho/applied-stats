## Lab 7 Hypergeometric, geometric, poisson
## David Cho (A01351217)


# 1. Suppose we are drawing cards from a standard 52-card deck without replacement.
# Using dhyper, create a table and a barplot that gives that exact probability
# distribution for 𝑋 = the number of Aces obtained when drawing 8 cards without
# replacement. Give clear and descriptive labels for your barplot

# draw 8 cards from a 52 card deck (without replacement -> makes it hypergeometric)
# X = number of Aces you get

N <- 52
K <- 4 # aces
n <- 8 # sample size

# e.g. P(X=3)
dhyper(3, K, N-K, n) # success, failure, sample size


# make a table and barplot using dhyper
X.vals <- 0:8
pdf.vals <- dhyper(X.vals, K, N-K, n)

# table
names(pdf.vals) <- X.vals
cbind(pdf.vals)

# barplot
barplot(pdf.vals, names.arg = X.vals, space=0,
        xlab = "X = number of Aces", 
        ylab="P(X)",
        main="Number of Aces Drawn in 8 Cards")

# Here you will approximate the probability distribution for 𝑋 = the number of aces
# obtained when drawing 8 cards from a standard 52-card deck using relative
# frequencies obtained using simulation

#Q2.a Write a function that simulates drawing n.cards cards m.trials times, using
# the sample function and techniques from Lab 5. Your function should
# output a table giving the relative frequencies, as well as a bar plot. Run your
# function for n.dice=8, m.trials= 105 and record the table and bar plot.

AcesSimulated <- function(n.cards, m.trials){
  # create X values
  X.raw <- replicate(m.trials, 
                     sum(sample(c(rep("A", 4), 
                                  rep("D", 48)), 
                                8, 
                                replace=FALSE) == "A"))
  
  # make table with relative frequencies
  print(table(X.raw) / m.trials)
  
  # make barplot/histogram
  hist(X.raw, breaks=seq(-0.5, 4.5, by=1), 
       freq=FALSE,
       xlab="X = number of Aces",
       ylab="P(X)",
       main="Prob Hist of X (simulated)")
}

AcesSimulated(8, 10^5)


# Q2.b
# Write a function that simulates drawing n.cards cards m.trials times, using
# the rhyper function. As before, your function should output a table giving the
# relative frequencies, as well as a bar plot. Run your function for n.dice=8,
# m.trials =105 and provide a table and a bar plot.

AcesRhyper <- function(n.cards, m.trials){
  # create X values
  X.raw <- rhyper(m.trials, 4, 48, n.cards)
  
  # make table with relative frequencies
  print(table(X.raw) / m.trials)
  
  # make barplot/histogram
  hist(X.raw, breaks=seq(-0.5, 4.5, by=1), 
       freq=FALSE,
       xlab="X = number of Aces",
       ylab="P(X)",
       main="Prob Hist of X (rhyper)")
}

AcesRhyper(8, 10^5)


# Q.3 
# During one stage in the manufacture of integrated circuits, a coating must be
# applied. Suppose that in a batch of 999 chips, 333 did not receive a thick enough
# coating. Now 300 of the 999 chips are randomly selected for testing. Give the
# commands, along with your output, to compute the 
# following probabilities (answers are in brackets):

# Q3.a Exactly 100 do not receive a thick enough coating [0.05835]
dhyper(100, 333, 666, 300)

# Q3.b 100 or fewer do not receive a thick enough coating [0.5305]
phyper(100, 333, 666, 300)

# Q3.c Fewer than 100 do not receive a thick enough coating [0.4721]
phyper(99, 333, 666, 300)

# Q3.d At least 110 do not receive a thick enough coating [0.08254]
1 - phyper(109, 333, 666, 300)

# Q3.e Between 90 and 110 (inclusive) do not receive a thick enough coating [0.8759]
phyper(110, 333, 666, 300) - phyper(89, 333, 666, 300)


## Q.4 A student decides to purchase lottery tickets until she wins a prize. Suppose the
# probability of winning a prize is 1/3. Use dgeom to find the exact probability
# distribution for the number of tickets the student will buy before getting a winner.
# Exclude all probabilities less than 0.00005. (i.e., Stop calculating 𝑃(𝑥) once it
# becomes small enough that it gets rounded to 0.0000.) Record the probability
# distribution as both a table and a bar plot.
LottoProb <- function(p){
  x <- 0:20
  p.x <- dgeom(x, p)
  indices <- which(p.x > 0.00005)
  x_filtered <- x[indices]
  p.x_filtered <- p.x[indices]
  tickets <- x_filtered + 1 
  prob_table <- data.frame(tickets = tickets, probability = p.x_filtered)
  print(prob_table)
  barplot(p.x_filtered, names.arg = tickets, space=0,
          xlab = "X = Number of Tickets",
          ylab = "P(X)",
          main = "Number of Tickets to Win Lotto")
}

LottoProb(1/3)

## Q.5 Approximate the probability distribution for the number of lottery tickets 
# the student must buy before obtaining a winner in two ways:
# Q.5 a 
# By writing a function that uses the function sample to simulate r.reps
# repetitions of the experiment in which a student buys a lottery ticket every
# day until she wins. For each repetition, record the value 𝑋 = the number of
# tickets (including the final winning ticket) until the first win. Record a table
# showing the relative frequencies of 𝑋 and a bar plot showing the distribution
# of 𝑋. Run your function for r.reps = 105 and record the table and bar plot
# (with appropriate axis labels and title).

LottoSimulation <- function(p, r.reps){
  X.raw <- replicate(r.reps, {
    x <- 0
    win <- FALSE
    while (!win) {
      x <- x + 1
      win <- sample(c(TRUE, FALSE), 1, prob = c(p, 1 - p))
    }
    x
  })
  
  # table with relative frequencies
  freq_table <- table(X.raw) / r.reps
  print(freq_table)
  # barplot
  barplot(freq_table, xlab = "X = Number of Tickets",
          space=0,
          ylab = "Relative Frequency",
          main = "Probability Distribution of X (Simulated)")
}

LottoSimulation(1/3, 10^5)



# Q5.b
# By writing a function that simulates r.reps repetitions of the same
# experiment, but use rgeom to generate the simulated values of 𝑋. As before,
# your function should output a table giving the relative frequencies, as well as
# a bar plot. Run your function for r.reps = 105 and provide a table and a bar
# plot. NOTE: Ensure the first value of X is 1.
# The results of both of your simulations should be very similar to the results
# obtained with the dgeom function.

LottoRgeom <- function(p, r.reps){
  X.raw <- rgeom(r.reps, p) + 1
  
  # make table with relative frequencies
  print(table(X.raw) / r.reps)
  
  # make barplot/histogram
  hist(X.raw, breaks=seq(-0.5, max(X.raw) + 0.5, by=1), 
       freq=FALSE,
       xlab="X = number of tickets",
       ylab="P(X)",
       main="Prob Hist of X (rgeom)")
}
LottoRgeom(1/3, 10^5)

# The function pgeom computes cumulative probabilities. For example, we can compute the
# probability of having to purchase 7 or fewer tickets in order to get a winner:
#  > pgeom(7-1, 1/3)
# [1] 0.9414723

## Q6
# In an industrial “torture test”, a light switch is turned on then off 
# repeatedly until it fails. Each on-off attempt has a probability 0.001 of failure. 
# Record the R command used to calculate each of the probabilities:
# Q6a. That the switch will fail before it has been turned on and off 500 times [0.3936]
pgeom(499, prob = 0.001)
# Q6b. That the switch will not fail until it has been turned on and off at least 1200 times [0.3013]
1 - pgeom(1198, prob = 0.001)

# Q6c. That the switch will fail when it has been turned on and off between 1000 and 2000 times [0.2329]
pgeom(2000, prob = 0.001) - pgeom(998, prob = 0.001)

## Q7 A factory produces fibre optic cables that have an average of 0.75 flaws per meter.
# Use dpois to give an exact probability distribution for the number of flaws in one
# metre of cable as both a table and a bar plot.

# X = number of flaws in 1 meter
# lambda
lambda <- 0.75

# range of values
x_values <- 0:6

# dpois probability
probabilities <- dpois(x_values, lambda)

# table
probability_table <- data.frame(
  flaws = x_values,
  probability = probabilities
)

print(probability_table)

# barplot
barplot(probabilities,
        names.arg = x_values,
        space=0,
        xlab = "X = number of Flaws",
        ylab = "P(X)",
        main = "Probability Hist of X (Flaws per meter)")



## Q8 Use rpois to write a function that simulates selecting r.reps meter-long fibre optic
# cables and counting the number of flaws on each. For each cable, 𝑋 = the number
# of flaws. As before, your function should output a table giving the relative
# frequencies of 𝑋, as well as a bar plot. Run your function for r.reps = 105 and
# provide a table and a bar plot. Your results should be very similar to the ones you
# got in Question 7.

FlawsSimulation <- function(lambda, r.reps){
  X.raw <- rpois(r.reps, lambda)
  
  # table
  probability_table <- data.frame(
    probability = table(X.raw) / r.reps
  )
  print(probability_table) 

  # barplot
  hist(X.raw, breaks=seq(-0.5, max(X.raw) + 0.5, by=1), 
       freq=FALSE,
       xlab="X = number of flaws",
       ylab="P(X)",
       main="Prob Hist of X (rpois)")
}

FlawsSimulation(0.75, 10^5)

## Q9 
# A city records an average of 34 transformer failures per year. Give the commands,
# along with your output, to compute the following probabilities (answers are in brackets):

# a. Exactly 34 transformers fail in a year [0.06825]
dpois(34, 34)
# b. 30 or fewer transformers fail in a year [0.2804]
ppois(30, 34)
# c. Fewer than 30 transformers fail in a year [0.2235]
ppois(29, 34)
# d. More than 38 transformers fail in a year [0.2166]
1 - ppois(38, 34)
# e. At least 38 transformers fail in a year [ 0.2681]
1 - ppois(37, 34)
# f. Between 30 and 40 transformers (inclusive) fail in a year [0.6429]
ppois(40, 34) - ppois(29, 34)
# g. 34 or fewer transformers fail in each of two consecutive years [0.2975]
ppois(34, 34)^2
# h. 68 or fewer transformers fail over a two-year period [0.5322]
ppois(68, 34*2)






