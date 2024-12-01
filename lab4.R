## MATH 3042 - lab 4
## Author: David Cho
## Date: 2020-09-25

library(mosaic)
library(dplyr)

## simulate yahtzee
sample(c (1,2,3,4,5,6), 5, replace=TRUE, prob=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
## sample() function is used to simulate a random experiment
## c(1,2,3,4,5,6) is the sample space of rolling a dice
## 5 is the number of trials
## replace=TRUE is used to simulate rolling a dice with replacement
## prob=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5) is the probability of rolling a 1, 2, 3, 4, 5, 6

## what is the probability of rolling at least one six?
m.trials <- 1000000
k.success <- 0

for (i in 1:m.trials) {
  if (6 %in% sample(c (1,2,3,4,5,6), 5, replace=TRUE, prob=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))){
    k.success <- k.success + 1
  }
}
k.success / m.trials
## if 6 is in the sample of 5 dice rolls, then k.success is incremented by 1

## simulate a single coin flip
sample(1:2, 1)
## what is 1:2? 1:2 is a vector of integers from 1 to 2
## 1. one line sample() function that runs 10 coin flips
sample(1:2, 10, replace=TRUE)
sample(c("Heads", "Tails"), 1)

FlipOnce = function(){
  result <- sample(c("Heads", "Tails"), 1)
  return(result)
}
FlipOnce()

FlipCoins <- function(n){
  coins <- integer(n)
  for (i.coin in 1:n){
    coins[i.coin] <- FlipOnce()
  }
  return(coins)
}

FlipCoins(10)

## probability of the event E= Heads based on m.trials experiments
ProbHeads <- function(m.trials){
  coins <- FlipCoins(m.trials)
  k.Heads <- sum(coins == "Heads")
  return (k.Heads / m.trials)
}
ProbHeads(100)

## FlipCoins more efficient by rewriting without for-lopp
sample(c("Heads", "Tails"), 10, replace=TRUE)

## 2. write a second definition of FlipCoins that does no use loops.
## This second version will get used in ProbHeads
FlipCoins <- function(n) {
  sample(c("Heads", "Tails"), size = n, replace = TRUE, prob = c(0.51, 0.49))
}

ProbHeads <- function(m.trials){
  coins <- FlipCoins(m.trials)
  k.Heads <- sum(coins == "Heads")
  return (k.Heads / m.trials)
}

ProbHeads(100000)

## Example: to simulate repeating 5 sets of 10 trials of flipping coin
replicate(5, sample(1:2, 10, replace=TRUE))

## 3. Write a function called MinAndMaxHeads that calls
## ProbHeads(m.trials) a total of r.reps times and returns
## the minimum and maximum values of the r.reps probabilities
## of obtaining heads over the r.reps reptitions
MinAndMaxHeads <- function(m.trials, r.reps){
  probs <- replicate(r.reps, ProbHeads(m.trials))
  ##  replicate() function is used to repeat the same experiment r.reps times
  return(c(min(probs), max(probs)))
}

## m.trials, r.reps
MinAndMaxHeads(10, 10)
MinAndMaxHeads(10, 100)
MinAndMaxHeads(10, 1000)
MinAndMaxHeads(10, 10000)

MinAndMaxHeads(100, 10)
MinAndMaxHeads(100, 100)
MinAndMaxHeads(100, 1000)
MinAndMaxHeads(100, 10000)

MinAndMaxHeads(1000, 10)
MinAndMaxHeads(1000, 100)
MinAndMaxHeads(1000, 1000)
MinAndMaxHeads(1000, 10000)

MinAndMaxHeads(10000, 10)
MinAndMaxHeads(10000, 100)
MinAndMaxHeads(10000, 1000)
MinAndMaxHeads(10000, 10000)

## as m.trials increases, the probability of getting heads approaches 0.5
## as r.reps increases, for small m.trails, there is extreme proportions
## however for large m.trails, the proportion of heads approaches 0.5

## Rolling a Fair dice for returning a table of results and frequency
## for example, in coin flip m= 100: Result Frequency Heads = 47 and Tails = 53
## we can do this by saving a list of reuslts of each trial then use table()
die.list <- c(3,6,3,1,2,6,4,2,1,5)
die.table <- table(die.list)
die.table
barplot(die.table)

## 4. Write a function RollDice(m.trials) that simulates the rolling a dice
## m.trials times and creates a bar plot of the distribution of the outcomes
RollDice <- function(m.trials){
  die.list <- sample(1:6, m.trials, replace=TRUE)
  die.table <- table(die.list)
  barplot(die.table, 
          main=paste("Outcome of", m.trials, "die rolls"), 
          ylim = c(0, max(die.table) * 1.1))
}
RollDice(100)
RollDice(1000)
RollDice(10000)

k.success <- 0
for (i in 1:1000) {
  if (5 == sum(sample(1:2, 10, replace=TRUE))){
    k.success <- k.success + 1
  }
}


## Experiment 3: Rolling Multiple (n > 1) Fair Dice
## 5 represents the number of dice not the number of trials
sample(1:6, 5, replace= TRUE)

## 5. Write a function called RollSomeDice that simulates m trials of the experiment
## in which you roll n dice at once. Let X be the number of dice showing 3.
RollSomeDice <- function(m.trials, n.dice){
  die.list <- sample(1:6, n.dice * m.trials, replace=TRUE)
  die.list <- matrix(die.list, nrow = m.trials)
  ## what does matrix() take in: matrix(data, nrow)
  x.vals <- apply(die.list, 1, function(x) sum(x == 3))
  ## apply() function is used here to iterate over the rows (MARGIN=1) of the matrix die.list
  ## sum(x == 3) is applied to each row, counting how many of the dice show the number 3 in that trial
  x.table <- table(x.vals)
  barplot(x.table, main=paste("Distribution of X for n=", n.dice, "dice (based on", m.trials, "trials)"), 
          ylim = c(0, max(x.table) * 1.1), xlab="X (# of 3s)", ylab="Frequency")
  return(x.table)
}
RollSomeDice(10000, 1)
RollSomeDice(10000, 2)
RollSomeDice(10000, 5)
RollSomeDice(10000, 10)
RollSomeDice(10000, 100)

## Experiment 4: Drawing Cards
## Modelling sample without replacement
## Getting 3 red cards if we draw 5 cards without replacement from a 52-card deck

## 6. Write a DrawCards(m.trials, n.cards, replace) function that simulates
## drawing n.cards from a 52 card deck. The argument replace indicates
## whether the cards are drawn with or without replacement. The experiment
## is performed a total of m.trials times
DrawCards <- function(m.trials, n.cards, replace){
  deck <- c(rep("Red", 26), rep("Black", 26))
  x.vals <- integer(m.trials)
  for (i in 1:m.trials) {
    cards_drawn <- sample(deck, size = n.cards, replace = replace)
    ## sample randomly selects elements from the deck vector
    x.vals[i] <- sum(cards_drawn == "Red")
  }
  x.levels <- 0:n.cards
  x.factor <- factor(x.vals, levels = x.levels)
  x.table <- table(x.factor)
  barplot(x.table,
          main = paste("Distribution of X (number of red cards out of", n.cards, "cards)", 
                       "where replacement=", replace),
          xlab = "Number of Red Cards (X)",
          ylab = "Frequency",
          ylim = c(0, max(x.table) * 1.1))
  
  return(x.table)
}
DrawCards(10000, 1, TRUE)
DrawCards(10000, 1, FALSE)
DrawCards(10000, 5, TRUE)
DrawCards(10000, 5, FALSE)
DrawCards(10000, 10, TRUE)
DrawCards(10000, 10, FALSE)
DrawCards(10000, 20, TRUE)
DrawCards(10000, 20, FALSE)
DrawCards(10000, 50, TRUE)  
DrawCards(10000, 50, FALSE)
