## lab 5
## David Cho (A01351217)
## Oct 3, 2024

## P(Full time job | BCIT ) = 0.001
## | means given


## 1. Assume that 5% of people have been sick with COVID-19 and therefore have
## COVID-19 antibodies. Write a function, hasAntibodies that simulates checking n
## people and returns the number of people with antibodies. 
## (Assume that people being checked are independent 
## and that each person has a 5% chance of having antibodies.)

hasAntibodies <- function(n){
  # Simulate n people, where each has a 5% chance of having antibodies
  result <- sample(c(1, 0), n, replace = TRUE, prob = c(0.05, 0.95))
  # Sum the number of people with antibodies (1 represents antibodies)
  return(sum(result))
}


hasAntibodies(100000)

## One particular serology test has a sensitivity of
## 90%, meaning that 90% of people who have been sick with COVID-19 actually test positive.
## (The sensitivity is also known rate of true positives.)

## In the terminology of conditional probability: the sensitivity is the 
## probability that a person tests positive, given that they have COVID-19 antibodies.

## 2. Write a function called truePositives that simulates testing n people who have
## COVID-19 antibodies and returns the number that test positive. Run your function
## for 𝑛 = 10^5 and give your output.

truePositives <- function(n){
  # Simulate testing n people with antibodies
  # Each has a 90% chance of testing positive (sensitivity)
  result <- sample(c(1, 0), n, replace = TRUE, prob = c(0.9, 0.1))
  # Sum the number of true positives
  return(sum(result))
}


truePositives(100000)

## The specificity of the test is 92%. This means that 92% of people who have not been sick
## with COVID-19 test negative (i.e., the test does not detect the presence of antibodies)

## 3. In a sentence, express the rate of false positives (the percentage of times
## a person without COVID-19 antibodies test positive) in terms of conditional
## probability. 
## 8% of people without COVID-19 antibodies test positive, 
## indicating a false positive rate of 8%.

## 4. Write a function called falsePositives that simulates testing n people who do not
## have COVID-19 antibodies and returns the number who test positive.
## Run your function for 𝑛 = 10^5 and record your output

alsePositives <- function(n){
  # Simulate testing n people without antibodies
  # Each has an 8% chance of testing positive (false positive rate)
  result <- sample(c(1, 0), n, replace = TRUE, prob = c(0.08, 0.92))
  # Sum the number of false positives
  return(sum(result))
}

falsePositives(100000)

## 5. What is the probability that a person has COVID-19
## antibodies, given that they test positive? 
## Suppose our test tells us that a person does not have COVID-19 antibodies.
## what do you think is the probability that this person
## actually does not have COVID-19 antibodies.

## answer in word document

## 6. In this question, you will create a simulation that uses the relative frequency
## approach to answer Question 5 (Supposing the test is negative, what is the
## probability that the person actually does not have COVID-19 antibodies?)
## To do this, write a function called ProbNegGivenTestsNeg that simulates testing n
## samples and returns the following proportion:
##  number of people who test negative and do not have COVID19 antibodies / number of people who test negative
## Note that the denominator will be a sum, because there are two categories of
## people who get a negative result: those who have the COVID-19 antibodies and
## those who do not have the antibodies.
## Run your function for 𝑛 = 10^5 and record your output. 
##(Hint: your output should be above 0.99) 
## [Using the functions you wrote in Question 1, 2, and 3, you can write  this function in just a few lines.]
## it should retun true negatives / total negatives

ProbNegGivenTestsNeg <- function(n){
  # Simulate the number of people with antibodies
  with.antibodies <- hasAntibodies(n)
  # Calculate the number without antibodies
  without.antibodies <- n - with.antibodies
  
  # Simulate true positives among those with antibodies
  true.positive <- truePositives(with.antibodies)
  # Calculate false negatives (people with antibodies who test negative)
  false.negative <- with.antibodies - true.positive
  
  # Simulate false positives among those without antibodies
  false.positive <- falsePositives(without.antibodies)
  # Calculate true negatives (people without antibodies who test negative)
  true.negative <- without.antibodies - false.positive
  
  # Total number of negative tests
  total.negatives <- false.negative + true.negative
  
  # Proportion of true negatives among all negative tests
  result <- true.negative / total.negatives
  return(result)
}


ProbNegGivenTestsNeg(100000)

## 7. Now write a function called ProbPosGivenTestsPos to get an answer to the
## question: supposing the test is positive, what is the probability that the person
## actually has COVID-19 antibodies?
## Run your function for 𝑛 = 10^5 and give your output.

ProbPosGivenTestsPos <- function(n){
  # Simulate the number of people with antibodies
  with.antibodies <- hasAntibodies(n)
  # Calculate the number without antibodies
  without.antibodies <- n - with.antibodies
  
  # Simulate true positives among those with antibodies
  true.positive <- truePositives(with.antibodies)
  # Calculate false negatives (not needed here but included for completeness)
  false.negative <- with.antibodies - true.positive
  
  # Simulate false positives among those without antibodies
  false.positive <- falsePositives(without.antibodies)
  # Calculate true negatives (not needed here but included for completeness)
  true.negative <- without.antibodies - false.positive
  
  # Total number of positive tests
  total.positives <- true.positive + false.positive
  
  # Proportion of true positives among all positive tests
  result <- true.positive / total.positives
  return(result)
}

ProbPosGivenTestsPos(100000)

## 8. How do your outputs for questions 6 and 7 compare to your guesses in question 5?
## If you worked at the serology lab that tests samples, would you consider a negative
## result to be fairly reliable? How about a positive result?

## My outputs from question 6 (99.44%) and question 7 (37.22%) were 
#relatively similar to my guess in question 5 (90%, and 40%, respectively).

# For negative test results - P(No Antibodies|Test Negative), the result is over 99% 
# shows that it is reliable since it means that if someone tests negative, 
# there is a high chance that they do not have COVID-10 antibodies.

# For positive test results - P(Has Antibodies∣Test Positive), the result is 
# around 37%, making it a bit less reliable than negative test results. 


## 9. Now let’s see what happens if the incidence of COVID-19 were different. Suppose
## that in a different region, 10% of people have been sick with COVID-19 and
## therefore have antibodies. Now what are your results for Questions 6 and 7? What if
## a 50% of people in a region have had COVID-19 and have antibodies? Records your
## results, clearly labeled.

hasAntibodies <- function(n){
  result <- sample(c(1, 0), n, replace = TRUE, prob = c(0.10, 0.90))
  return(sum(result))
}

truePositives <- function(n){
  result <- sample(c(1, 0), n, replace = TRUE, prob = c(0.9, 0.1))
  return(sum(result))
}

falsePositives <- function(n){
  result <- sample(c(1, 0), n, replace = TRUE, prob = c(0.08, 0.92))
  return(sum(result))
}

ProbNegGivenTestsNeg <- function(n){
  with.antibodies <- hasAntibodies(n)
  without.antibodies <- n - with.antibodies
  
  true.positive <- truePositives(with.antibodies)
  false.negative <- with.antibodies - true.positive 
  
  false.positive <- falsePositives(without.antibodies)
  true.negative <- without.antibodies - false.positive  
  
  total <- false.negative + true.negative
  
  
  result <- true.negative / total
  return(result)
}

ProbPosGivenTestsPos <- function(n){
  with.antibodies <- hasAntibodies(n)
  without.antibodies <- n - with.antibodies
  
  true.positive <- truePositives(with.antibodies)
  false.negative <- with.antibodies - true.positive 
  
  false.positive <- falsePositives(without.antibodies)
  true.negative <- without.antibodies - false.positive  
  
  total <- true.positive + false.positive
  
  
  result <- true.positive / total
  return(result)
}

ProbNegGivenTestsNeg(100000)
ProbPosGivenTestsPos(100000)

hasAntibodies <- function(n){
  result <- sample(c(1, 0), n, replace = TRUE, prob = c(0.5, 0.5))
  return(sum(result))
}

ProbNegGivenTestsNeg <- function(n){
  with.antibodies <- hasAntibodies(n)
  without.antibodies <- n - with.antibodies
  
  true.positive <- truePositives(with.antibodies)
  false.negative <- with.antibodies - true.positive 
  
  false.positive <- falsePositives(without.antibodies)
  true.negative <- without.antibodies - false.positive  
  
  total <- false.negative + true.negative
  
  
  result <- true.negative / total
  return(result)
}

ProbPosGivenTestsPos <- function(n){
  with.antibodies <- hasAntibodies(n)
  without.antibodies <- n - with.antibodies
  
  true.positive <- truePositives(with.antibodies)
  false.negative <- with.antibodies - true.positive 
  
  false.positive <- falsePositives(without.antibodies)
  true.negative <- without.antibodies - false.positive  
  
  total <- true.positive + false.positive
  
  
  result <- true.positive / total
  return(result)
}

ProbNegGivenTestsNeg(100000)
ProbPosGivenTestsPos(100000)


## 10. Assume again that 5% of people have been sick with COVID-19 and have the
## antibodies. Use Bayes’ Theorem to calculate the exact value of
## 𝑃( Has Antibodies | Pos Test )

## P(Pos Test) = P(Pos Test | Has Antibodies) x P(Has Antibodies) + P(Pos Test | No Antibodies) x P(No Antibodies) 
## P(Has Antibodies | Pos Test) = P(Pos Test | Has Antibodies) * P(Has Antibodies) / P(Pos Test)
## full answer in word document

