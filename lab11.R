## Lab11
## David Cho
## A01351217

# test the claim the mean age of a student at university if 19
# 1. claim mu = 19
# 2. hypothesis: H0: mu = 19, H1: mu not equal to 19 [two tailed)
# 3. test statistic t = Xbar - u / (s / sqrt(n)) = 3.2683
# 4. p-value: [1-pt(3.2683, df=236)] * 2
# 5. Decision: Reject H0
# 6. Conclusion: The mean age is not 19 (p-value = 0.0012)

library(MASS)
data(survey)
t.test(survey$Age)

# • t is the “test statistic” 𝑡 = 𝑋̅ −𝜇
#   𝑠/√𝑛 where 𝑋̅ , 𝑠, and 𝑛 are calculated from the sample
#   data, and 𝜇 is the “null hypothesis” value of 𝜇. 
#   (If you do not specify 𝜇, then the default value is 0.)
# • df is the “degrees of freedom,” equal to 𝑛 − 1, for the given sample
# • p-value is the area in the tail(s) beyond the given test statistic
# • alternative hypothesis is a text version of the 𝐻1 hypothesis 
#  (the null hypothesis is always 𝐻0: 𝜇 = mu, where mu is the value 
#    passed into the function as an argument)

t.test(survey$Age, mu=19)
# 𝐻0: 𝜇 = 19
# 𝐻1: 𝜇 ≠ 19
# 𝑡 = 𝑋̅ −𝜇
# 𝑠/√𝑛 = 3.2683 and the p-value (based on two-tails) is 0.001243. 
# We can therefore reject the null hypothesis at the 5% significance level
# Conclusion: The mean age of students at University of Adelaide is not 19 years (p-value=.0012).

# one-sided hypothesis testing using optional alternative argument
t.test(survey$Age, mu=19, alternative="greater")
# This output tells us that we are using the hypotheses: H0 = mu = 19
# The test statistic is again 𝑡 = 3.2683, but the p-value is now
# .0006215 (half as large as before) since we are only counting the
# right tail beyond the value 𝑡 = 3.2683. H1 : mu > 19
# Conclusion: There is sufficient evidence to conclude that the mean age of students at
# University of Adelaide is greater than 19 years (p-value=.00062)

# Q.1
# The function t.test can also be used to
# compare two groups. For instance, we
# could test the claim that male students are
# taller than female students, on average. (A side-by-side boxplot suggests this is true.)
# Read the help file for t.test and then give a
# sequence of commands to test the claim at
# the 5% significance level that male students
# are on average taller than female students
# at University of Adelaide. Write a sentence
# stating your conclusion
help(t.test)
# Perform a one-tailed t-test
t_test_result <- t.test(Height ~ Sex, 
                        data = survey, 
                        alternative = "greater", 
                        var.equal = TRUE)

# Print the result
print(t_test_result)
# Based on the t-test results at the 5% significance level, we conclude that male students are on average taller than female students at the University of Adelaide.

# Q.2
# Now write a function runHypothesis that takes arguments:
# • X.data - which is the raw data for a numerical variable 𝑋
# • alpha – the significance level to be used
# • var - a descriptor/name of the variable 𝑋
# • unit – the units applicable to the variable 𝑋
# • mu – the value of the population mean to be used in the null hypothesis
# • alternative – either “greater than”, “less than”, or “not equal to”
# Your function should test the claim given by your inputs and return a sentence
# conclusion. For example:
#   > runHypothesis(survey$Age, 0.01, "student age", "years", 20,
#                   "greater than")
# At alpha=0.01, we do not have sufficient evidence to claim that
# the mean student age is greater than 20 years.
# Use your function runHypothesis to test the claim at a 10% significance level that
# the mean student pulse rate is greater than 73 beats per minute. Record both your
# code and your output as answers to this question.

runHypothesis <- function(X.data, alpha, var, unit, mu, alternative) {
  # map the alternative hypothesis to the format used by t.test
  if (alternative == "greater than") {
    alt <- "greater"
  } else if (alternative == "less than") {
    alt <- "less"
  } else if (alternative == "not equal to") {
    alt <- "two.sided"
  } else {
    stop("Invalid alternative hypothesis. Choose from 'greater than', 'less than', or 'not equal to'.")
  }
  
  # remove missing values from the data
  X.data <- X.data[!is.na(X.data)]
  
  # perform the one-sample t-test
  test <- t.test(X.data, mu = mu, alternative = alt)
  
  # determine if we reject the null hypothesis
  if (test$p.value < alpha) {
    result <- "have sufficient evidence to claim that"
  } else {
    result <- "do not have sufficient evidence to claim that"
  }
  
  # format the significance level for display
  alpha_formatted <- format(alpha, nsmall = 2)
  
  # construct the conclusion sentence using paste
  conclusion <- paste("At alpha =", alpha_formatted,", we", result, "the mean", var, "is", alternative, mu, unit, ".")
  
  return(conclusion)
}

# use the function to test the claim about student pulse rate
runHypothesis(survey$Pulse, 0.10, "student pulse rate", "beats per minute", 73, "greater than")


# Use t.test to test the claim at 𝛼 = 0.05 that the span of students’ writing hands
# differs from the span of their non-writing hands. Include a sentence conclusion with your output.
# Here the two samples are not independent (since each person contributes one
#                                          numerical value to each sample). Read the t.test helpfile to determine how to
# handle data for such a sample. 

t.test(survey$Wr.Hnd, survey$NW.Hnd, paired = TRUE, alternative="two.sided")


###
t.test(survey$Age, mu=19, alternative="two.sided")
x.success <- sum(survey$Smoke != "Never", na.rm=TRUE)
n.sample <- sum(!is.na(survey$Smoke))
prop.test(x.success, n.sample, p=0.25, alternative="less")

# 4. Use prop.test to test the claim that the proportion of students at University of
# Adelaide who are left-handed is less than 10%. Include a full-sentence conclusion

x.success <- sum(survey$W.Hnd == "Left", na.rm=TRUE)
n.sample <- sum(!is.na(survey$W.Hnd))
prop.test(x.success, n.sample, p=0.10, alternative="less")

# Q. 5 
# Use prop.test to test the claim that students at University of Adelaide who never
# smoke are more likely to be frequent exercisers than students who smoke. Include
# a full-sentence conclusion.

x.success <- sum(survey$Smoke == "Never" & survey$Exer == "Freq", na.rm=TRUE)
n.sample <- sum(!is.na(survey$Smoke))
prop.test(x.success, n.sample, p=0.10, alternative="greater")

# Q. 6
# Use prop.test to test the claim that the proportion of left-handedness among male
# students is equal to the proportion of left-handedness among female students at
# the University of Adelaide.

x.male <- sum(survey$Sex == "Male" & survey$W.Hnd == "Left", na.rm = TRUE)
n.male <- sum(survey$Sex == "Male", na.rm = TRUE)

x.female <- sum(survey$Sex == "Female" & survey$W.Hnd == "Left", na.rm = TRUE)
n.female <- sum(survey$Sex == "Female", na.rm = TRUE)

prop.test(c(x.male, x.female), c(n.male, n.female), alternative = "two.sided")

dice_rolls <- read.csv("/Users/davidcho/Downloads/dice_rolls.txt", sep="")
View(dice_rolls)
dice_rolls
# let x = the number that turns up when you roll a 12 sided dice for n =50 times in
# dice_rolls
# if die is fiar, we expect mu = 6.5. test claim that mu = 6.5 at 5% significance level
# H0: mu = 6.5, H1: mu not equal to 6.5
Xbar <- mean(dice_rolls$X)
s <- sd(dice_rolls$X)
n <- 50
mu <- 6.5
t = (Xbar - mu) / (s / sqrt(n))

# p-value
p_value = 2 * (1 - pt(t, df = n - 1))

# Decision
if (p_value < 0.05) {
  decision = "Reject H0"
} else {
  decision = "Fail to reject H0"
}
t.test(dice_rolls$X, mu = 6.5)

# test claim that the proportion p of rolls x = 12 is greater than 1/6 (more than double what a fair die would do)
# H0: p = 1/6, H1: p > 1/6
# p-value
p = sum(dice_rolls$X == 12) / n
p_value = 1 - pbinom(sum(dice_rolls$X == 12) - 1, n, 1/6)
prop.test(sum(dice_rolls$X == 12), 50, p = 1/6, alternative = "greater")

