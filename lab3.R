library(MASS)
library(dplyr)
library(mosaic)

data(quine)
mean(quine$Days)
mean(~Days, data=quine)
mean(Days~Eth, data=quine)

bwplot(~Days, data=quine)
bwplot(Days~Age, data=quine)

## Q.1 Skewness
SK <- 3 * (mean(quine$Days) - median(quine$Days)) / sd(quine$Days)
SK


histogram(~Days, data=quine, type="count", main=paste("Days of absence in school (n = ", nrow(quine), ")"), breaks = seq(0, max(quine$Days) + 5, 5))

## Q.3 Create a boxplot for the number of days of absence. Labels the 𝑋-axis appropriately
## and provide a title including 𝑛. How many outliers are there? Using the 25th and 75th
## percentiles as endpoints, state the range of “typical” numbers of days absent for
## students in this sample. (Write a sentence: “The typical student was absent between ...”)
boxplot(quine$Days, data=quine, main=paste("Days of absence in school (n = ", nrow(quine), ")"), xlab="Days of absence", horizontal = TRUE)
sum(quine$Days > quantile(quine$Days, 0.75) + IQR(quine$Days)* 1.5, quine$Days < quantile(quine$Days, 0.25) - IQR(quine$Days) * 1.5)


quantile(quine$Days, 0.25)
quantile(quine$Days, 0.75)
## range of typical days absent
range(quantile(quine$Days, 0.25), quantile(quine$Days, 0.75))

# Q.4 
## Calculate the “typical” number of days absent using both mean and median. Does
## this agree with your answer from Question 3?
mean(quine$Days)
median(quine$Days)

## Q.5
favstats(Days~Sex, data=quine)
## create side-by-side boxplots for number of Days of absence, grouped by Age
## also calculate favstats for Days grouped by Age
bwplot(Days~Age, data=quine)
favstats(Days~Age, data=quine)

## Q.6
## Use appropriate commands to determine which group of students, grouped by age and sex, is most consistent with regards to absences, and which is least consistent.
sd(Days~Age + Sex, data=quine)

## Q.7
## favstats function returned the values corresponding to the quartiles (25%, median, 75%), the mean, and the standard deviation.
## quantile function allows to generalize other percentages
quantile(~Days, data=quine)
quantile(quine$Days)
quantile(~Days, probs=0.9, data=quine)
quantile(~Days, probs=seq(0.1,0.9, 0.1), data=quine)

## Find the 20th, 40th, 60th, and 80th percentiles for days of absence, grouped by:
## - learner status
## - sex
## - ethnicity

quantile(Days ~ Lrn, data=quine, probs=c(0.2, 0.4, 0.6, 0.8))
quantile(Days ~ Sex, data=quine, probs=c(0.2, 0.4, 0.6, 0.8))
quantile(Days ~ Eth, data=quine, probs=c(0.2, 0.4, 0.6, 0.8))

## we can go in the other direction: find percentile rankings for each data value
percent_rank(quine$Days)
## this tells us that the first value in the Days column represents the 8.9655th percentile of the Days variable
options(digits=1)
percent_rank(quine$Days)
## this tells us that the first entry in the table is from a student who was absent
## more often than 9% of the students

## Q.8
## give a list of percentile rankings (two decimal palces) for the number of days absent
## for the male F0 students only
## give all the commands, as well as your output as a column
maleF0 <- quine %>%
percentile_ranks <- percent_rank(maleF0$Days)
options(digits=2)
percentile_ranks

## or you can condense the two lines into one
percent_rank(quine$Days[quine$Sex == "M" & quine$Age == "F0"])

# Q.9
## scale function provides z-scores of all data in a list
scale(quine$Days)
z.scores <- scale(quine$Days)
## at least 1 standard deviation from the mean
sum(abs(z.scores) >= 1)
sum(abs(z.scores) >= 2)
sum(abs(z.scores) >= 3)

hist(scale(quine$Days))

# Q. 10
## Do your results from question 9 satisfy Chebyshev’s theorem? 
## Do they satisfy the empirical rule?
## If not, explain why this is with reference to answers to earlier questions




