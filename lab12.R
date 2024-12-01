## Lab12
## David Cho
## A01351217

library(MASS)
library(dplyr)

data(survey)

plot(survey$Age, survey$Height, pch =20)
# r = linear correlation coefficient
r <- cor(survey$Age, survey$Height,  use="complete.obs")
# significance
cor.test(survey$Age, survey$Height)
# rho = pop.correlation
# is p-value is low, it must go -> reject null hypothesis
# if p-value is high, it must fly -> fail to reject null hypothesis
cor.test(survey$Wr.Hnd, survey$Height)
# highly confident there is blurry trend

## 1.Create a function CorrelationTest that reads in paired data X.data and Y.data,
# variable names/labels var1 and var2, and a significance level alpha. The function
# returns a sentence stating the appropriate conclusion about the population
# proportion between 𝑋 and 𝑌.
# Example:
#  > CorrelationTest(survey$Age, survey$Height, "student age", "student height", 0.05)
# At the 5% significance level, we have insufficient evidence of a linear correlation
# between student age and student height (P-value=0.5921).
# Demonstrate that your function can reproduce the example above

CorrelationTest <- function(X.data, Y.data, var1, var2, alpha) {
  # Create a data frame to align X.data and Y.data
  data <- data.frame(X = X.data, Y = Y.data)
  
  # Remove rows with missing values
  data <- na.omit(data)
  
  # Perform the correlation test
  test <- cor.test(data$X, data$Y)
  
  # Determine if we reject the null hypothesis
  if (test$p.value < alpha) {
    result <- "have sufficient evidence of a linear correlation"
  } else {
    result <- "have insufficient evidence of a linear correlation"
  }
  
  # Format the significance level for display
  alpha_formatted <- format(alpha, nsmall = 2)
  
  # Construct the conclusion sentence using paste
  conclusion <- paste("At the", alpha_formatted, "significance level, we", result, "between", var1, "and", var2, "(P-value =", test$p.value,").")
  
  return(conclusion)
}


# use the function to test the claim about student age and height
CorrelationTest(survey$Age, survey$Height, "student age", "student height", 0.05)

# Now let’s look at some data that is actually correlated. We would expect a strong
# correlation between the span of a student’s writing hand (Wr.Hnd) and the span of a
# student’s non-writing hand (NW.Hnd)
# 2. What is the correlation coefficient 𝑟 for the spans of student hands?
cor(survey$Wr.Hnd, survey$NW.Hnd, use="complete.obs")

# 3. Using your function CorrelationTest from Question 1, confirm that there is a linear
# correlation between the span of a student’s writing hand and the span of a
# student’s non-writing hand. Record the input and output.
CorrelationTest(survey$Wr.Hnd, survey$NW.Hnd, "writing hand span", "non-writing hand span", 0.05)

# 4. Create a scatterplot of Wr.Hnd (𝑦-axis) vs NW.Hnd (𝑥-axis). Give your scatterplot a
# meaningful title and meaningful axis labels.
plot(survey$NW.Hnd, survey$Wr.Hnd, pch = 20, main = "Span of Writing Hand vs. Span of Non-Writing Hand", xlab = "Span of Non-Writing Hand", ylab = "Span of Writing Hand")

# Now that we have established a linear correlation between the span of a student’s writing
# hand and the span of a student’s non-writing hand, let’s find a linear model 
# (i.e., regression line) and use it to make predictions. We use the function lm (“linear model”) to do this 
model <- lm(Wr.Hnd~NW.Hnd, data=survey)
model
# this gives regression line 𝑦̂ = 1.8361 + 0.9058𝑋, where X denotes the student's non-writing hand

abline(model$coefficients, col="red",lty="dashed",lwd=2)

# 5. Use regression line equation to find the best point estimate for the span of the
# writing hand for a student whose non-writing hand has a span of 20.0 cm.

y <- 1.8361 + 0.9058*(20)
y

# We can use predict to get a prediction interval rather than a point estimate. The variable
# model should already contain the linear model from earlier. We can apply this to make a
# series of predictions for the values
# NW.Hnd = 20.0
# NW.Hnd = 21.0
# NW.Hnd = 22.0
# NW.Hnd = 23.0

predictor.vals <- data.frame(NW.Hnd=c(20.0, 21.0, 22.0, 23.0))
predict(model, predictor.vals)
predict(model, predictor.vals, interval="predict")

# 6. Find the correlation coefficient 𝑟 for a student’s height and the span of their writing
# hand. How does this number compare to the answer you got for Question 2 
# (larger, smaller, around the same)? Is this what you would expect? Explain.
cor(survey$Height, survey$Wr.Hnd, use="complete.obs")
# question 2: 0.9483101 and question 6: 0.6009909
# the correlation coefficient for a student's height and the span of their writing hand is smaller than the correlation coefficient for the spans of student hands. This is expected because the span of a student's writing hand is more directly related to the span of their non-writing hand, while height is a more general measure that may not be as closely related to hand span.

# 7. Using the function CorrelationTest you wrote in Question 1, determine whether
# there is a linear correlation between the height of a student and the span of a
# student’s writing hand at the population level. Provide your output.
CorrelationTest(survey$Height, survey$Wr.Hnd, "student height", "writing hand span", 0.05)

# 8. Create a scatterplot of Wr.Hnd (𝑦-axis) vs Height (𝑥-axis). Give your scatterplot a
# meaningful title and meaningful axis labels. Plot the line of best fit directly on the
# graph.
plot(survey$Height, survey$Wr.Hnd, pch = 20, main = "Height vs. Writing Hand Span", xlab = "Height", ylab = "Writing Hand Span")
model2 <- lm(Wr.Hnd~Height, data=survey)
abline(model2$coefficients, col="red",lty="dashed",lwd=2)

# 9. At 95% confidence, predict the span of the writing hand for a student who is 173 cm
# tall. Include a sentence stating your conclusion.
predict(model2, data.frame(Height=173), interval="predict")
# At 95% confidence, we predict that the span of the writing hand for a student who is 173 cm tall is between 17.68 and 19.00 cm.

# 10. Is the prediction interval for Wr.Hnd narrow or wider when using Height as the
# predictor variable compared to when using NW.Hnd as the predictdor variable?
# Explain why, making reference to other values you have computed in this lab.
# The prediction interval for Wr.Hnd is wider when using Height as the predictor variable compared to when using NW.Hnd as the predictor variable. This is because the correlation coefficient between Height and Wr.Hnd is smaller than the correlation coefficient between NW.Hnd and Wr.Hnd. A smaller correlation coefficient indicates a weaker relationship between the predictor and response variables, leading to a wider prediction interval. In contrast, a larger correlation coefficient indicates a stronger relationship and a narrower prediction interval.

# 11. Calculate the 95% prediction interval for Wr.Hnd with Height = 173 cm using the
# formula
# 𝑦̂ − 𝐸 < 𝑌 < 𝑦̂ + 𝐸
# and
#𝐸 = 𝑡𝛼 2⁄ ⋅ 𝑆𝑒√1 + 1
#𝑛 + (𝑥0 − 𝑋̅ )2
# (𝑛 − 1)𝑠𝑋
#𝑆𝑒 = √(∑ 𝑦2) − 𝑎(∑ 𝑦) − 𝑏(∑ 𝑥𝑦)
#𝑛 − 2

data <- na.omit(data.frame(Height = survey$Height, Wr.Hnd = survey$Wr.Hnd))

model <- lm(Wr.Hnd ~ Height, data = data)
a <- coef(model)[1]
b <- coef(model)[2]

X0 <- 173
Y_hat <- a + b * X0
n <- nrow(data)
X <- data$Height
Y <- data$Wr.Hnd
X_mean <- mean(X)
Sx <- sd(X)
t_alpha2 <- qt(1 - 0.025, df = n - 2)
Se <- sqrt((sum(Y^2) - a * sum(Y) - b * sum(X * Y)) / (n - 2))
E <- t_alpha2 * Se * sqrt(1 + (1 / n) + ((X0 - X_mean)^2 / ((n - 1) * Sx^2)))

# Prediction interval
c(Y_hat - E, Y_hat + E)
