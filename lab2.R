library(mosaic)
library(MASS)
library(aplpack)

pie(table(survey$W.Hnd), labels = c("Left-Handed", "Right-Handed"))

## percent in order
percents <- c(10,30,5,35,20)

## vector with labels
homeType <- c("on campus", "with parents", "alone", "with
roommates", "with spouse")

pieColors <- c("red", "yellow", "green", "cyan", "blue")

## pie chart
pie(percents, homeType, col=pieColors, main = "Living Arrangements for college students")

## survey
library(MASS)
View(survey)

## to view first few rows of data from
head(survey)

## creates vector hands
hands <- survey$W.Hnd

levels(survey$W.Hnd)
tab <- table(hands)
print(tab)
pie(table(hands))

## add numbers of students of each type to label
lbls <- paste(rownames(tab), sep="\n", tab)
lbls
pie(tab, labels = lbls)
survey.colors <- c("pink", "turquoise")
pie(tab, labels = lbls, col = survey.colors, main="Writing hands of 237 students")

## bar graphs in R
barplot(tab)
barplot(tab, horiz=TRUE, main="Writing hands of 237 students")

## stemplot
## only extract height column
survey$Height
head(survey$Height)
## use cbind to get data as a column
cbind(survey$Height)
stem(survey$Height)
## to move decimal places
stem(survey$Height, scale = 2)

## back-to-back stem plots
## command stem.leaf.backback allows to plot male and female heights back to back
## get MensHeight and Womens height
MensHeights <- c(filter(survey, Sex=="Male")$Height)
WomensHeights <- c(filter(survey, Sex=="Female")$Height)

stem.leaf.backback(MensHeights, WomensHeights, m=10,
                   depths=FALSE)
## change amount of stem
stem.leaf.backback(MensHeights, WomensHeights, m=2,
                   depths=FALSE)

## frequency table range
range(survey$Height) ## NA output
numerical.Heights <- survey$Height[ !is.na(survey$Height) ]
range(numerical.Heights)
## we go up to 205 since [195, 200) which excludes any student who is 200 cm tall.
breaks <- seq(150, 205, by=5)
## sort into intervals cut() function is used to divide continuous data into intervals or bins
Heights.classes = cut(numerical.Heights, breaks, right=FALSE)
## generate frequency table
Height.freqs = table(Heights.classes)
## cbind() function in R combines vectors, matrices, or data frames by column
cbind(Height.freqs)

## create frequency table for student ages
MaleAges <- survey[survey$Sex == "Male", ]$Age
MaleAges <- filter(survey, Sex == "Male")$Age
FemaleAges <- survey[survey$Sex == "Female", ]$Age
MaleAges <- MaleAges[!is.na(MaleAges)]
FemaleAges <- FemaleAges[!is.na(FemaleAges)]
ageBreaks <- seq(15, 75, by=5)
MaleAge.classes = cut(MaleAges, ageBreaks, right=FALSE)
FemaleAge.classes = cut(FemaleAges, ageBreaks, right=FALSE)
MaleAge.freqs = table(MaleAge.classes)
FemaleAge.freqs = table(FemaleAge.classes)
cbind(MaleAge.freqs)
cbind(FemaleAge.freqs)

## histograms in r
## we can display using hist(numerical.Heights)
## to display heights of students
hist(numerical.Heights)
hist(numerical.Heights, main="Heights of 237 students", xlab="Height (cm)", ylab="Frequency")
hist(numerical.Heights, breaks=seq(floor(min(numerical.Heights)), ceiling(max(numerical.Heights)), 2.5), main="Heights of 237 students", xlab="Heights (cm)", col="white", border="black")

## Cumulative frequency
cumul.freq <- cumsum(Height.freqs)
cbind(cumul.freq)

## Ogives in R
## plot the cumulative frequency
cumul.freq <- c(0, cumul.freq)
plot(breaks, cumul.freq, type="p", pch=19)
plot(breaks, cumul.freq, type="b", pch=1, lty=1, xlab="student heights in cm", ylab="number of students", main="Ogive of 209 student heights")

## scatterplots in R
## help visualize the realtionship between two numerical variables
## example: see how height and weight are correlated
xyplot(survey$Wr.Hnd ~ survey$Height)
xyplot(survey$Wr.Hnd ~ survey$Height, xlab = "Height (cm)", ylab = "Writing hand span (cm)", main="Scatterplot of Writing hand span vs Height")

## scatterplot for Wr.Hand vs NW.hand
xyplot(survey$Wr.Hnd ~ survey$NW.Hnd, xlab = "Non-writing hand span (cm)", ylab = "Writing hand span (cm)", main="Scatterplot of Writing hand span vs Non-writing hand span")

## scatterplot for Height vs Pulse
xyplot(survey$Height ~ survey$Pulse, xlab = "Pulse rate (beats per minute)", ylab = "Height (cm)", main="Scatterplot of Height vs Pulse rate")

