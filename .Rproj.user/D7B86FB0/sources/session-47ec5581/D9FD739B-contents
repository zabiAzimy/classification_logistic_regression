# Classification on Smarket dataset

# as always load the required libraries
library(ISLR2)
install.packages("corrplot")
library(corrplot)

# load the datasets of Smarket
data("Smarket")

# some initial data exploration
names(Smarket)
head(Smarket)
dim(Smarket)
summary(Smarket)

# correlation plots
corrplot(cor(Smarket[, -9]))

# the function cor() gives the correlation matrix
# trying for the first time we get error, because col 9 is not numeric
cor(Smarket)

# that is why we exclude the 9th column
cor(Smarket[, -9])

# attach the Smarket dataset
attach(Smarket)
plot(Volume, pch = 16, col = "darkgreen")


# we now try to fit a logistic regression to the Smarket data
# we will use glm() which is called generalized linear model
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume ,
               data = Smarket , family = binomial)

# take a look at the summary of the model
summary(glm.fit)

# we can access only the coefficients for the fitted model using coef()
coef(glm.fit)

# we can also take only the coefficients from summary funcition output
summary(glm.fit)$coef

glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]

# how is the dummy variable is created for our classes
contrasts(Direction)

glm.pred <- rep("Down", 1250)
glm.pred

glm.pred[glm.probs > .5] = "Up"
glm.pred


# let's now produce the confusion matrix
table(glm.pred, Direction)

# compute the fraction of days which the prediction was correct
mean(glm.pred == Direction)

# this is the percentage of time that predection was correct
(sum(glm.pred == Direction) / 1250) * 100

# one main part is missing here, we should split our data into train and test
# and using the test test we should assess our model
train <- (Year < 2005)
train

Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)

# we have prepared the train set above and now fit it to the model
# using the subset arg we specify the data for the model to be trained on
glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket, family = binomial, subset = train 
)

glm.probs <- predict(object = glm.fits, Smarket.2005, type = "response")
length(glm.probs)

# As we can see above, we have used different sets for trainng and the test
# The training was done for the observations before 2005
# the test set is using the data that belong to year 2005


# let's now assess the model
glm.pred <- rep("Down", 252)

glm.pred[glm.probs > 0.5] <- "Up"

Direction.2005 <- Direction[!train]
length(Direction.2005)
length(glm.pred)
# construct the confusion matrix
table(glm.pred, Direction.2005)

# Finding the correctly predictions
(sum(glm.pred == Direction.2005) / 252) * 100

# Compute the incorrect predictions
mean(glm.pred != Direction.2005) # this gives 0.52 which is our test error
# in terms of percentage that is going to be 52%
# the result is very disappointing -- It is worse than random guessing!


# recalling the p-values from the previous steps we remember that they were
# not very small 
# looking at the p-values we decide to take lag1 and lag2 
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket,
                family = binomial, subset = train)

glm.probs <- predict(object = glm.fits, Smarket.2005, type = "response")

# we do the same procedure in order to construct the confusion matrix
glm.pred <- rep("Down", 252)

glm.pred[glm.probs > 0.5] <- "Up"

table(glm.pred , Direction.2005)

# The predictions are now 56% of times correctly done
mean(glm.pred == Direction.2005)

























