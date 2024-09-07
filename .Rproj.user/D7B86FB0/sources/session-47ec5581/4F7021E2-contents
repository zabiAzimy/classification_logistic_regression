#==============================================================================#
# R code template Classification exercise: Diabetes                            #
#==============================================================================#

# 00: packages -----------------------------------------------------------------
install.packages("pROC")
library(pROC)  # install package if necessary

# 01: load data ----------------------------------------------------------------
load("Diabetes.Rda")  # ??? stands for "path/Diabetes.Rda"

# 02: exploration data analysis ------------------------------------------------
# Answer parts (a) to (e) in the worksheet
# a: How many observations are there?
nrow(Diabetes)
dim(Diabetes)
# b: Obtain frequency table for diabetes status
table(Diabetes$YN)

# c: What is the mean and standard deviation of BMI and Age
summary(Diabetes)
mean(Diabetes$BMI)
sd(Diabetes$BMI)

mean(Diabetes$Age)
sd(Diabetes$Age)

# d(a): plot a histogram of BMI and Age
hist(Diabetes$BMI, col = "lightblue", main = "Histogram of BMI", xlab = "BMI")
hist(Diabetes$Age, col = "orange", main = "Histogram of Age", xlab = "Age")

# d(b): Scatter plot of Age and BMI
plot(Diabetes$Age, Diabetes$BMI, col = "darkgreen", xlab = "Age",
                                                    ylab = "BMI",
                          main = "Scatter plot of Age and BMI")

# e: Create a box plot of BMI against YN and BMI against YN
boxplot(data = Diabetes, BMI ~ YN, col = "orange")
boxplot(data = Diabetes, Age ~ YN, col = "maroon")

# 03: modeling -----------------------------------------------------------------

# 03a: Train/Test Split ----
# Split the data into a train/test with 2000 observations in the test data set
set.seed(50)
n <- dim(Diabetes)[1]
n
testidx <- sample(n, 2000)
test <- Diabetes[testidx, ]
train <- Diabetes[-testidx, ]

table(train$YN)

# 03b: model training ----
# Fit a logistic regression model and examine the model summary.
glm.obj <- glm(YN ~ BMI, data = train, family = "binomial")
summary(glm.obj)  #it looks as if higher BMI means diabetes is more likely  
BMI.grid <- 10:82
BMI.grid
glm.pred <- predict(glm.obj, 
                    newdata = data.frame(BMI = BMI.grid), 
                    type="response")
plot(BMI.grid, glm.pred, type = "o", xlab="BMI", lwd = 2, col = "blue")

# Question: does the logistic regression Diabetes = 'Yes' or Diabetes = 'No' as
#           dependent variable?  -- Yes they are our dependent variables

# 04: Assessment of Classification Quality -------------------------------------

# 04a: Classification matrix ----

# Define "High Risk of Diabetes" using a cut off of alpha=0.5 and construct
# the classification matrix
alpha <- 0.5 
fit1 <- fitted(glm.obj)
HiRisk <- fit1 > alpha
table(HiRisk)
tab <- table(train$YN, HiRisk, dnn = c("observed", "predicted"))
print(tab)

# The following 3 commands calculate the sensitivity, specificity and accuracy.
# Which is which?
(tab[1, 1] + tab[2, 2])/sum(tab) # This is the accuracy 
tab[1, 1]/sum(tab[1, ]) # This is the specificity
tab[2, 2]/sum(tab[2, ]) # This is sensitivity

# 04b: ROC curve and AUC ----
roc.obj1  <-  roc(train$YN, fit1)
plot(roc.obj1)   # R base graphics
ggroc(roc.obj1, lwd = 0.8, col = "darkgreen")  # ggplot graphics
auc(roc.obj1)


# roc produces a vector of thresholds (alpha), specificities and sensitivities.
# find the index of the threshold nearest to alpha=0.5
alpha <- 0.5 
idx <- which.min(abs(roc.obj1$thresholds-alpha))
idx

roc.obj1$thresholds[idx]
roc.obj1$sensitivities[idx]
roc.obj1$specificities[idx]

# do these values concur (fit) with your answers above?
# Answer : yes, they are the same as the calculations I did on paper, taking
# the threshold alpha value as 0.5

# 04c: out-of-sample goodness-of-fit -----
# get the predicted probabilities for the test data
ptest <-  predict(glm.obj, newdata = test, type = "response")

# confusion matrix for the test data
table(test$YN, ptest > alpha, dnn = c("observed", "predicted"))
test.roc.obj1  <-  roc(test$YN, ptest)

# ROC curve and AUC
ggroc(list(train=roc.obj1, test=test.roc.obj1), lwd = 0.8)
auc(test.roc.obj1)

# the training and test results are similar -- Yes, we get a very similar 
# AUC value for the training set as well.

# 05: Further modeling ---------------------------------------------------------

# 05a: Repeat the analysis done so far with variable 'Age'
glm.obj2 <- glm(YN ~ Age, data=Diabetes, family=binomial)
glm.obj2$coefficients  # it looks as if older means diabetes is more likely  
ptest2 <-  predict(glm.obj2, newdata=test, type="response")
test.roc.obj2  <-  roc(test$YN, ptest2)
ggroc(list("BMI"=test.roc.obj1, "Age"=test.roc.obj2), lwd=0.8)
auc(test.roc.obj2)

# 05b: repeat analysis with a model containing both BMI and Age 
glm.obj3 <- glm(YN ~ BMI + Age,data=Diabetes,family=binomial)
summary(glm.obj3)

ptest3 <- predict(glm.obj3, newdata = test, type = "response")
test.roc.obj3 <- roc(test$YN, ptest3)
ggroc(list("BMI"=test.roc.obj1,
           "Age"=test.roc.obj2, "BMI & Age"= test.roc.obj3), lwd=0.8)

# find the area under the curve for the third model
auc(test.roc.obj3)
alpha <- 0.19
# let's also create the confusion matrix
table(test$YN, ptest3 > alpha, dnn = c("observed", "predicted"))

fit3 <- fitted(glm.obj3)

# let's find the accuracy, sensitivity and specificity 
alpha <- 0.19
idx <- which.min(abs(test.roc.obj3$thresholds-alpha))
idx

test.roc.obj3$thresholds[idx]
test.roc.obj3$sensitivities[idx]
test.roc.obj3$specificities[idx]

# construct the classification matrix again using the new alpha 0.19
table(test$YN, ptest3 > alpha, dnn = c("observed", "predicted"))
# Add further comments and code ....
# Have fun!




