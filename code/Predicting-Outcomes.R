# Predicting Continuous Outcomes Using Linear Regression

##  PREDICTING GDP USING PRIOR GDP

## Set the working directory
## (delete the lines of code that are not for your computer, and
##  if using Windows, replace "username" with your own username)
setwd("~/Desktop/DSS") # if Mac
setwd("C:/Users/username/Desktop/DSS") # if Windows
setwd("/cloud/project/DSS") # if in the cloud
setwd("~/Dropbox/SIT/SIT_Classes/Public_Pages/Data-Storytelling-2026/data")

## Load the dataset
co <- read.csv("countries.csv") # reads and stores data

## Understand the data
## (Read about description of variables and unit of observation)
head(co) # shows first observations

## Identify the types of variables included
## (character vs. numeric; binary vs. non-binary)

## Identify the number of observations
dim(co) # provides dimensions of dataframe: rows, columns

### 4.4.1 RELATIONSHIP BETWEEN GDP AND PRIOR GDP

hist(co$gdp)
## Visualize the relationship
plot(x=co$prior_gdp, y=co$gdp) # creates scatter plot

## Compute correlation
cor(co$gdp, co$prior_gdp)

## Fit linear model
lm(co$gdp ~ co$prior_gdp) # option a: using $
lm(gdp ~ prior_gdp, data = co) # option b: using data argument


## Add fitted line to scatter plot
fit <- lm(gdp ~ prior_gdp, data = co) # saves fitted model
abline(fit) # adds line to scatter plot

### 4.4.2 WITH NATURAL LOGARITHM TRANSFORMATIONS

## Create log−transformed GDP variables
co$log_gdp <- log(co$gdp) # gdp
co$log_prior_gdp <- log(co$prior_gdp) # prior gdp

## Create histograms
hist(co$gdp) # gdp
hist(co$log_gdp) # log−transformed gdp
hist(co$prior_gdp) # prior gdp
hist(co$log_prior_gdp) # log−transformed prior gdp

## Create scatter plots
plot(x=co$prior_gdp, y=co$gdp) # before transformation
plot(x=co$log_prior_gdp, y=co$log_gdp) # after transformation

## Compute new correlation
cor(co$log_gdp, co$log_prior_gdp)

## Fit new linear model
lm(log_gdp ~ log_prior_gdp, data = co) 

## 4.5 PREDICTING GDP GROWTH USING CHANGE IN NIGHT-TIME LIGHT EMISSIONS

## Create GDP percent change variable
co$gdp_change <- ((co$gdp - co$prior_gdp)/co$prior_gdp) * 100

## Create light percent change variable
co$light_change <- ((co$light - co$prior_light )/co$prior_light ) * 100

## Create histograms
hist(co$gdp_change) # of change in gdp
hist(co$light_change) # of change in light

## Create scatter plot
plot(x=co$light_change, y=co$gdp_change)

## Compute correlation
cor(co$gdp_change, co$light_change) 

## Fit linear model
lm(gdp_change ~ light_change, data = co) 

## 4.6 MEASURING HOW WELL THE MODEL FITS THE DATA WITH R^2

## Compute R−squared for each predictive model
cor(co$gdp, co$prior_gdp)^2 # model 1
cor(co$log_gdp, co$log_prior_gdp)^2 # model 2
cor(co$gdp_change, co$light_change)^2 # model 3

## Do the same with Test-Training Split

train_index <- sample(1:nrow(co), size = 0.8*nrow(co)) # creates index for training set

train <- co[train_index, ] # creates training set
test <- co[-train_index, ] # creates test set

## Fit linear model on training set

model_train <- lm(gdp ~ prior_gdp, data = train) # model 1
model_train_log <- lm(log_gdp ~ log_prior_gdp, data = train) # model 2

## Make predictions on test set
pred_test <- predict(model_train, newdata = test) # model 1
pred_test_log <- predict(model_train_log, newdata = test) # model 2

## Compute R−squared for predictions
cor(test$gdp, pred_test)^2 # model 1
cor(test$log_gdp, pred_test_log)^2 # model 2

plot(x=test$gdp, y=pred_test) # model 1
abline(a=0, b=1) # adds 45 degree line to scatter plot

## Exercise 1: 

# Fit a linear model to predict gdp_change using light_change, and then:
# A. calculate R-squared for gdp_change
# B. Graph predicted gdp_change vs. actual gdp_change, and add a 45 degree line to the graph


# Predicting Binary Outcomes Using Logistic Regression

bes <- read.csv("BES.csv")

head(bes)

## Can we predict whether a respondent voted to leave with their age?

train_index_bes <- sample(1:nrow(bes), size = 0.8*nrow(bes)) # creates index for training set

plot(x=bes$age, y=bes$leave) # creates scatter plot

train_bes <- bes[train_index_bes, ] # creates training set
test_bes <- bes[-train_index_bes, ] # creates test set

model_bes <- glm(leave ~ age, data = train_bes, family = binomial) # fits logistic regression model

summary(model_bes) # shows summary of fitted model

pred_test_bes <- predict(model_bes, newdata = test_bes, type = "response") # makes predictions on test set

pred_test_bes_binary <- ifelse(pred_test_bes > 0.5, 1, 0) # converts probabilities to binary predictions

confusion_matrix <- table(test_bes$leave, pred_test_bes_binary) # creates confusion matrix

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) # calculates accuracy

print(accuracy) # prints accuracy

## Exercise 2:

# - Fit a logistic regression model using education to predict whether a respondent voted to leave
# - Calculate the accuracy of the model on the test set
