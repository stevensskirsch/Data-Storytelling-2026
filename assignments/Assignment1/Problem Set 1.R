setwd("C:/Users/skirs/Documents/GitHub/Data-Storytelling-2026/assignments/Assignment1")


## 1. Load the data into R and output the first 10 rows
bes <- read.csv("ps1_BES.csv")
head(bes, 10)

## 2. How many rows and columns does the dataset have
dim(bes)
# 30,895 rows and 4 columns

## 3. Print the names of the columns
names(bes)

## 4. What types of data are in each column
sapply(bes, class)
#vote, leave, and education contains characters while age contains integers


## 5. Explore the data. Are there any missing or incorrectly coded values? How many and which columns?
unique(bes$vote)
unique(bes$leave)
#Missing
unique(bes$education)
#Missing
sort(unique(bes$age))
#-9

sum(bes$leave == "Missing")
# 2852 unrecorded under 'leave'
sum(bes$education == "Missing")
# 3425 unrecorded under 'education'
sum(bes$age == -9)
# 5 unrecorded under 'age'

bes$leave[bes$leave == "Missing"] <- NA
bes$education[bes$education == "Missing"] <- NA
bes$age[bes$age == -9] <- NA

## 6. Examine the dataset's variable types. Convert variables to appropriate format.
bes$vote <- as.factor(bes$vote)
bes$leave <- as.numeric(bes$leave)
bes$education <- as.factor(bes$education)
bes$age <- as.numeric(bes$age)
sapply(bes,class)



## 7. Calculate summary statistics for all numeric columns
mean(bes$leave, na.rm = TRUE)
median(bes$leave, na.rm = TRUE)
sd(bes$leave, na.rm = TRUE)
# LEAVE: mean=0.48821; median=0; sd=0.49987

mean(bes$age, na.rm = TRUE)
median(bes$age, na.rm = TRUE)
sd(bes$age, na.rm = TRUE)
# AGE: mean=50.75413; median=53; sd=16.59786

## 8. Boxplot for relationship between leave and age
boxplot(age ~ leave, data = bes)

## 9. Correlation matrix to assess the relationships between age and leave
cor(bes$age, bes$leave, use = "complete.obs")
# 0.24003, this shows the variables are positively correlated, and that the relationship is weak

## 10. Load data from other file
ua <- read.csv("ps1_UA_survey.csv")
head(ua)

## a. Difference in means of pro_russian_vote between those who did and didn't receive Russian TV broadcasts
tv_mean <- mean(ua$pro_russian_vote[ua$russian_tv == 1], na.rm = TRUE)
no_mean <- mean(ua$pro_russian_vote[ua$russian_tv == 0], na.rm = TRUE)

tv_mean - no_mean
# The difference is 0.11911, and it suggests that respondents who receive Russian TV broadcasts have a slightly higher probability (on average) of voting for a Pro-Russian candidate than those who do not get the Russian broadcasts
# Given this information we can't interpret this relationship as causal, because in this dataset people weren't randomly assigned to receive Russian TV