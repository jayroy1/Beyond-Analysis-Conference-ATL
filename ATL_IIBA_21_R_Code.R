## R code and documentation for Predictive Analytic

## Data set from Kaggle.com - it is downloadable as a csv file.

## Original Example comes from book Machine Learning with R. Author: Brett Lantz and "tweaked" by Jay Roy.


## Example: Using Linear Regression to Predicting Medical Expenses ----

## Step 1: Import necessary libraries prior to data profiling exploring and preparing the data ----

## You can obtain the libraries from https://cran.r-project.org/
library(psych)

dev.off()
c(1, 1, 1, 1)
par(mar = c(.5, .5, .5, .5))

## library(stats) already included in base R and used to build your models.

## Step 2: Data Profiling: Exploring data to obtain a better understanding of it.
## This part of the Analytics process is called "Descriptive Statistics" ----

## Read the csv file from kaggle.com into R.

insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)

## Determine where your data sits?
getwd()

str(insurance)
dim(insurance)

# summary the medical expenses (charges) variable
summary(insurance$expenses)

# histogram of insurance charges - right tailed (skew)
hist(insurance$expenses)

# table of region
table(insurance$region)

# exploring relationships among features: correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])

# visualizing relationships among features: scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "expenses")])
pairs(insurance[c("age", "expenses")])


## from the plot comparing age to expenses, we see that as age increases, so does expenses.

# more informative scatterplot matrix
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])
pairs.panels(insurance[c("age", "expenses")])

## highest correlation between the pairs of predictors is age and expenses.

## Once we have a feel for our data, we can begin to identify potential variables of a model to determine 
## what the drivers of medical claims could be.

## Using linear regression - Y= Bo + B1*X1 +B2*X2+e (X=variables, B=Betas/Coefficients, e=error)

## lm() function

## Step 3: Build your model and train it on your historical  data ----
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
                data = insurance)
## ins_model <- lm(expenses ~ ., data = insurance) # this is equivalent to above

# see the estimated beta coefficients
ins_model

## Step 4: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(ins_model)

## Step 5: Improving the linear regression model performance "Model Optimization" ----

# add a higher-order "age" term
insurance$age2 <- insurance$age^2

# add an indicator for BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# create final model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)

summary(ins_model2)

# making predictions with the regression model on the original data.
insurance$pred <- predict(ins_model2, insurance)

insurance$diff <- round(insurance$expenses - insurance$pred,2)
max(insurance$diff)
min(insurance$diff)
hist(insurance$diff)

cor(insurance$pred, insurance$expenses)
## strong correlation between actual expenses and predicted expenses based on model.


plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)## a=intercept and b=slope equal to 1

## this scatter plot shows us that each point is a patient expense
## plot shows us that there is a strong linear relationship - meaning the model is accurate.
## notice the scatter points above the line and below line.
## Those above the line are actual expenses greater than expected and those below less than expected(predicted)


## Now you can utilize the model to forecast the costs of potential new enrollees

## Using the prediction model from above with unique individual attributes, we can predict the medical costs in the near future.  

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1,
                   smoker = "no", region = "northeast"))

## $ 5973.77 is predicted cost for this male


predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))

## for female, the cost is $6,470.54

## This difference between the two is $496.77, on average males would have $497 less expenses for the plan which can be seen 
## in the difference in the regression coefficient, sexmale. 


predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 0,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))
## Similarly a female with no children is projected to have less medical costs setting the children to 0 = $678*2=$1,357.20

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1,
                   smoker = "yes", region = "northeast"))


predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1,
                   smoker = "no", region = "northeast"))
