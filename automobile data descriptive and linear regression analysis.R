
# the hypothesis we have is that car mileage differs between automatic and manual transmission cars
# Also factors like car weight and engine horsepower might influence the mielage variable


# get an idea on the observations in the dataset

head(mtcars)

### Description of variables###
# mpg 	Miles/(US) gallon
# cyl 	Number of cylinders
# disp 	Displacement (cu.in.)
# hp 	Gross horsepower
# drat 	Rear axle ratio
# wt 	Weight (lb/1000)
# qsec 	1/4 mile time
# vs 	V/S
# am 	Transmission (0 = automatic, 1 = manual)
# gear 	Number of forward gears
# carb 	Number of carburetors


# Step 0: 
# assign data set to a dataframe name cars

cars <- mtcars
str(cars)


# Step 1: Descriptive analysis of variables
# Further create a box plot and a histogram to understand the spread of data for weight, mileage variables
par(mfrow=c(2,2)) # this particular command allows arranging graphs you create into rows and columns



hist(cars$hp)  # histogram of horsepower
hist(cars$wt)  # histogram of weights of cars
hist(cars$mpg) # histogram of mileage data
hist(cars$cyl) # histogram of number of engine cylinders

boxplot(cars$mpg, main = "spread of mileage") # boxplot of mileage data

# here our task is to identify the skew of the distribution and infer any interesting insights and make notes


# Step 2: Trying to understand the relationship between variables
# create a scatter plot to understand the relationship between weight and mileage
# create a scatter plot to understand the relationship between weight and horsepower
# create a scatter plot to understand the relationship between horsepower and mileage 
# create a scatter plot to understand the relationship between number of cyalinders and engine displacment 

plot(cars$wt, cars$mpg, xlab = "weight - independent variable", ylab = "mileage - dependent variable", main = "weight vs mileage per gallon")
plot(cars$wt, cars$hp, xlab = "weight - independent variable", ylab = "horsepower - dependent variable", main = "weight vs horsepower")
plot(cars$hp, cars$mpg, xlab = "horsepower - independent variable", ylab = "mileage - dependent variable", main = "horsepower vs mileage")
plot(cars$cyl, cars$disp, xlab = "cylinders - independent variable", ylab = "engine displacment - dependent variable", main = "nr. cylinders vs engine displacment")

# if you want to quantify the linear relations
# The correlation coefficient of two variables in a data set 
# equals to their covariance divided by the product of their
# individual standard deviations. It is a normalized measurement 
# of how the two are linearly related

# correlation of +1 indicates a strong positive linear relationship
# corrleation of 0 indicates no linear relationship
# corröleation of -1 indicates a strong negative relationship

cor(cars$wt, cars$mpg) # indicates a negative linear relationship
cor(cars$hp, cars$disp) # indicates a positive linear relationsip
cor(cars$wt, cars$drat)
cor(cars$drat, cars$mpg)

# Applying linear regression model to predict car mileage
# identify variables we could we use for Linear regression analysis

# lets convert nominal numerical data into factors

cars$am <- as.factor(cars$am)
levels(cars$am) <- c("Auto-Transmission", "Manual-Transmission")

boxplot(cars$mpg~cars$am, ylab = "Miles per Gallon")

# lets subset these levels of transmission into different variables
Automatic <- cars[cars$am == "Auto-Transmission",] 
Manual <- cars[cars$am == "Manual-Transmission",]


# identify which category of cars i.e. auto or manual transmission have low median mileages

par(mfrow=c(1,1))
boxplot(cars$mpg~cars$am, main="transmission type vs mileage")
boxplot(cars$hp~cars$am, main = "transmission type vs horsepower")


#y = mx + c
# developing a model with single independent and a dependent variable

#The model above is achieved by using the lm() function in R and 
#the output is called using the summary() function on the model.

lm_single <- lm(mpg~wt, data=cars)
summary(lm_single) # print linear model output
# Derive linear model interpreting the sunmmary results

# Y = a + m1*X1 + m2*X2 + ... + mp*Xp
# developing a model with multiple independent and a dependent variable
# identify values for intercept 
# identify weight coefficient estimate
# ping in the values in y=mx+c
# here X is the known independent value like the weight of the car
# Y is unknown dependent value
# 'C' is the intercept value
# make sure your unknown predicted value is within the weight range of the cars in the dataset


# Developing a multiple linear regression model

lm_multiple <- lm(mpg~wt+hp+cyl+disp, data=cars)
summary(lm_multiple)

# scenario 2 including automatic and manual transmission classes
lm_multiple_2 <- lm(mpg~wt+hp+cyl+disp+am, data=cars)
summary(lm_multiple_2)

# interpreting the results and develop the regression model by hand




