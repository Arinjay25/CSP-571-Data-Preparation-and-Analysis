# CSP571
# Homework 4


#Load in the auto mpg data set: https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data
# Note with this data set, you are trying to predict "mpg"
dataset<-NULL
dataset<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",header=FALSE,na.strings = c('NA','?'), stringsAsFactors = TRUE)
colnames(dataset)<-c('MPG','CYLINDERS','DISPLACEMENT','HORSEPOWER','WEIGHT','ACCELERATION','MODELYEAR','ORIGIN','CARNAME')
#head(dataset)

# 1. Identify the columns (if any) with ANY data that is missing our
# could be reasonably construed as missing. Replace with median or mode, where
# appropriate
# 4 points

med <- median(dataset$HORSEPOWER, na.rm = TRUE)
#print(med)

impute.med <- function(x) {
  z <- median(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
}

dataset$HORSEPOWER <- impute.med(dataset$HORSEPOWER)
#dataset<- mean(data$HORSEPOWER, na.rm = TRUE)

# 2. Identify all of the categorical variables, 
# all of the numeric variables
# Store it in the variables below.
# 2 points


dataset$ORIGIN <- as.factor(dataset$ORIGIN)
dataset$CYLINDERS <- as.factor(dataset$CYLINDERS)
dataset$MODELYEAR <- as.factor(dataset$MODELYEAR)

numVars1 <- colnames(dataset)[sapply(dataset, is.numeric)]
catVars1 <- colnames(dataset)[sapply(dataset, is.factor)]

# 3. Identify the appropriate descriptive statistics and graph for this data set.
# Execute on those and use the comments to discuss relevant relationships or insights discovered.
# 2 points


# For Numeric Variables -
# 1) Histograms to check the normality.
# 2) Boxplots to present the mean and the quartiles.
# 3) scatter plots of mpg vs other variables.
# 4) scatter plot matrix to check correlation between the variables

for(columnName in numVars1){
  title <- paste("Histogram for", columnName, sep = " ")
  hist(dataset[[columnName]], main = title, xlab = columnName)
  
  title <- paste("Boxplot for", columnName, sep = " ")
  boxplot(dataset[[columnName]], main = title, xlab = columnName)
  
  title <- paste("Scatter plot for MPG vs.",columnName, sep = " ")
  plot(y=dataset$MPG, x=dataset[[columnName]], ylab = "MPG", xlab = columnName, main = title)
}

# From the histograms - 
# only accelaration seems to be almost normally distributed, others are positively skewed.
# From the box plots - 
# almost 50% of cars have mpg below 22.5 and there is one outlier. there are few cars with horsepower
# greater than 200 are treated as outliers. The median weight for the cars is around 2750.
# the accelration data is evenly distributed and the mean accelaration is around 16 mph^2.
# From the scatterplots -
# displacement, horsepower and weight are negatively related to the mpg and seems slightly non-linear.
# acceleration on the other hand seems to be positively correlated.


plot(dataset[which(colnames(dataset) %in% numVars)])
# From Scatterplot matrix -
# displacement, weight and horsepower are strongly positively correlated variables but all these are
# fairly strongly negatively correlated with accelaration.

# For Categorical variables -
# 1) Barplots to represent the count vs category.

for(columnName in catVars1){
  if(columnName!="CARNAME"){
    title <- paste("barplot for", columnName, sep = " ")
    barplot(table(dataset[[columnName]]), xlab=columnName, main = title, horiz = TRUE)
  }
}

# the bar plot for origin shows that majority of the data is for the US made cars (origin = 1)
# the bar plot for the model year is majorly evenly distributed with the highest cars from year 73.
# the bar plot for cylinders show that more than 200 cars from the dataset have 4 cylinders.

# 4. Create a correlation matrix for all of the numeric variables.
# 2 points

library('corrplot')
cor(na.omit(dataset[numVars]))
corMatrix <- cor(dataset[numVars])
corrplot(cor(na.omit(dataset[numVars])), method = "circle", diag = TRUE)

# 5. Create a box plot of mpg versus origin
# 2 points

boxplot(MPG~ORIGIN,data=dataset, main="MPG vs Origin", xlab="Origin", ylab="Miles Per Gallon")

# 6. Divide the data into a train/test set (80% and 20% respectively) using stratified sampling
# 2 points

# sample_size <- floor(0.80 * nrow(dataset))
# set.seed(123)
# train_sample<- sample(seq_len(nrow(dataset)), size = sample_size)
# train_data <- dataset[train_sample, ]
# test_data <- dataset[-train_sample, ]
# head(train_data)
# head(test_data)
set.seed(1001)
trainPct <- .8
library('caret')
inTrain <- createDataPartition(y = dataset$MPG, p = trainPct, list = FALSE)
train_data <- dataset[inTrain,]
test_data <- dataset[-inTrain,]
stopifnot(nrow(train_data) + nrow(test_data) == nrow(dataset))
head(train_data)
head(test_data)

# 7. Fit a linear model to the data using the numeric variables only. Calculate the R**2 on the test set.
# 3 points


xVars <- numVars1[-1]
targetVar <- 'MPG'
createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars, includeIntercept = TRUE)
model2 <- lm(modelForm, data = train_data)
summary(model2)

predTargetVar <- paste(targetVar, "_hat", sep = "")
test_data[,predTargetVar] <- predict(model2, test_data)


test_data[,'residualModel'] <- test_data[,predTargetVar] - test_data[,targetVar]
SST <- sum((test_data[,targetVar] - mean(test_data[,targetVar]))^2)
SSE <- sum(test_data['residualModel']^2)
rSqModel <- 1-SSE/SST

# The R-Squared on test-data: 0.7101716.

# linearmodel=lm(MPG ~ DISPLACEMENT + WEIGHT + ACCELERATION, data=dataset)
# #linearmodel=lm(V1~V3+V5+V6, data=dataset)
# summary(linearmodel)
# 
# #linearmodel_test=lm(V1~V3+V5+V6, data=test_data)
# linearmodel_test=lm(MPG ~ DISPLACEMENT + WEIGHT + ACCELERATION, data=test_data)
# summary(linearmodel_test)$r.squared


# 8. Programmatically identify and remove the non-significant variables (alpha = .05). Fit a new model with those variables removed.
# Calculate the R**2 on the test set with the new model. Did this improve performance?
# 4 points


xVars1 <- rownames(summary(model2)$coefficients[summary(model2)$coefficients[,4]<0.05,])[2]
modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars1, includeIntercept = TRUE)
model1 <- lm(modelForm, data = train_data)
summary(model1)

predTargetVar1 <- paste(targetVar, "_hat", sep = "")
test_data[,predTargetVar1] <- predict(model1, test_data)


test_data[,'residualModel1'] <- test_data[,predTargetVar1] - test_data[,targetVar]
SST1 <- sum((test_data[,targetVar] - mean(test_data[,targetVar]))^2)
SSE1 <- sum(test_data['residualModel1']^2)
rSqModel1 <- 1-SSE1/SST1

 #The R-Squared on the test-data: 0.6855307. The performance of the model doesnt improve when compared to the previous model.

# linearmodel=lm(MPG ~ DISPLACEMENT + WEIGHT + ACCELERATION, data=dataset)
# toselect.x <- summary(linearmodel)$coeff[-2,4] < 0.05
# relevant.x <- names(toselect.x)[toselect.x == TRUE]
# sig.formula <- as.formula(paste("y ~",paste(relevant.x, collapse= "+")))
# 
# new_model<-lm(formula = sig.formula, data=dataset)
# 
# #------------OR----------
# #Removed variables are V3 and V6 as p-value>0.05
# new_model<-lm(MPG ~ WEIGHT, data=dataset)
# summary(new_model)
# new_model_test<-lm(MPG ~ WEIGHT, data=test_data)
# summary(new_model_test)$r.squared

#R-Sqaured for test_data on new-model= 0.6863838. The performance of the model doesnt improve.

# 9. Attempt to fit a model on all of the relevant independent variables (including carName).
# Then calculate the R**2 on a test set. You will likely encounter an error.
# Explain why this error occurs. Fix this error.
# 4 points


xVars2 <- c(xVars1,catVars1)
modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars2, includeIntercept = TRUE)
model2 <- lm(modelForm, data = train_data)
summary(model2)

predTargetVar2 <- paste(targetVar, "_hat", sep = "")
test_data[,predTargetVar2] <- predict(model2, test_data)

# Error is due to carName variable which has records that were not previously seen in the training set.
# therefore, no dummy variables were present for these unencountered car names and hence, when the test record
# tries to predict the mpg for cars from test data which are not present, an error is issued.
# Hence, carName variable should not be considered in the model.

xVars3 <- c(xVars1,catVars1[which(catVars1 != "CARNAME")])
modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars3, includeIntercept = TRUE)
model3 <- lm(modelForm, data = train_data)
summary(model3)

predTargetVar3 <- paste(targetVar, "_hat", sep = "")
test_data[,predTargetVar3] <- predict(model3, test_data)

test_data[,'residualModel3'] <- test_data[,predTargetVar3] - test_data[,targetVar]
SST3 <- sum((test_data[,targetVar] - mean(test_data[,targetVar]))^2)
SSE3 <- sum(test_data['residualModel3']^2)
rSqModel3 <- 1-SSE3/SST3
#The R-Squared on the test-data: 0.842375.

# linearmodel1=lm(MPG ~ CYLINDERS + DISPLACEMENT + HORSEPOWER + WEIGHT + ACCELERATION + MODELYEAR + ORIGIN , data=test_data)
# summary(linearmodel1)$r.squared

#*******R-Squared=1, which means that the regression line perfectly fits the data.


# 10. Determine the relationship between model year and mpg.
# Interpret this relationship.
# Theorize why this relationship might occur.
# 4 points


boxplot(MPG ~ MODELYEAR, data=dataset)
means <- tapply(dataset$MPG,dataset$MODELYEAR,mean)
points(means,col="red",pch=18)
lines(means)



# it can be seen that the mean mpg has a positive relationship with year. As we go ahead in time, 
# the fuel efficiency is increasing (though a couple of drops are seen, the overall mpg is increasing)
# This is primarily due to the technology advancements, car designs and type of fuel.


rl_btw_modelyear_and_mpg<-lm( MPG ~ MODELYEAR, data=dataset)
summary(rl_btw_modelyear_and_mpg)
#It is linearly significant as both have a p-value<0.001 and hence are highly correlated.


# 11. Using only the variables provided, build the best linear model 
# you can (as measured by R**2 on the test data)
# Record the value obtained in the comments below. Make sure to show all your code.
# Record the best R**2 value on the test set in the comments below.
# My Best R**2 value: 0.8837109
# 4 points


plot(model3)
# as seen in the residual vs. fitted values plot, there seems to be a quadratic relationship between
# mpg and the variables. we use the stepwise regression method to fit the best model including the 
# quadratic terms.
library(leaps)
model4 <- lm(MPG ~ 1 + CYLINDERS + DISPLACEMENT + I(DISPLACEMENT^2) + HORSEPOWER + I(HORSEPOWER^2) +WEIGHT + I(WEIGHT^2)+ ACCELERATION + I(ACCELERATION^2)+MODELYEAR + ORIGIN, data=train_data)
summary(model4)

model5 <- step(model4, scope = list(lower= MPG~1, upper= MPG ~ 1 + CYLINDERS + DISPLACEMENT + I(DISPLACEMENT^2) + HORSEPOWER + I(HORSEPOWER^2) +WEIGHT + I(WEIGHT^2)+ ACCELERATION + I(ACCELERATION^2)+MODELYEAR + ORIGIN, data=train_data), direction = 'both')
summary(model5)

predTargetVar5 <- paste(targetVar, "_hat", sep = "")
test_data[,predTargetVar5] <- predict(model5, test_data)

test_data[,'residualModel5'] <- test_data[,predTargetVar5] - test_data[,targetVar]
SST5 <- sum((test_data[,targetVar] - mean(test_data[,targetVar]))^2)
SSE5 <- sum(test_data['residualModel5']^2)
rSqModel5 <- 1-SSE5/SST5

# the best model obtained using the quadratic terms has the R**2 value on the test-data = 0.8837109


# predTargetVar4 <- paste(targetVar, "_hat", sep = "")
# test_data[,predTargetVar4] <- predict(model4, test_data)
# 
# test_data[,'residualModel4'] <- test_data[,predTargetVar4] - test_data[,targetVar]
# SST4 <- sum((test_data[,targetVar] - mean(test_data[,targetVar]))^2)
# SSE4 <- sum(test_data['residualModel4']^2)
# rSqModel4 <- 1-SSE4/SST4

# # On the dataset
# linearmodel2=lm(MPG ~ CYLINDERS + DISPLACEMENT + HORSEPOWER + WEIGHT + ACCELERATION + MODELYEAR + ORIGIN + CARNAME, data=dataset)
# summary(linearmodel2)$r.squared
# #****R-Squared= 0.9934416
# #On the test dataset
# linearmodel1=lm(MPG ~ CYLINDERS + DISPLACEMENT + HORSEPOWER + WEIGHT + ACCELERATION + MODELYEAR + ORIGIN + CARNAME, data=test_data)
# summary(linearmodel1)$r.squared


# 12. Your boss wants to know if the 
# brand of the car will add predictive power to 
# your model. Create new variables called "brand" and "model" from the carName
# column. Do some research to figure out how to do this.
# Clean up the brand variable. Add the cleaned up "brand" variable to the
# best model you built from the previous question.
# Compare the adjusted R**2 on the test data set.
# Best Adjusted R**2 without brand variable: 0.8931
# Best Adjusted R**2 with brand variable: 0.9118
# 4 points

# library(reshape2)
# y <- colsplit(dataset$CARNAME," ",c("brand","model"))
# tail(y)

x <- dataset$CARNAME
rexp <- "^(\\w+)\\s?(.*)$"
y <- data.frame(brand=sub(rexp,"\\1",x), model=sub(rexp,"\\2",x))
tail(y)
dataset[,'BRAND'] <- as.factor(y$brand)
dataset[,'MODEL'] <- y$model

set.seed(377842)
trainPct <- .8
library('caret')
inTrain_new <- createDataPartition(y = dataset$BRAND, p = trainPct, list = FALSE)
train_data_new <- dataset[inTrain_new,]
test_data_new <- dataset[-inTrain_new,]
stopifnot(nrow(train_data_new) + nrow(test_data_new) == nrow(dataset))
head(train_data_new)
head(test_data_new)

#plot(model3)
# as seen in the residual vs. fitted values plot, there seems to be a quadratic relationship between
# mpg and the variables. we use the stepwise regression method to fit the best model including the 
# quadratic terms.
library(leaps)
model6 <- lm(MPG ~ 1 + CYLINDERS + DISPLACEMENT + I(DISPLACEMENT^2) + HORSEPOWER + I(HORSEPOWER^2) +WEIGHT + I(WEIGHT^2)+ ACCELERATION + I(ACCELERATION^2)+MODELYEAR + ORIGIN + BRAND, data=train_data_new)
summary(model6)

model7 <- step(model6, scope = list(lower= MPG~1, upper= MPG ~ 1 + CYLINDERS + DISPLACEMENT + I(DISPLACEMENT^2) + HORSEPOWER + I(HORSEPOWER^2) +WEIGHT + I(WEIGHT^2)+ ACCELERATION + I(ACCELERATION^2)+MODELYEAR + ORIGIN + BRAND, data=train_data_new), direction = 'both')
summary(model7)

predTargetVar7 <- paste(targetVar, "_hat", sep = "")
test_data_new[,predTargetVar7] <- predict(model7, test_data_new)

test_data_new[,'residualModel7'] <- test_data_new[,predTargetVar7] - test_data_new[,targetVar]
SST7 <- sum((test_data_new[,targetVar] - mean(test_data_new[,targetVar]))^2)
SSE7 <- sum(test_data_new['residualModel7']^2)
rSqModel7 <- 1-SSE7/SST7
# With the 'Brand' variable added the adjusted R-Squared value: 0.9118.
# Without the 'Brand' variable added the adjusted R-Squared value from the best model: 0.8931.
# The adjusted R-Squared values is more with the 'BRAND' variable in the datset.

