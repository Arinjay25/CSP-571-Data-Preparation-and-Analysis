# CSP571
# Homework 4


#Load in the auto mpg data set: https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data
# Note with this data set, you are trying to predict "mpg"
# Additional information is found at the link below
#, however additional interpretation may be needed from you
# https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.names



# 1. Identify the columns (if any) with ANY data that is missing our
# could be reasonably construed as missing. Replace with median or mode, where
# appropriate
# 4 points

fileUrl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
dataFrame<-NULL
dataFrame <- read.table(fileUrl, header=FALSE, na.strings = c('NA','?'), stringsAsFactors = TRUE)
names(dataFrame) <- c("Mpg", "Cylinders", "Displacement", "Horsepower", "Weight", "Acceleration", "ModelYear", "Origin", "CarName")

#checking the datatype of all columns
sapply(dataFrame, class)

# Checking NA in columns 
colSums(is.na(dataFrame))

#Here we found HorsePower have 6 missing value. Will replace it with median is more appropriate here.  

horsepower_med<-median(dataFrame$Horsepower, na.rm = TRUE)

dataFrame$Horsepower[is.na(dataFrame$Horsepower)]<-horsepower_med
head(dataFrame)

# 2. Identify all of the categorical variables, 
# all of the numeric variables
# Store it in the variables below.
# 2 points

sapply(dataFrame, class)

#ORIGIN, CYLINDERS, MODELYEAR are catagorical variables  
dataFrame$Origin <- as.factor(dataFrame$Origin)
dataFrame$Cylinders <- as.factor(dataFrame$Cylinders)
dataFrame$ModelYear <- as.factor(dataFrame$ModelYear)

sapply(dataFrame, class)

numVars<-names(dataFrame)[sapply(dataFrame, is.numeric)]
print(numVars)
catVars<-names(dataFrame)[sapply(dataFrame, is.factor)]
print(catVars)


# 3. Identify the appropriate descriptive statistics and graph for this data set.
# Execute on those and use the comments to discuss relevant relationships or insights discovered.
# 2 points

#summary of whole data set. 
summary(dataFrame)


# For Categorical columns I will use bar-chart for better understanding of number of elements per category.

for(k in catVars){
  if(k!= colnames(dataFrame[9])){ ##not loop on Carname col
    barplot(table(dataFrame[[k]]), xlab=k, las = 1)
  }
}


# Results and Information from BAR Chart - 
# column cylinders has 200+ records at 4 category.
# column Origin has 250+ records at 1 category.
# column Model Year is almost uniformly distributed, except at 73, 76 and 78. Max records are at 73


#For numeric columns I will use box-plot, histogram and plot between variables 
#BOXPLOT #Histogram #Plot
for (i in numVars){
  lable <- paste("Box-plot of", i) 
  boxplot(dataFrame[[i]], main = lable, xlab = i, las = 1)
  # histograms 
  hist(dataFrame[[i]], main = i, xlab = i)
  
  #Plot
  plot(y=dataFrame$Mpg, x=dataFrame[[i]], ylab = "Miles per gallon", xlab = i, las = 1)
}


# Results and Information from BOXPLOT - 
# 1 outlier in MPG value around 46 and a half numbers of cars have good Miles per gallon about 23. 
# many outliers cars have more than 200 horsepower. 
# weight seems perfect no outliers are present from min 1613, median 2804 to max 5140.
# In acceleration some lower and upper outliers and mean 15.50.

# Results and Information from histograms -
# from all histograms we can say Acceleration is more seem like Normally Distributed. 
# and others are like left-shifted, lower values have more frequencies like positively skewed Mode<Median<Mean on the x-axis.

# Results and Information from the plots -
# acceleration has a positive correlation with Mpg(Miles per gallon) 
# and others (Displacement, Horsepower, and Weighthave) have negative correlation with the Mpg((Miles per gallon) and non-linear.


# 4. Create a correlation matrix for all of the numeric variables.
# 2 points

library('corrplot')

corMatrix <- cor(dataFrame[numVars])
corMatrix
corrplot(corMatrix, method = "circle", diag = TRUE)



# 5. Create a box plot of mpg versus origin
# 2 points

boxplot(dataFrame$Mpg~dataFrame$Origin, xlab = 'Origin', ylab = 'Mpg(Miles per gallon)', las = 1)


# 6. Divide the data into a train/test set (80% and 20% respectively) using stratified sampling
# 2 points


#install.packages('splitstackshape')
library(splitstackshape)
library(dplyr)
set.seed(42)

#train_DF<-sample_frac(dataFrame, 0.8)
#sid<-as.numeric(rownames(train_DF)) # because rownames() returns character
#test_DF<-dataFrame[-sid,]

#indexs <- stratified(dataFrame, "Mpg", size=0.8)
#valid <- df[-validIndex,]
#train <- df[validIndex,]

library('caret')
set.seed(42)
indexs <- createDataPartition(y = dataFrame$Mpg, times = 1, p = 0.8, list = FALSE)
train_DF <- dataFrame[indexs,]
test_DF <- dataFrame[-indexs,]
head(train_DF)

# 7. Fit a linear model to the data using the numeric variables only. Calculate the R**2 on the test set.
# 3 points

#Liner model
groupvars<-numVars[-1]
# This returns the formula:
modelFormula <- as.formula(paste('Mpg', paste(groupvars, collapse=" + "), sep=" ~ "))
model <- lm(modelFormula, data = train_DF)# build the model
summary(model)

Mpg_pred<-predict(model, test_DF)

#residual = predict - actual 
res<- Mpg_pred - test_DF$Mpg
sse <- sum(res**2)
#sst = sum((y-yhat)**2)
sst<- sum((test_DF$Mpg-mean(test_DF$Mpg))**2)  
rSq <- 1-sse/sst
rSq #R**2 on  test data is 0.668995


#R square from test model on numeric data only
#test_model = lm(modelFormula, data=test_DF)
#summary(test_model)$r.squared
#R**2 on  test model is 0.6847652

# 8. Programmatically identify and remove the non-significant variables (alpha = .05). Fit a new model with those variables removed.
# Calculate the R**2 on the test set with the new model. Did this improve performance?
# 4 points

xvars1 <- rownames(summary(model)$coefficients[summary(model)$coefficients[,4]>0.05,])
xvars1 #non-significant variables P-value > (alpha = .05)

modelFormula1 <- as.formula(paste('Mpg', paste(xvars1, collapse=" + "), sep=" ~ "))
model1 <- lm(modelFormula1, data = train_DF)
summary(model1)

Mpg_pred1<-predict(model1, test_DF)

#residual = predict - actual 
res1<- Mpg_pred1 - test_DF$Mpg
sse1 <- sum(res1**2)
#sst = sum((y-yhat)**2)
sst1<- sum((test_DF$Mpg-mean(test_DF$Mpg))**2)  
rSq1 <- 1-sse1/sst1
rSq1 #R**2 on  test data is 0.603126
#The performance of the model does not seems improve when compared to the previous model.

#R square from test model on numeric data only
#test_model1 = lm(modelFormula1, data=test_DF)
#summary(test_model1)$r.squared
#R**2 on  test model is 0.6235423

#The R-Squared on the test-data: 0.6235423 
#The performance of the model does not seems improve when compared to the previous model.


# 9. Attempt to fit a model on all of the relevant independent variables (including carName).
# Then calculate the R**2 on a test set. You will likely encounter an error.
# Explain why this error occurs. Fix this error.
# 4 points

relxvars<-rownames(summary(model)$coefficients[summary(model)$coefficients[,4]<0.05,])[-1]
xvars2<-c(relxvars,catVars)
modelFormula2 <- as.formula(paste('Mpg', paste(xvars2, collapse=" + "), sep=" ~ "))

#Creating model(Name is model9 for question-9)
model9 <- lm(modelFormula2, data = train_DF)
summary(model9)

Mpg_pred9<-predict(model9, test_DF)

# Error is due to carName variable have some value or names that are new or unseen in the training set.
# and there is no dummy variables for the same records. Hence, when the test record
# tries to predict the mpg for cars from test data which are not present, an error is occurred.
# One soluction: carName variable should not be considered in the model.

xVars3 <- c(relxvars,catVars[which(catVars != "CarName")])
modelFormula91 <- as.formula(paste('Mpg', paste(xVars3, collapse=" + "), sep=" ~ "))
model91 <- lm(modelFormula91, data = train_DF)
summary(model91)


Mpg_pred91<-predict(model91, test_DF)

#residual = predict - actual 
res91<- Mpg_pred91 - test_DF$Mpg
sse91 <- sum(res91**2)
#sst = sum((y-yhat)**2)
sst91<- sst ##always same 
rSq91 <- 1-sse91/sst91
rSq91#R**2 on  test data is 0.8376818

## Here we go we have biger R**2 here that means this model that the regression line perfectly fits the data.


# 10. Determine the relationship between model year and mpg.
# Interpret this relationship.
# Theorize why this relationship might occur.
# 4 points


average_mpg_year<- tapply(dataFrame$Mpg,dataFrame$ModelYear,mean)
yearvalue<-unique(dataFrame$ModelYear)
numericyear<-as.numeric(levels(yearvalue))[yearvalue]
cor_mpg_myears<-cor(numericyear,average_mpg_year, method = "pearson")
cor_mpg_myears #0.884

data= data.frame(yearvalue,average_mpg_year)
plot(data, xlab="Years", ylab=" Miles per gallon")
title (" Miles per gallon over Years")
points(average_mpg_year,col="blue",pch=19)
lines(average_mpg_year)

#highly positive correlation that means they have a positive increasing relationship between Mpg and Model years. 
# and seems logically correct because as per market demand for better Miles per gallon.(though a couple of drops are seen, the overall mpg is increasing) 
#so every year companies try to give better performance in this direction that we can see in the above plot.


# 11. Using only the variables provided, build the best linear model 
# you can (as measured by R**2 on the test data)
# Record the value obtained in the comments below. Make sure to show all your code.
# Record the best R**2 value on the test set in the comments below.
# My Best R**2 value: 0.8640147
# 4 points

library(leaps)
squ_model <- lm(Mpg ~ 1 + Cylinders + Displacement + I(Displacement^2) + Horsepower + I(Horsepower^2) +Weight + I(Weight^2)+ Acceleration + I(Acceleration^2)+ModelYear + Origin, data=train_DF)
summary(squ_model)

mybest_model <- step(squ_model, scope = list(lower= Mpg~1, upper= Mpg ~ 1 + Cylinders + Displacement + I(Displacement^2) + Horsepower + I(Horsepower^2) +Weight + I(Weight^2)+ Acceleration + I(Acceleration^2)+ModelYear + Origin, data=train_DF), direction = 'both')
summary(mybest_model)


predmybest <- predict(mybest_model, test_DF)

residual_mybest_Model <- predmybest - test_DF[,"Mpg"]
SST_mybest_model <- sst   #SST never change
SSE_mybest_model <- sum(residual_mybest_Model**2)

rSq_mybest_Model <- 1-SSE_mybest_model/SST_mybest_model
rSq_mybest_Model

# the best model obtained using the quadratic terms has the R**2 value on the test-data = 0.8640147
# best model formula get from summary: summary(mybest_model)
bestmodelFormula = Mpg ~ Cylinders + Displacement + I(Displacement^2) + Horsepower + Weight + I(Weight^2) + Acceleration + I(Acceleration^2) + ModelYear

# this function return the Adjusted R-squared
adjRSquare<-function(n,k,Rsqu){
  adjRSqu = 1- (1-Rsqu)*(n-1)/(n-k-1)
  return(adjRSqu)
}

#cal the Adjusted R-squared
n = nrow(test_DF)
k = 9 #no of parameter of above model get this from summary
Rsqu = rSq_mybest_Model
adj_rSq_bestmodel<-adjRSquare(n,k,Rsqu)
adj_rSq_bestmodel
#Best Adjusted R**2 without brand: 0.8457481

# 12. Your boss wants to know if the 
# brand of the car will add predictive power to 
# your model. Create new variables called "brand" and "model" from the carName
# column. Do some research to figure out how to do this.
# Clean up the brand variable. Add the cleaned up "brand" variable to the
# best model you built from the previous question.
# Compare the adjusted R**2 on the test data set.
# Best Adjusted R**2 without brand variable: 0.8457481
# Best Adjusted R**2 with brand variable: 0.9256365
# 4 points

#Findng brand name and car model
carName <- dataFrame$CarName
rexp <- "^(\\w+)\\s?(.*)$"
brand_carmodel <- data.frame(CarBrand=sub(rexp,"\\1",carName), CarModel=sub(rexp,"\\2",carName))
head(brand_carmodel)
dataFrame[,'BRAND'] <- as.factor(brand_carmodel$CarBrand)
dataFrame[,'MODEL'] <- brand_carmodel$CarModel
tail(dataFrame)


set.seed(42)
#library('caret')
inTrain_new <- createDataPartition(y = dataFrame$BRAND, p = 0.8, list = FALSE)
train_DF_new <- dataFrame[inTrain_new,]
test_DF_new <- dataFrame[-inTrain_new,]
stopifnot(nrow(train_DF_new) + nrow(test_DF_new) == nrow(dataFrame))
head(train_DF_new)
head(test_DF_new)


library(leaps)
squ_model12 <- lm(Mpg ~ 1 + Cylinders + Displacement + I(Displacement^2) + Horsepower + I(Horsepower^2) +Weight + I(Weight^2)+ Acceleration + I(Acceleration^2)+ModelYear + Origin + BRAND, data=train_DF_new)
summary(squ_model12)

mybest_model12 <- step(squ_model12, scope = list(lower= Mpg~1, upper= Mpg ~ 1 + Cylinders + Displacement + I(Displacement^2) + Horsepower + I(Horsepower^2) +Weight + I(Weight^2)+ Acceleration + I(Acceleration^2)+ModelYear + Origin + BRAND, data=train_DF_new), direction = 'both')
summary(mybest_model12)

#best model formula with brand: formula = Mpg ~ Cylinders + Horsepower + I(Horsepower^2) + 
# Weight + I(Weight^2) + Acceleration + I(Acceleration^2) + 
#  ModelYear + BRAND

predmybest12 <- predict(mybest_model12, test_DF_new)

residual_mybest_Model12 <- predmybest12 - test_DF_new[,"Mpg"]
SST_mybest_model12 <- sum((test_DF_new[,"Mpg"] - mean(test_DF_new[,"Mpg"]))^2)
SSE_mybest_model12 <- sum(residual_mybest_Model12**2)

rSq_mybest_Model12 <- 1-SSE_mybest_model12/SST_mybest_model12
rSq_mybest_Model12

#Best model R-Square with Brand: 0.935777

brandbestformula<- Mpg ~ Cylinders + Horsepower + I(Horsepower^2) + 
  Weight + I(Weight^2) + Acceleration + I(Acceleration^2) + 
  ModelYear + BRAND

#cal the Adjusted R-squared
n12 = nrow(test_DF_new)
k12 = 9 #no of parameter of above model get this from summary
Rsqu12 = rSq_mybest_Model12
adj_rSq_bestmodel12<-adjRSquare(n12,k12,Rsqu12)
adj_rSq_bestmodel12
#Best Adjusted R**2 with brand: 0.9256365

# With the 'Brand' variable added the adjusted R-Squared value: 0.9256365.
# Without the 'Brand' variable added the adjusted R-Squared value from the best model: 0.8457481.
# The adjusted R-Squared values is more with the 'BRAND' variable in the datset.





#creating model on test data set.
#test_model12 <- lm(brandbestformula, data = train_DF_new)
#summary(test_model12)

#best model Adjusted R-squared on test data set: 0.9006509
#summary(test_model12)$adj.r.square