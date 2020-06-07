# CSP 571 Homework 5


# 1. Please write a function called backwards() that implements the
# backward selection algorithm using AIC.
# 4 points

backwards <- function(model,scope){
  nextIter <- TRUE
  while(formula(model) != scope$lower & nextIter){
    nextIter <- FALSE
    modelAIC <- AIC(model)
    vars <- all.vars(formula(model))
    y <- vars[1]
    xPreds <- vars[2:length(vars)]
    previousModel <- model               
    previousModelAIC <- modelAIC
    for(predictor in xPreds){
      newModel <- update(model, as.formula(paste(".~. -",predictor)))
      newModelAIC <- AIC(newModel)
      if (previousModelAIC > newModelAIC){
        previousModel <- newModel
        previousModelAIC <- newModelAIC
        #add a flag 
        nextIter <- TRUE
      }
    }
    model <- previousModel
    modelAIC <- previousModelAIC
  }
  return(model)
}

# 2. Download the credit card default data set from the UCI machine learning
# repository. https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
# Load the data into R.
# 2 points

#install.packages('readxl')
library('readxl')
defClientsData<-read_excel("default of credit card clients.xls")
head(defClientsData)
defClientsData <- defClientsData[-1,]
coln <- c("ID","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","AGE","PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6","default payment next month")
colnames(defClientsData) <- coln
defClientsData$`default payment next month` <-ifelse(defClientsData$`default payment next month` == 0, FALSE, TRUE)
defClientsData[,c(1,2,6:24)] <- apply(defClientsData[,c(1,2,6:24)],2, as.numeric)


# 3. Identify all the relevant categorical, numeric, and logical variables# 2 points.

factVars <- colnames(defClientsData)[sapply(defClientsData, is.factor)]
numVars  <- colnames(defClientsData)[sapply(defClientsData, is.numeric)]
logVars  <- colnames(defClientsData)[sapply(defClientsData, is.logical)]
targetVar  <- c("default payment next month")
xVars <- coln[-which(coln == "default payment next month")]

# 4. Perform all required EDA on this data set.
# 4 points

# Histogram, Boxplot and scatter plot for Numeric variables

#install.packages('ggplot2')
pdf("Problem4_Graphs.pdf")
library('ggplot2')
for(columnName in numVars){
  title <- paste("Histogram for", columnName, sep = " ")
  hist(defClientsData[[columnName]], main = title, xlab = columnName)
  
  title <- paste("Boxplot for", columnName, sep = " ")
  boxplot(defClientsData[[columnName]], main = title, xlab = columnName)
  
  print(ggplot(defClientsData, aes(`default payment next month`, columnName)) + geom_jitter())
}

# scatter plot matrix -- Note this takes long time to run and hence is attached in a separate file.
plot(defClientsData[which(colnames(defClientsData) %in% numVars)])

# Bar plots for categorical variables.
for(columnName in factVars){
  title <- paste("barplot for", columnName, sep = " ")
  barplot(table(defClientsData[[columnName]]), ylab=columnName, main = title, horiz = TRUE, xlim = c(0,20000))
}
library('corrplot')
corMat <- cor(defClientsData[which(colnames(defClientsData) %in% numVars)])
corrplot(corMat)

dev.off()

# 5.Build a logistic regression model to determine whether or not a
# customer defaulted. Use all of the variables. Validate the model on a
# test data set. Use the comments to discuss the performance of the model.
# 4 points

library('lubridate')
library('caret')
set.seed(8777)
sampleIndex <- createDataPartition(y = defClientsData$`default payment next month`, p = 0.8, list = FALSE)
trainData   <- defClientsData[sampleIndex,]
testData    <- defClientsData[-sampleIndex,]
stopifnot(nrow(trainData) + nrow(testData) == nrow(defClientsData))

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(sprintf("`%s`", targetVar), "~", paste(xVars, collapse = '+ '),sep = ""))
  } else {
    modelForm <- as.formula(paste(sprintf("`%s`", targetVar), "~", paste(xVars, collapse = '+ '), -1,sep = ""))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars,TRUE)

logitFit <- glm(modelForm,family=binomial(link='logit'),data=trainData)
predDef <- predict(logitFit, testData,type="response")
defaulted.pred <- ifelse(predDef > 0.5,TRUE,FALSE)
conf_matrix <- table(defaulted.pred, testData$`default payment next month`)
confusionMatrix(conf_matrix, positive = "TRUE")

# The model has an accuracy of 81.38% at the threshold of 0.5 but the Recall (Sensitivity) is 25.17%. 

# 6. Using forward selection, determine the best model.
# 4 points

forward.fit <- step(glm(`default payment next month`~1,family=binomial(link='logit'),data = trainData),direction = 'forward',scope = list(lower = `default payment next month`~1, upper =modelForm ))
formula(forward.fit)
predfwdfit <- predict.glm(forward.fit, testData, type = "response")
fwdfit.pred <- ifelse(predfwdfit > 0.5,TRUE,FALSE)
conf_matrix1 <- table(fwdfit.pred, testData$`default payment next month`)
confusionMatrix(conf_matrix1, positive = "TRUE")

# The best model is
# Step:  AIC=22306.8
# `default payment next month` ~ PAY_0 + LIMIT_BAL + PAY_AMT2 + 
#   PAY_2 + BILL_AMT1 + MARRIAGE + EDUCATION + PAY_AMT1 + PAY_5 + 
#   BILL_AMT3 + PAY_3 + PAY_AMT4 + SEX + AGE + PAY_AMT5
# 
# Df Deviance   AIC
# + PAY_AMT6   1    22258 22306
# <none>            22261 22307
# + PAY_AMT3   1    22260 22308
# + BILL_AMT5  1    22260 22308
# + BILL_AMT6  1    22260 22308
# + BILL_AMT2  1    22260 22308
# + PAY_4      1    22260 22308
# + ID         1    22261 22309
# + PAY_6      1    22261 22309
# + BILL_AMT4  1    22261 22309

# 7. Using the backwards selection function you implemented in #1
# , determine the best model.
#2 points

backfit <- backwards(logitFit, scope = list(lower = `default payment next month`~1, upper =modelForm))
formula(backfit)
summary(backfit)
# the best model is when AIC: 22307.
predbckfit <- predict.glm(backfit, testData, type = "response")
bckfit.pred <- ifelse(predbckfit > 0.5,TRUE,FALSE)
conf_matrix2 <- table(bckfit.pred, testData$`default payment next month`)
confusionMatrix(conf_matrix2, positive = "TRUE")
# The accuracy is 81.35%.The best model is
# `default payment next month` ~ LIMIT_BAL + SEX + EDUCATION + 
#   MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_5 + BILL_AMT1 + 
#   BILL_AMT2 + BILL_AMT5 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + 
#   PAY_AMT4 + PAY_AMT5 + PAY_AMT6

# 8. Run an implementation of backwards selection found in an R package on this
# data set. Discuss any differences between the results of this implementation
# and your implemnetation in question 7.
# 2 points

backward.fit <- step(logitFit,direction = 'backward',scope = list(lower = `default payment next month`~1, upper =modelForm ))
formula(backward.fit)
summary(backward.fit)

predbckfit <- predict.glm(backward.fit, testData, type = "response")
bckfit.pred <- ifelse(predbckfit > 0.5,TRUE,FALSE)
conf_matrix2 <- table(bckfit.pred, testData$`default payment next month`)
confusionMatrix(conf_matrix2, positive = "TRUE")
# The model generated by backwards() and step() are the same because both use AIC as the criteria.
# The accuracy is 81.35%. The best model is:
# Start:  AIC=22317.84
# `default payment next month` ~ ID + LIMIT_BAL + SEX + EDUCATION + 
#   MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + 
#   PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + 
#   BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + 
#   PAY_AMT6


# 9. Run lasso regression on the data set. Briefly discuss how you determined
# the appropriate tuning parameters.
# 2 points
library(lars)
x<- model.matrix(modelForm,trainData)
new.x <- model.matrix(modelForm, testData)
fit.lasso <- lars(x=as.matrix(x),y=as.numeric(trainData$`default payment next month`), type="lasso")

#Method 1 - Considering Mellow's Cp
optLambda <- fit.lasso$lambda[which.min(fit.lasso$Cp)-1]
coef(fit.lasso, s=optLambda, mode = "lambda")  #print the co-efficients of the parameters at optimal Lambda.

lassoPred <- predict.lars(fit.lasso, new.x,s=optLambda, type = "fit", mode="fraction")
lasso.Pred <- ifelse(lassoPred$fit > 0.5, TRUE, FALSE)
conf_matrix3 <- table(lasso.Pred, testData$`default payment next month`)
confusionMatrix(conf_matrix3, positive = "TRUE")

# the optimum tuning parameter is the lambda value corresponding to the lowest Mellow's Cp.

#Method 2 - Considering CV
cv.lasso <- cv.lars(x=as.matrix(x),y=as.numeric(trainData$`default payment next month`), type="lasso")
limit <- min(cv.lasso$cv) + cv.lasso$cv.error[which.min(cv.lasso$cv)]
cv.optLambda <-cv.lasso$index[min(which(cv.lasso$cv < limit))]
coef(fit.lasso, s=cv.optLambda, mode="lambda")

cvlassoPred <- predict(fit.lasso, new.x,s = cv.optLambda, type = "fit", mode = "fraction")
lasso.Pred <- ifelse(cvlassoPred$fit > 0.5, TRUE, FALSE)
conf_matrix4 <- table(lasso.Pred, testData$`default payment next month`)
confusionMatrix(factor(lasso.Pred), factor(testData$`default payment next month`), positive = "TRUE")

# 10. Run ridge regression on the data set. Briefly discuss how you determined
# the appropriate tuning parameters.
# 2 points

#install.packages('glmnet')
library(glmnet)
fit.ridge <- cv.glmnet(x=as.matrix(x), y=as.numeric(trainData$`default payment next month`), alpha=0, family='binomial')
optLambdaRidge <- fit.ridge$lambda.1se
ridge.fit <- glmnet(x=as.matrix(x), y=as.numeric(trainData$`default payment next month`), alpha=0, lambda=optLambdaRidge, family='binomial')
ridge.predict <- predict(ridge.fit, newx = new.x, s = optLambdaRidge, type = "response") 
fit.ridge$lambda.1se
# the optimum tuning parameter is the lambda value corresponding to the lowest Mellow's Cp

# 11. Run naive bayes on the data set.
# 2 points
library(e1071)
library(caret)
naiveBayes.fit <- naiveBayes(modelForm, data = trainData)
#testData <- as.numeric(testData)
naivePred <- predict(naiveBayes.fit, testData, type = "class")
conf_matrix6 <- table(naivePred, testData$`default payment next month`)
confusionMatrix(conf_matrix6, positive = "TRUE")
# The accuracy is 78.61%. The recall is 72.19%.

# 12. Build a decision tree to classify the customers as defaulted
# or not-defaulted. Plot the resulting tree. Discuss whether you
# feel this is a good model.
# 2 points

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)
decTree <- rpart(modelForm, data = trainData, method = "class",parms = list(split="information"))
summary(decTree)
fancyRpartPlot(decTree)

preddecTree <- predict(decTree, testData, type = "class")
conf_matrix7<- table(preddecTree,testData$`default payment next month`)
confusionMatrix(conf_matrix7, positive = "TRUE")

# Decision tree has an accuracy of 81.78%, the Recall is 32.17%
# in this particular case of the data, the Recall should be more as we would want to be cautious
# and predict as many defaulters as possible.

# 13. Build a random forest model and apply it to classify the test data set.
# 2 points

#install.packages('randomForest')
library(randomForest)
fitRF<- randomForest(x = trainData[-which(colnames(trainData)=="default payment next month")],y=trainData$`default payment next month`,data=trainData,importance=TRUE, type = "class")
fitRF
varImpPlot(fitRF)

PredictRF <- predict(fitRF, testData, type = "response")

RF.pred <- ifelse(PredictRF > 0.5, TRUE, FALSE)
confusionMatrix(factor(RF.pred),factor(testData$`default payment next month`), positive = "TRUE")
#The accuracy of this model is 81.23%.

# 14. Discuss the comparative performance of all of the models used. How should
# we determine which is best? Provide justification for your answer.
# 4 points

#Statistical methods to find the best model are as follows:

#Adjusted Rsquared and Predicted R squared:
#The adjusted R squared increases only if the new term improves the model after adding the predictors if the explain the variance and it can also decrease with poor quality predictors.
#The predicted R-squared is a form of cross-validation and it can also decrease.But increase when new variable is added irrespective of its explanation of the model.

#Cross-validation helps us to determine how our model generalizes other data sets after partitioning the data.

#P-values for the predictors:
#In regression, low p-values indicate terms that are statistically significant.
#Reducing the is a practice of including all candidate predictors in the model,and then systematically removing the term with the highest p-value one-by-one until we areleft with only significant predictors.

#Stepwise regression and Best subsets regression:
#Both the procedures automated.  They are used to identify useful predictors in exploratory stages of model building. They output best subsets regression, with statical inference.


# We use Recall and Accuracy as the comparision criteria metric.As we want to classify as many defaulters as possible.Hence a model with high recall and accuracy would be preferred.

# Decision tree has the highest accuracy of all the models (81.78%) and recall as high as 0.32178 the decision tree splits on PAY_0 because if the customer has defaulted in the current month it is more likely that the customer will default in the next month as well.

#In the above models Logistic regression has good accuracy 81.36% but it has a recall percent of 25.17% at the threshold of 0.5.
# Lasso and ridge have a good accuracy but not a very good recall similar to logistic regrssion and hence they are less prefferable.
# Naive Bayes has both accuracy and recall values low, this might be because of the independence assumption.
# Random Forest has the accuracy is 0.8123, the recall as .35 which is very low .Hence not preferred.