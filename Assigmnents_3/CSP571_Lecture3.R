#CSP/MATH 571 Lecture 3
library(MASS)
head(survey)

# Correlation Examples


# Pearson Correlation
# Recall the student data set
# Want to know whether there is a linear relationship between 
# Wr.Hnd and Height. Pearson correlation can help.
# We will arbitrarily assign x and y

# Will drop any row containing na for now
print(nrow(survey))
dt <- na.omit(survey)
print(nrow(dt))
x <- dt$Wr.Hnd
y <- dt$Height

plot(x, y) # more on plotting later! preliminary

  
  
numerator = sum((x-mean(x))*(y-mean(y)))
denominator = sqrt(sum((x-mean(x))**2)* sum((y-mean(y))**2))
pearsonCor = numerator/denominator
pearsonCor
c_pear <- cor(x, y, method = 'pearson')

# Note this new function
# This is how you do asserts in R
stopifnot(abs(pearsonCor - c_pear) <.001)


# Spearman correlation


xRank = rank(x)
yRank = rank(y)

numerator = sum((xRank-mean(xRank))*(yRank-mean(yRank)))
denominator = sqrt(sum((xRank-mean(xRank))**2)* sum((yRank-mean(yRank))**2))
spearCor = numerator/denominator
spearCor

c_spear <- cor(x, y, method = 'spearman')
c_spear
c_spear2 <- cor(rank(x), rank(y), method = 'pearson')
c_spear2
stopifnot(abs(spearCor - c) <.001)


# What is the association between smoking and exercise?
# Question to the class: How should we approach this?

head(survey$Exer)
head(survey$Smoke)


# More on dealing with factor variables later!
x = as.character(dt$Exer)
x
y = as.character(dt$Smoke)
y

y <- replace(y, y=='Never', values = 0)
y
y <- replace(y, y=='Occas', values = 1)
y <- replace(y, y=='Regul', values = 2)
y <- replace(y, y=='Heavy', values = 3)
y
y <- as.numeric(y)

# Do the same for X
x <- replace(x, x=='None', values = 0)
x <- replace(x, x=='Some', values = 1)
x <- replace(x, x=='Freq', values = 2)
x <- as.numeric(x)

plot(x, y)
plot(y ~jitter(x, 1))

sm1 <- cor(x, y, method = 'spearman')
sm1




# Wait! You just made up those values for the different levels

y <- replace(y, y=='Never', values = 0*5)
y <- replace(y, y=='Occas', values = 1*5)
y <- replace(y, y=='Regul', values = 2*5)
y <- replace(y, y=='Heavy', values = 3*5)
y <- as.numeric(y)

x <- replace(x, x=='None', values = 0*100)
x <- replace(x, x=='Some', values = 1*100)
x <- replace(x, x=='Freq', values = 2*100)
x <- as.numeric(x)

sm2<- cor(x, y, method = 'spearman')
sm2

sm1
sm2




# Since we are using the rank, the values we assign don't matter as long as they 
# preserve the order of the ordinal values

## T Test example

USCars <- c(18,  15,	18,	16,	17,	15,	14,	14,	14,	15,	15,	14,	15,	14,	22,	18,	21,	21,	10,	10,	11,	9,	28,	25,	19,	16,	17,	19,	18,	14,	14,	14,	14,	12,	13,	13,	18,	22,	19,	18,	23,	26,	25,	20,	21,	13,	14,	15,	14,	17,	11,	13,	12,	13,	15,	13,	13,	14,	22,	28,	13,	14,	13,	14,	15,	12,	13,	13,	14,	13,	12,	13,	18,	16,	18,	18,	23,	11,	12,	13,	12,	18,	21,	19,	21,	15,	16,	15,	11,	20,	21,	19,	15,	26,	25,	16,	16,	18,	16,	13,	14,	14,	14,	28,	19,	18,	15,	15,	16,	15,	16,	14,	17,	16,	15,	18,	21,	20,	13,	23,	20,	23,	18,	19,	25,	26,	18,	16,	16,	15,	22,	22,	24,	23,	29,	25,	20,	18,	19,	18,	27,	13,	17,	13,	13,	13,	30,	26,	18,	17,	16,	15,	18,	21,	19,	19,	16,	16,	16,	16,	25,	26,	31,	34,	36,	20,	19,	20,	19,	21,	20,	25,	21,	19,	21,	21,	19,	18,	19,	18,	18,	18,	30,	31,	23,	24,	22,	20,	22,	20,	21,	17,	18,	17,	18,	17,	16,	19,	19,	36,	27,	23,	24,	34,	35,	28,	29,	27,	34,	32,	28,	26,	24,	19,	28,	24,	27,	27,	26,	24,	30,	39,	35,	34,	30,	22,	27,	20,	18,	28,	27,	34,	31,	29,	27,	24,	23,	38,	36,	25,	38,	26,	22,	36,	27,	27,	32,	28,	31)

JapaneseCars <- c(24,  27,	27,	25,	31,	35,	24,	19,	28,	23,	27,	20,	22,	18,	20,	31,	32,	31,	32,	24,	26,	29,	24,	24,	33,	33,	32,	28,	19,	32,	34,	26,	30,	22,	22,	33,	39,	36,	28,	27,	21,	24,	30,	34,	32,	38,	37,	30,	31,	37,	32,	47,	41,	45,	34,	33,	24,	32,	39,	35,	32,	37,	38,	34,	34,	32,	33,	32,	25,	24,	37,	31,	36,	36,	34,	38,	32,	38,	32,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999,	-999)

# We are told Japanese values of -999 are an error
# Lets remove them

JapaneseCars <- JapaneseCars[JapaneseCars!= -999]


nUs <- length(USCars)
nJapanese <- length(JapaneseCars)

meanUs <- mean(USCars)
meanJapanese <- mean(JapaneseCars)

stdUs <- sd(USCars)
stdJapanese <- sd(JapaneseCars)


t.test(x = USCars, y = JapaneseCars, alternative = 'two.sided'
       , paired = FALSE
       , var.equal = FALSE
       , conf.level = .95)


t.test(x = USCars, y = JapaneseCars, alternative = 'two.sided'
       , paired = FALSE
       , var.equal = TRUE
       , conf.level = .95)


## Anova Example

lowCal <- c(8,  9,	6,	7,	3)
lowFat <- c(2,  4,	3,	5,	1)
lowCarb <- c(3,  5,	4,	2,	3)
control <- c(2,  2,	-1,	0,	3)

alpha = .05

# Compute the group means
meanLowCal <- mean(lowCal)
meanLowFat <- mean(lowFat)
meanLowCarb <- mean(lowCarb)
meanControl <- mean(control)

allObs <- c(lowCal, lowFat, lowCarb, control)
grandMean <- mean(allObs)


nLowCal <- length(lowCal)
nLowFat <- length(lowFat)
nLowCarb <- length(lowCarb)
nControl <- length(control)
N = length(allObs)


# Compute the Sum of Squares Between Groups (SSB)

SSB <-
  (nLowCal * (meanLowCal - grandMean)^2
  + nLowFat * (meanLowFat - grandMean)^2
  + nLowCarb * (meanLowCarb - grandMean)^2
  + nControl * (meanControl - grandMean)^2)

# Compute the Sum of Square Errors
lowCalSqDiff <- sum((lowCal - meanLowCal)^2)
lowFatSqDiff <- sum((lowFat - meanLowFat)^2)
lowCarbSqDiff <- sum((lowCarb - meanLowCarb)^2)
controlSqDiff <- sum((control - meanControl)^2)
SSE = lowCalSqDiff + lowFatSqDiff + lowCarbSqDiff + controlSqDiff

# Compute the degrees of freedom
df1 = 4 - 1
df2 = N - 4

# Compute the Means Squares
MSB = SSB / df1
MSE = SSE / df2

# Compute the F-statistic
F = MSB / MSE

# Find the critical value of F
# Reject H0 if F >= criticalValue
criticalValue = qf(p = 1 - alpha
   , df1 = df1
   , df2 = df2
   )

ifelse(F >= criticalValue, "Reject H0", "Fail to reject H0")

# Do Anova with R's built-in function
# We need to change the data
treatment <- c(
   rep('lowCal', 5)
  , rep('lowFat', 5)
  , rep('lowCarb', 5)
  , rep('control', 5)
  )
weightLoss <- c(lowCal, lowFat, lowCarb
                , control)

df = data.frame(treatment, weightLoss)

fit <- aov(weightLoss ~ treatment)
fit
summary(fit)


### Chi Squared Example

library(MASS)
data(survey)

# Create the contigency table
tbl = table(survey$Smoke, survey$Exer)
tbl

n <- sum(tbl)
rows <- rowSums(tbl)
cols <- colSums(tbl)
r <- length(rows)
c <- length(cols)

install.packages('reshape')
library(reshape)
# Expand the table into one row for each combination
tbl2 <- melt(tbl)
# Preserve the naems
names(tbl2) <- c('Smoking', 'Exercise', 'Observed')
tbl2

# Compute the expected frequencies for each combination
# Note I wrote this for clarity, not efficiency
# There are better ways of doing this, but they make understanding the point
# more difficult.

# Create a place to stored the expected values
expectedVals <- c()
for(i in 1:nrow(tbl2)){
  # Look up the levels for smoking and exercise
  Smoking <- tbl2[i, 1]
  Exercise <- tbl2[i, 2]

  # Return the sums that we already computed
  # for that level of smoking and exercise
  SmokingVal <- rows[Smoking]
  ExerciseVal <- cols[Exercise]

  # Computed the expected count per the formula
  expected <- (SmokingVal * ExerciseVal)/n

  # Append the expected count to the vector
  expectedVals <- c(expectedVals, expected)
}


tbl2['Expected'] <- expectedVals
tbl2['(O - E)**2 / E'] <- (tbl2['Observed']- tbl2['Expected'])**2 / tbl2['Expected']
tbl2

chiSquaredVal = sum(tbl2['(O - E)**2 / E'])

# Compute the degrees of freedom as specified for this distribution
df = (r - 1) * (c - 1)

alpha = .05
criticalVal <- qchisq(p = 1 - alpha, df = df)
pVal <- pchisq(chiSquaredVal, df, lower.tail = FALSE)

if(chiSquaredVal >= criticalVal){
  print("We reject the null hypothesis")
} else {
  print("We fail to reject the null hypothesis")
}


# In practice, we use R's builtin method for this
chisq.test(tbl)
chisq.test(x = survey$Smoke, y = survey$Exer
           , simulate.p.value = TRUE
           , B = 1000000)


# Clarification on apply functions
# Recall apply functions are best practice in R in lieu of for-loops

?apply
# apply(X, MARGIN, FUN)
# Here:
#   -x: an array or matrix
# -MARGIN:  take a value or range between 1 and 2 to define where to apply the function:
#   -MARGIN=1`: the manipulation is performed on rows
# -MARGIN=2`: the manipulation is performed on columns
# -MARGIN=c(1,2)` the manipulation is performed on rows and columns
# -FUN: tells which function to apply. Built functions like mean, median, 
# sum, min, max and even user-defined functions can be applied>


m1 <- matrix(C<-(1:10),nrow=5, ncol=6)
m1
a_m1 <- apply(m1, 2, sum)
a_m1

?lapply
# lapply is list-apply
# Does not use a margin
# Will return a list
movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower <-lapply(movies, tolower)
str(movies_lower)

# Can use unlist to convert back to a vector
movies_lower <-unlist(lapply(movies,tolower))
str(movies_lower)

# OR, can you sapply, which works like lapply but returns a vector
# very common use case
dt <- cars
lmn_cars <- lapply(dt, min)
smn_cars <- sapply(dt, min)
lmn_cars
smn_cars

lmxcars <- lapply(dt, max)
smxcars <- sapply(dt, max)
lmxcars
smxcars




# mapply is multvariate apply; frequently used as a way of vectorizing
# other functions
# Notice how Q1 and Q2 are the same
# Create a 4x4 matrix
?mapply
# Note how the ... represents arguments to vectorize over
# MoreArgs can be used in teh case where FUN takes additional arguments
Q1 <- matrix(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)),4,4)

# Print `Q1`
print(Q1)

# Or use `mapply()`
Q2 <- mapply(rep,1:4,4)

# Print `Q2`
print(Q2)


# tapply allows you to apply a function, but it will group by the 
# index column
# tapply(X, INDEX, FUN = NULL)
# Arguments:
#   -X: An object, usually a vector
# -INDEX: A list containing factor
# -FUN: Function applied to each element of x

data(iris)
head(iris)
tapply(X=iris$Sepal.Width, INDEX=iris$Species, FUN=median)

# Let's create a new column to illustrate how we can apply to multiple columns
sizeBin <- function(x){
  if (x<=.3) {
    return("small")
  } else if(x<=1.8){
    return("medium")
  } else {
    return("large")
  }
}

iris[,'petalBin'] <- sapply(X=iris$Petal.Width, FUN=sizeBin)

r <- tapply(X=iris$Sepal.Width, INDEX=list(iris$Species, iris$petalBin), FUN=median)
r
str(r)


