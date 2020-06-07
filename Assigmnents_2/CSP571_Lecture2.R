# CSP571_Lecture_2

# Adapted from Zumel companion code
uciCar <- read.table(    # Note: 1
  'http://www.win-vector.com/dfiles/car.data.csv', 	# Note: 2
  sep=',', 	# CAUTION when using commas as the delimiter on text data
  header=T 	# Note: 4
)

class(uciCar)

summary(uciCar)

dim(uciCar)

# Another example

d <- read.table(paste('http://archive.ics.uci.edu/ml/',
                      'machine-learning-databases/statlog/german/german.data'
                      ,sep=''),
                # Best practice is to set this to FALSE and explicitly define
                # your factor variables in the code
                stringsAsFactors=F
                ,header=F)
print(d[1:3,])


# What types of variables are there in this data set?
# install.packages("MASS")
library(MASS)
head(survey)
# Data dictionary
# https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/survey.html
summary(survey)
# Identify the discrete and continuous variables
# Categorize each variable accodring to their scale:
# categorical, ordinal, interval, ratio
# Are there any dichotomous variables?
# Are there are binary variables?
# Are there any primary keys?
# Construct one of each from the data set


# Computing confidence intervals in R
# Adapted from http://www.r-tutor.com/elementary-statistics/interval-estimation/interval-estimate-population-mean-unknown-variance
height.response <- na.omit(survey$Height)


n <- length(height.response)
s <- sd(height.response)
SE <- s/sqrt(n)
alpha <- .005

# Need to figure out z/alpha/2
zVal <- qnorm(p = 1-alpha/2)

E <- zVal*SE
xbar = mean(height.response)

# Notice how we can leverage the vector operations for easy creation of the confidence interval
xbar+c(-E,E)

# Obtaining data from web sources
# Webscraping (very basic introduction)
install.packages('rvest') # Webscraping package
install.packages('stringr') # Package that makes manipulating strings (text) easier
install.packages('tidyr') # Package for making data manipulation easier

library(rvest)
library(stringr)
library(tidyr)

# Let's take a look at this webpage and how html tables are constructed
url <- 'http://espn.go.com/nfl/superbowl/history/winners'
webpage <- read_html(url)
# How html tables are constructed: http://www.w3schools.com/html/html_tables.asp

sb_table <- html_nodes(webpage, css = 'table')
str(sb_table)
t <- sb_table[[1]]

# We want to read this hmtl table and convert it into a data frame
sb <- html_table(sb_table)[[1]]
head(sb)

# Note that the X1... is garbage, so we want to remove that
# Also it just repeads "Super Bowl Winners..." so we also want to remove that
sb <- sb[-(1:2), ]
# Let's assign the right names
names(sb) <- c("number", "date", "site", "result")
head(sb)


# The dates are not formatted in an easy to use way. Let's convert them

sb$date <- as.Date(sb$date, "%B. %d, %Y")
head(sb)


# The Super Bowl number is listed as a Roman numeral. That's annoying.
# Let's replace that.
sb$number <- 1:nrow(sb)

# Let's split the result into the winners and losers
sb <- separate(sb, result, c('winner', 'loser')
               , sep=', ' # We want to split this where the comma is located
               , remove=TRUE)
head(sb)

# We want to extract the scores from the end of winner and loser
# We use a reguluar expression (denoted as the pattern variable)
# This says to extract one or more numbers before the end of a line
# Regular expressions are not just an R concept, they are widely used in all
# major programming languages.
pattern <- " \\d+$"
sb$winnerScore <- as.numeric(str_extract(sb$winner, pattern))
sb$loserScore <- as.numeric(str_extract(sb$loser, pattern))
sb$winner <- gsub(pattern, "", sb$winner)
sb$loser <- gsub(pattern, "", sb$loser)
head(sb)


### Reading in Excel data
# install.packages('readxl')
# https://drive.google.com/open?id=1fbCywWCuiatUer-aUW3vt0ugwgrmQ0iG
library('readxl')
file <- '/Users/amcelhinney/Google Drive/IIT/CSP 571 Spring 2019/Course Materials/CSP_571_Lecture2_Superstore.xls'
Orders <- read_excel(file, sheet = "Orders")
Returns <- read_excel(file, sheet = "Returns")
People <- read_excel(file, sheet = "People")

head(Orders)
head(Returns)
head(People)

# Notice by default it reads it in as a class. We probably want to convert to a data.frame. How can we do this?


Orders <- as.data.frame(Orders)
Returns <- as.data.frame((Returns))
People <- as.data.frame((People))


## How many orders were there to postal code 90049?

sum(Orders[,'Postal Code'] == 90049)

## How concentrated geographically are our sales? For example, what percentage of sales 
## were in the top ten postal codes?

# First, let's get the number of orders per postal
ordersPerPostal <- table(Orders[,'Postal Code'])

# Let's sort this descending
ordersPerPostal <- sort(ordersPerPostal, decreasing = TRUE)

# Let's take the top ten
topTen <- ordersPerPostal[1:10]

# Sum those orders
topTenSum <- sum(topTen)

# Divide by total number of orders
topTenSum/nrow(Orders)

# As a one-liner
sum(sort(table(Orders[,'Postal Code']), decreasing = TRUE)[1:10])/nrow(Orders)


## Set operations in R
## Note this is not guarenteed to work the same way in SQL, Python or other languages
## This example is modified from the Python Pandas example. Interesting to compare and contrast how
## the two toolsets differ.
## https://pandas.pydata.org/pandas-docs/stable/merging.html#database-style-dataframe-joining-merging

# Before we get started
# Let's look at how we might create a test data set like in the slides
?data.frame

# Why am I writing a function to do this?
genTestVec <- function(prefix, start=0, n) {
  return(paste0(rep(prefix, n), c(start:(n+start))))
}
N = 3 

m <- 0
left = data.frame(genTestVec('A', start=m, n=N)
                 , genTestVec('B', start=m, n=N)
                 , genTestVec('K', start=m, n=N)
                 , row.names = as.character(0:(N+m)) # What is going on here?
)
colnames(left) <- c("A", "B", "key")

N=2
right = data.frame(genTestVec('C', start=m, n=N)
                  , genTestVec('D', start=m, n=N)
                  , genTestVec('K', start=m, n=N)
                  , row.names = as.character(0:(N+m)) # What is going on here?
)
colnames(right) <- c("C", "D", "key")

head(left)
head(right)

# Join operations
?merge

# Inner join
# How many rows will be returned?
merge(x=left, y=right, by ="key")

# Left join
# How many rows will be returned?
merge(x=left, y=right, by ="key", all.x = TRUE)

# Right join
# How many rows will be returned?
merge(x=left, y=right, by ="key", all.y = TRUE)


# Outer join
# How many rows will be returned?
merge(x=left, y=right, by ="key", all = TRUE)

### Concatenate dataframe in R



m <- 0
df1 = data.frame(genTestVec('A', start=m, n=N)
                 , genTestVec('B', start=m, n=N)
                 , genTestVec('C', start=m, n=N)
                 , genTestVec('D', start=m, n=N)
                 , row.names = as.character(0:(N+m)) # What is going on here?
                    )
colnames(df1) <- c("A", "B", "C", "D")

m <- 4
df2 = data.frame(genTestVec('A', start=m, n=N)
                 , genTestVec('B', start=m, n=N)
                 , genTestVec('C', start=m, n=N)
                 , genTestVec('D', start=m, n=N)
                 , row.names = as.character(m:(N+m)) 
)
colnames(df2) <- c("A", "B", "C", "D")

m<- 8
df3 = data.frame(genTestVec('A', start=m, n=N)
                 , genTestVec('B', start=m, n=N)
                 , genTestVec('C', start=m, n=N)
                 , genTestVec('D', start=m, n=N)
                 , row.names = as.character(m:(N+m))
)
colnames(df3) <- c("A", "B", "C", "D")

head(df1)
head(df2)
head(df3)

# Concatenate the rows
rbind(df1, df2, df3)


## What happens if the data.frames do not share all the same columns?

m <- 0
df1 = data.frame(genTestVec('A', start=m, n=N)
                 , genTestVec('B', start=m, n=N)
                 , genTestVec('C', start=m, n=N)
                 , genTestVec('D', start=m, n=N)
                 , row.names = as.character(0:(N+m)) # What is going on here?
)
colnames(df1) <- c("A", "B", "C", "D")

m <- 4
df2 = data.frame(genTestVec('A', start=m, n=N)
                 , genTestVec('E', start=m, n=N)
                 , row.names = as.character(m:(N+m)) 
)
colnames(df2) <- c("A", "E")

rbind(df1, df2) # Why does this fail? Note this is NOT the same in many languages. Caution!!!!!
?rbind

# Two options:
# 1. Create the columns with nulls
# 2. Use another package that will fill this for us

# Option 1
df1[, "E"] <- rep(NA,nrow(df1))
df2[, c("B", "C", "D")] <- c(rep(NA,nrow(df2)), rep(NA,nrow(df2)), rep(NA,nrow(df2)))
head(df1)
head(df2)

rbind(df1, df2)

# Option 2
# plyr package can help
library('plyr')
m<-0
df1 = data.frame(genTestVec('A', start=m, n=N)
                 , genTestVec('B', start=m, n=N)
                 , genTestVec('C', start=m, n=N)
                 , genTestVec('D', start=m, n=N)
                 , row.names = as.character(0:(N+m)) # What is going on here?
)
colnames(df1) <- c("A", "B", "C", "D")

m <- 4
df2 = data.frame(genTestVec('A', start=m, n=N)
                 , genTestVec('E', start=m, n=N)
                 , row.names = as.character(m:(N+m)) 
)
colnames(df2) <- c("A", "E")
head(df1)
head(df2)

?rbind.fill
rbind.fill(df1,df2)


##### Munging data
## Let's go back to Excel sheet

head(Orders)
head(Returns)
head(People)
# You are tasked to calculate the profit by region, net of returns
# If an item is returned, the profit is zero BEFORE a restocking fee is applied.
# The restocking fee is PER unit. 
# Different regions have restocking fees as listed below:
# West: $5 + 2% of items value
# East: $15
# Central: $2.5 + 5% of items value
# South: $11.35
# Verbally describe plan we'll need to do in order to conduct this analysis, including the set operations.




# Step 1: Add to the People table, the flat fee and percentage return cost
People[, 'returnFee'] <- c(5, 15, 2.5, 11.35) 
People[, 'returnPct'] <- c(.02, 0, .05, 0)

head(People)

# Step 2: Calculate the unit cost for each order

Orders[, 'unitCost'] <- Orders[, 'Sales']/Orders[,'Quantity'] # What is potentially "fragile" about this line?
head(Orders)


# Step 3: Determine which orders were returned. What join type do we need?
OrdersNew <- merge(x=Orders, y=Returns, on='Order ID', all.x = TRUE)

# How can we verify step 3 worked as expected?
stopifnot(nrow(OrdersNew) == nrow(Orders))

stopifnot(sum(OrdersNew[,'Returned'] == "Yes", na.rm=TRUE) == nrow(Returns))
# WAIT!? What happened above?

sort(table(Returns[,'Order ID']), decreasing = TRUE)[1:5]
sort(table(Orders[,'Order ID']), decreasing = TRUE)[1:5]

# We see that Order ID is NOT a primary key in the Orders table.
# That is okay. We can still verify that very return in the Returns table is in the Orders table.
# Why are we subtracting 1?
stopifnot(length(unique(OrdersNew[OrdersNew[,'Returned'] == "Yes", 'Order ID'])) -1 == nrow(Returns))


# Step 4: Join the return costs for a given region from the People table. What type of join do we want?
OrdersNew <- merge(x=OrdersNew, y=People, on ='Region', all.x = TRUE)

# Let's confirm the join worked as intended
table(OrdersNew[,c("returnFee", "returnPct")], useNA = 'always')


# Step 5: Write a function that calculates the return cost for a given row
# in the OrdersNew table
head(OrdersNew)

returnCost <- function(Returned, unitCost, Quantity, returnFee, returnPct){
  if(Returned == "Yes"){
    returnCost <- Quantity*returnFee + Quantity*unitCost*returnPct
  } else {
    returnCost<-0
    }
  return(returnCost)
  }

# ALWAYS test your functions
Returned <- 'Yes'
unitCost <- 10
returnFee <- 1
returnPct <- .1
Profit <- 10
Quantity <- 2
# This item will have $2 in return fees and $2 in returnPct, so $4 total
stopifnot(returnCost(Returned, unitCost, Quantity, returnFee, returnPct)==4)

Returned <- 'NO'
unitCost <- 10
returnFee <- 1
returnPct <- .1
Profit <- 10
Quantity <- 2
stopifnot(returnCost(Returned, unitCost, Quantity, returnFee, returnPct)==0)


Returned <- NA
unitCost <- 10
returnFee <- 1
returnPct <- .1
Profit <- 10
Quantity <- 2
stopifnot(returnCost(Returned, unitCost, Quantity, returnFee, returnPct)==0)
# WHAT HAPPEND?!

NA == TRUE
NA == FALSE
# let's modify our function to be more robust
returnCost <- function(Returned, unitCost, Quantity, returnFee, returnPct){
  if(is.na(Returned)){
    Returned <- "no"
  }
  if(Returned == "Yes"){
    returnCost <- Quantity*returnFee + Quantity*unitCost*returnPct
  } else {
    returnCost<-0
  }
  return(returnCost)
}

# Retest our functions
Returned <- 'Yes'
unitCost <- 10
returnFee <- 1
returnPct <- .1
Profit <- 10
Quantity <- 2
# This item will have $2 in return fees and $2 in returnPct, so $4 total
stopifnot(returnCost(Returned, unitCost, Quantity, returnFee, returnPct)==4)

Returned <- 'NO'
unitCost <- 10
returnFee <- 1
returnPct <- .1
Profit <- 10
Quantity <- 2
stopifnot(returnCost(Returned, unitCost, Quantity, returnFee, returnPct)==0)


Returned <- NA
unitCost <- 10
returnFee <- 1
returnPct <- .1
Profit <- 10
Quantity <- 2
stopifnot(returnCost(Returned, unitCost, Quantity, returnFee, returnPct)==0)

# Step 6: Apply the function to each row
# We should use a for-loop, right?!


?apply
table(apply(X=OrdersNew, FUN = returnCost, MARGIN = 1)) # Why?

# Across Python and R, tricky to apply functions that have multiple inputs
returnCostCalc<- mapply(FUN=returnCost, OrdersNew[,'Returned'], OrdersNew[,'unitCost'], OrdersNew[,'Quantity']
       , OrdersNew[,'returnFee']
       , OrdersNew[,'returnPct'])

OrdersNew[,'returnCost'] <- returnCostCalc

table(OrdersNew[,'returnCost'])

# Step 7: Calculate netProfit
OrdersNew[,'netProfit'] <- OrdersNew['Profit'] - OrdersNew['returnCost']
summary(OrdersNew)

# Step 8: Sum up the netProfit per region
# NOTE: This is NOT the right way to do this. On hw1, I hint you to a better way to do this.
# If you do this on the homework, you will not receive credit
regions <- People[,'Region']
netProfitList <- list()
for(region in regions){
  netProfit <- sum(OrdersNew[OrdersNew[,'Region'] == region, 'netProfit'])
  netProfitList[region] <- netProfit
}

netProfitList


################################################################################ 
##### Dates and times in R
#### Dates
### See slides on dates and times in R
# We previously saw how to create a date from a string
myDate <- as.Date("2019-01-01")

weekdays(myDate)
months(myDate)
quarters(myDate)
# We can use format to extract the months or years or other components
format(myDate, "%y")
# How might we get the four digit year?


format(myDate, "%Y")
# What if we wanted to get months?

format(myDate, "%y-%m")
# Your accounting system requires dates formatted as YYYY/MM/DD
# What would we do?

format(myDate, "%Y/%m/%d")

# How to get today's date?
Sys.Date()

# We can do date month just with normal operations, knowing that the result is in days
days<- Sys.Date() - myDate
days
class(days) # What is the format of this?


# Going back to our orders data set, let's get summary statistics on 
# the number of days it took to fullfill an order. 
# How might we do this?
head(Orders)

# Let's compute the amount of days between the ship date and the order date
Orders[,'Ship Date'] - Orders[,'Order Date']


# Whats wrong with this?

class(Orders[,'Ship Date'])


# Looks like they are stored as a time value
# What should we do?


# Convert to date

OrderDate <- as.Date(Orders[,'Order Date'])
ShipDate <-  as.Date(Orders[,'Ship Date'])
daysToFullfill <- ShipDate - OrderDate


summary(daysToFullfill)

# Why is this not working?


class(daysToFullfill[1])



daysToFullfillInt <- as.integer(daysToFullfill)


summary(daysToFullfillInt)


# What is the distributio of orders placed for a given day of the week?
orderDist <- table(weekdays((OrderDate)))

# Want to order this in our expected order

weekdayOrder <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

?match # What does the match function do? Can it help us?

orderDistProper <- orderDist[match(weekdayOrder, names(orderDist))]
# Unpack what's going on there

# We want the distribution of orders, so we want this a percentage
orderDistProper/sum(orderDistProper)


############
# Times in R
# R uses the POSIXlt or POSIXct functions

rightNow <- Sys.time()
rightNow
class(rightNow)

?POSIXct
# Notice there are elements for sec, min, hour, etc
# What's odd about seconds?


?Sys.time

# Notice the varying accuracy depending on operating system.
# Use caution when extremely high time accuracy required

# Convert the time back to a list and unpack
timeList <- unclass(as.POSIXlt(rightNow))
class(timeList)
names(timeList)

# What is my current time zone? Operating system
timeList$zone

# What hour of the day is it?

timeList$hour

# What is this doing?

rightNow + 10


# Adding seconds. Recall seconds are the units used in POSIX time

# TODO: Create a question where people have to write a function that computes the number of days until their birthday
# TODO: Week of the month
# TODO: Have some time math in the homework
# TODO: Recode those "gotchas" in Evernote into homework
# Doctor wants you to come back for check up at least 48 hours after today but not on weeks and between a certain time