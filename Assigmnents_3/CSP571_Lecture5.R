#### CSP571
#### Lecture 5 R code

# Recall the insurnace data set
custdata <- read.table('https://raw.githubusercontent.com/WinVector/zmPDSwR/master/Custdata/custdata.tsv',
                       header=TRUE,sep='\t')

head(custdata)
summary(custdata)

library(ggplot2)

# Let's look at the relationship between income and health insurance
ggplot(custdata, aes(x = income, y = as.numeric(health.ins))) + geom_point() + geom_jitter(height = .1)+ stat_smooth()+ xlim(0, 100000)

# Notice how we "jitter" the data to better display overlapping values
# We see that slicing income at roughly 12500, 40000, 50000 might make sense

breaks <- c(0, 12500, 40000, 50000, Inf)

# We can use the cut function to quickly bin data
?cut
custdata$incomeBinned <- cut(custdata$income, breaks = breaks, include.lowest = TRUE)
str(custdata$incomeBinned)
levels(custdata$incomeBinned)


# Let's look at income on its own
ggplot(custdata, aes(x = income)) + geom_density()

# We see that its highly skewed. Lets apply a log transformation
custdata$incomeLog <- log(custdata$income, base=10)

ggplot(custdata, aes(x = incomeLog)) + geom_density()

# Notice that we are losing values <1. Log is not defined for those values.
# We can use the signed log transform to deal with this.
signedLog <- function(x){
  ifelse(abs(x) <= 1, 0, sign(x) * log10(abs(x)))
}
signedLog(-1)
custdata$incomeSignedLog <- sapply(custdata$income, FUN = signedLog)

ggplot(custdata, aes(x = incomeSignedLog)) + geom_density()




# Sampling data

# One way to implement sampling is to generate a uniform random number from 0-1
# Then if each number is less than our desired percentage for that group, allocate
# it to that group.
trainPct <- .8
testPct <- 1 - trainPct

# Generate a some random numbers between 0 and 1
# Always set a seed for reproducibility!!
set.seed(34543)
randVect <- runif(n = nrow(custdata), min = 0, max = 1)
# Convert to data.frame so we can plot in ggplot2
randDf <- as.data.frame(randVect)
ggplot(randDf, aes(x = randVect)) + geom_histogram()
trainGroup <- randVect<trainPct
testGroup <- !trainGroup
custTrain <- custdata[trainGroup,]
custTest <- custdata[testGroup,]
# Always check key assumptions like below!!!
stopifnot(nrow(custTrain) + nrow(custTest) == nrow(custdata))
nrow(custTrain)
nrow(custTest)
nrow(custdata)
# Note that the perecentages will not be exact, but as N gets large, they
# will be very close
nrow(custTrain)/nrow(custdata)
nrow(custTest)/nrow(custdata)


# Better way to actually implement this
custSample <- sample(custdata$custid, size = trainPct * nrow(custdata), replace = FALSE)
custTrain <- custdata[custdata$custid %in% custSample, ]
custTest <- custdata[!custdata$custid %in% custSample, ]
# Always check key assumptions like below!!!
stopifnot(nrow(custTrain) + nrow(custTest) == nrow(custdata))
nrow(custTrain)
nrow(custTest)
nrow(custdata)

# How different is the train and test data, with respect to health.ins
sum(custTrain$health.ins)/nrow(custTrain)
sum(custTest$health.ins)/nrow(custTest)


# Implement stratified sampling
library('caret')
inTrain <- createDataPartition(y = custdata$health.ins, p = trainPct, list = FALSE)
custTrain2 <- custdata[inTrain,]
custTest2 <- custdata[-inTrain,]
stopifnot(nrow(custTrain2) + nrow(custTest2) == nrow(custdata))

# How different is the train and test data, with respect to health.ins
# using stratified sampling? Should be much less.
sum(custTrain2$health.ins)/nrow(custTrain2)
sum(custTest2$health.ins)/nrow(custTest2)


################################################################
# Dealing with categorical variables
################################################################

# R uses the factor data type to store both categorical and ordinal variables
# Caution! There are some gotcha's!

# One issue: R does not always correctly "guess" the data type
# This issue can arrise many ways:
# Numeric variables in strings in R
dat <- c("1", "2", "2.5", "3.153", "4")
str(dat)

# When importing from an external file
# Recall in the lecture where we talked about importing csv data
# My advice, stringsAsFactors = FALSE then manually specify 

# Another example where R gets tripped up
otherDat <- c(1, 2, 3, 4, "NA", '')
str(otherDat)

# Let's explore how R deals with categorical and factor vars
dat <- sample(0:1, 20, replace = TRUE)
dat
# Check if factor
str(dat) 
is.factor(dat)
is.numeric(dat)

# R's categorical and ordinal data
facDat <- as.factor(dat)
str(facDat)
facDat

# Let's say that we knew that 0 is left handed and 1 is right handed
# Best practice would be to encode that info for readability, interpretation, etc
myLabels <- c('left', 'right')
facDat <- factor(dat, labels = myLabels)
dat[1:5]
facDat[1:5]
is.factor(facDat)

# What if we made a mistake? Actually 1 is left handed, 0 right handed
# Note we use the levels() function here (not labels: GOTCHA!)
factDatNew <- facDat
levels(factDatNew) <- c('right', 'left')
factDatNew[1:5]
facDat[1:5]
dat[1:5]

# What if we had a vector of characters, that actually was
# categorical. 
charDat <- c("low", "middle", "low", "low", "low", "low", "middle", "low", "middle",
         "middle", "middle", "middle", "middle", "high", "high", "low", "middle",
         "middle", "low", "high")

is.factor(charDat)

# Maybe we'll just convert it to factor
charDatBad <- as.factor(charDat)
charDatBad[1:5]
charDat[1:5]
levels(charDatBad)

# Let's are out of order. By default, R uses alphabetical order.
charDatNew <- factor(charDat, levels = c('low', 'medium', 'high'))
charDatNew[1:5]
charDat[1:5]
levels(charDatNew)

# Okay, but isn't low, medium and high actually ordinal? 
# Yes! Great point.
ordDat <- ordered(charDat, levels = c('low', 'middle', 'high'))
str(ordDat)
ordDat[1:5]
charDat[1:5]
levels(ordDat)

# If you try to add a new element to a factor or ordered factor that is not a level
# you get problems
ordDat[5]
ordDat[5] <- 'really high'
# Gives an NA! 
# Gotcha!! (at least error message)
ordDat[5]

# First we need to add the level
levels(ordDat) <- c(levels(ordDat), 'really high')
levels(ordDat)
# Now we can assign
ordDat[5] <- 'really high'
ordDat[5]




# What if we want to get back the numeric values? 
testDat <- sample(c(0, 5, 6), 10, replace = TRUE)
testDat
testDat2 <- factor(testDat)
testDat2

as.numeric(testDat2)
# Doesnt work!
# A few ways of doing this; easiest to remember (but not fastest)
as.numeric(as.character(testDat2))



# Let's go back to otherDat
otherDat
# We want to make this a numeric variable
# Let's try this:
as.numeric(otherDat)


# Wont work. 
# A few ways to do this
# First replace '' and "NA"
otherDat2 <- replace(otherDat, otherDat ==''|otherDat =='NA', NA)
otherDat2
otherDat2 <- as.numeric(otherDat2)




################################################################
# Geographic data 
################################################################
# Modified and extended from: http://spatial.ly/wp-content/uploads/2013/12/intro-spatial-rl-3.pdf
# Need a bunch of packages; This takes awhile.
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
#install.packages(x) # warning: uncommenting this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages

# Download data locally: https://github.com/Robinlovelace/Creating-maps-in-R
# Too big to do in code. 

# Let's read in the data
# Data is in the form of a shapefile
# Data is population of London Boroughs in 2001 and the percentage
# of the population participating in sporting activities
lnd <- readOGR(dsn='C:/Users/amcelhinney/Google Drive/IIT/CSP_571_SP2020/Creating-maps-in-R-master/data', layer='london_sport')

# What type of data do we have? 
class(lnd)

# This data type is made of a number of different "slots"
# Slots are denoted by the @ symbol
# Key slots are typically @data (used for attributes 
# and @polygons, @lines (depending on data set)
head(lnd@data)
head(lnd@polygons, n=1)


# We can slice and dice the data like we normally do, just use the @ symbol
hist(lnd@data[,'Partic_Per'])
sum(lnd@data[,'Partic_Per']<15)


# What if we just use base R to plot this
plot(lnd)
# Works! Not the prettiest, but we'll revisit.

# Plots are cummulative, so we can slice and dice to highlight different areas. 
# Let's highlight areas with lots of sports participation
plot(lnd, col = "lightgrey") # plot the london_sport object
sel <- lnd$Partic_Per > 25
plot(lnd[ sel, ], col = "turquoise", add = TRUE) # add selected zones to map


# There are powerful built in features. For example, it looks like most sports participation
# is in the center of the city. What if we want to find the center of lond and circle everything within
# 10 km of that.
plot(lnd, col = "grey")
# find London's geographic centroid (add ", byid = T" for all)
cent_lnd = gCentroid(lnd[lnd$name == "City of London",], byid = FALSE) 
points(cent_lnd, cex = 3)
# set 10 km buffer
lnd_buffer = gBuffer(spgeom = cent_lnd, width = 10000) 
sel <- lnd$Partic_Per > 25
plot(lnd[ sel, ], col = "turquoise", add = TRUE)



# Let's shade the map based on the population
library(tmap)
qtm(lnd, fill='Pop_2001')

# That's not right. What's going on?
str(lnd@data)



# We need to do some work
lnd@data$Pop_2001 <- as.numeric(as.character(lnd@data$Pop_2001))



# Let's make this blue
qtm(lnd, fill='Pop_2001', fill.palette="-Blues")
# Note the qtm function is a GREAT wrapper that saves you a lot of complicated steps
# if you try to implement in ggplot or other package

# Let's mess around with some GPS data.
# Let's get a list of all airports and their lat lons.
airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
                     , header = FALSE)
colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO"
                        , "lat", "lon", "altitude", "timezone", "DST")
head(airports)

# We don't need the last there columns
table(airports[,ncol(airports)])
table(airports[,ncol(airports)-1])

# Drop last 3 columns
airports[,(ncol(airports)-2):ncol(airports)] <- NULL

head(airports)


# Let's get a map to plot with
library(rworldmap)

map <- getMap(resolution='low')
plot(map)
points(airports$lon, airports$lat, col = 'blue', cex=.1)


# Let's find Ohare
ord <- airports[airports$IATA_FAA=='ORD',]
head(ord)

# Let's compute the distance of every airport to Ohare
#install.packages('geosphere')
library(geosphere)
?distm
distances <- t(distm(x=c(ord$lon, ord$lat), y=airports[,c('lon', 'lat')], distHaversine))
airports[,'ordDist'] <- distances
sortedAirports <- airports[order(airports$ordDist),]
sortedAirports[1:10,]
sortedAirports[(nrow(airports)-10):nrow(airports),]

?distHaversine
?distVincentyEllipsoid




# Let's plot all of the closest airports
closeAirports <- sortedAirports[1:10,]
#install.packages('maps')
library('maps') 
#library('ggmaps')
library(leaflet)
map('state', 'illinois')
points(closeAirports$lon, closeAirports$lat, col = 'blue', cex=.6)


