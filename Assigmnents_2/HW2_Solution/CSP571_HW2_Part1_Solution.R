# CSP/MATH 571 Homework 2 Part 1
# Enter your email below
yourEmail<-'kgunasekaran@hawk.iit.edu'

# Load in the Boston Housing data set using the code below.
#install.packages('mlbench')
library('mlbench')
data(BostonHousing)
housing <- BostonHousing
summary(BostonHousing)
# 1. Create a scatterplot matrix of all variables in the data set. Save your output.

pairs(BostonHousing)


# 2. For each numeric variable in BostonHousing, create a separate boxplot using
# "Method 2" listed in the class notes. Do this programmatically; meaning do
# not simply hardcode the creation of every boxplot. Instead, loop over the
# approriate columns and create the boxplots. Save your output. Ensure your boxplots
# all have proper titles
#print(housing)
numericCols <- BostonHousing[, sapply(BostonHousing, is.numeric)]
print(numericCols)
for(name in names(numericCols)){
  png(paste(name,'_box.png'))
  boxplot(BostonHousing[, name], main = name)
  dev.off()
}



# 3. Create a correlation matrix and correlation plot
# for the BostonHousing data set. Save your output.

library('corrplot')
numericColumns <- sapply(BostonHousing, is.numeric)
#print(numericColumns)
corMatrix <- cor(BostonHousing[, numericColumns])
corrplot(corMatrix, method = "circle", diag = TRUE)

# 4. Identify the top 3 strongest absolute correlations in the data set. Save your output.

corMatrix[lower.tri(corMatrix,diag=TRUE)]=NA
#corrplot(corMatrix, method = "circle", diag = TRUE)
corMatrix=as.data.frame(as.table(corMatrix))
corMatrix=na.omit(corMatrix)
corMatrix=corMatrix[order(-abs(corMatrix$Freq)),]
corMatrix[1:3,]

#Output
# Var1 Var2       Freq
# 112   rad  tax  0.9102282
# 82    nox  dis -0.7692301
# 42  indus  nox  0.7636514

# 5. Create a new variable call ageGroup quartiles. Divide the age variable
# into four even sections and assign it to one quartile.

BostonHousing$ageGroup <- cut(BostonHousing$age, breaks = quantile(BostonHousing$age, probs = seq(0, 1, 0.25)),
                              include.lowest = TRUE)



# 6. Go to the website listed below. Convert the html table into a
# dataframe with columns NO, Player, Highlights
library('rvest')
library('tidyr')
url = 'http://www.espn.com/nfl/superbowl/history/mvps'
webpage_data <- read_html(url)
#print(webpage_data)
webpage_data_table <- html_nodes(webpage_data, css = 'table')
#print(webpage_data)
webpage_data_table <- html_table(webpage_data_table)[[1]]
webpage_data_table <- webpage_data_table[-(1:2), ]
names(webpage_data_table) <- c("NO", "Player", "Highlights")
#print(webpage_data_table)


# 7.Extract the names of the MVPs, Position and Team into columns
# MVP1, MVP2, Position, Team

webpage_data_table <- separate(webpage_data_table, Player, c('MVPs', 'Position', 'Team'), sep=', ', remove=TRUE)
webpage_data_table$MVPs <- ifelse(!grepl("&", webpage_data_table$MVPs), paste0(webpage_data_table$MVPs,"&"), as.character(webpage_data_table$MVPs))
webpage_data_table <- separate(webpage_data_table, MVPs, c('MVP1', 'MVP2'), sep='&', remove=TRUE)
#print(webpage_data_table)
# 8. Determine the 90th%, 92.5th%, 95th%, 97.5th% and 99th% confidence intervals
# for the mean of passing yards
# (as listed in "Highlights" column) for quarterbacks.
# Note that there are a few intermediate steps you'll probably want to do
# in order to accomplish this. I am purposelly leaving that up to you, so that
# you are starting to develop the types of problem solving skills you'll need
# to tackle these problems in the wild.

yards <- c()
i=0
QB_data <- subset(webpage_data_table, Position =='QB')
for(d in QB_data$Highlights){
  if(grepl("yards passing", d)){
    i <- i+1
    yards[i] <- sapply(strsplit(d, " yards passing"),head,1)
  }
}
#print(yards)
t.test(as.numeric(yards), conf.level = 0.9)
t.test(as.numeric(yards), conf.level = 0.925)
t.test(as.numeric(yards), conf.level = 0.95)
t.test(as.numeric(yards), conf.level = 0.975)
t.test(as.numeric(yards), conf.level = 0.99)
#Output
# 
# One Sample t-test
# 
# data:  as.numeric(yards)
# t = 15.391, df = 20, p-value = 1.496e-12
# alternative hypothesis: true mean is not equal to 0
# 90 percent confidence interval:
#   236.4028 296.0733
# sample estimates:
#   mean of x
# 266.2381
# 
# > t.test(as.numeric(yards), conf.level = 0.925)
# 
# One Sample t-test
# 
# data:  as.numeric(yards)
# t = 15.391, df = 20, p-value = 1.496e-12
# alternative hypothesis: true mean is not equal to 0
# 92.5 percent confidence interval:
#   233.7463 298.7299
# sample estimates:
#   mean of x
# 266.2381
# 
# > t.test(as.numeric(yards), conf.level = 0.95)
# 
# One Sample t-test
# 
# data:  as.numeric(yards)
# t = 15.391, df = 20, p-value = 1.496e-12
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   230.1538 302.3224
# sample estimates:
#   mean of x
# 266.2381
# 
# > t.test(as.numeric(yards), conf.level = 0.975)
# 
# One Sample t-test
# 
# data:  as.numeric(yards)
# t = 15.391, df = 20, p-value = 1.496e-12
# alternative hypothesis: true mean is not equal to 0
# 97.5 percent confidence interval:
#   224.3215 308.1547
# sample estimates:
#   mean of x
# 266.2381
# 
# > t.test(as.numeric(yards), conf.level = 0.99)
# 
# One Sample t-test
# 
# data:  as.numeric(yards)
# t = 15.391, df = 20, p-value = 1.496e-12
# alternative hypothesis: true mean is not equal to 0
# 99 percent confidence interval:
#   217.0176 315.4586
# sample estimates:
#   mean of x
# 266.2381
#webpage_data_table <- separate(webpage_data_table, Highlights, c('yards', 'Other highlights'), sep=' yards passing', remove= FALSE)
# webpage_data_table$yards <- as.numeric(webpage_data_table$yards)
# t.test(webpage_data_table$yards, conf.level = .90)
# t.test(webpage_data_table$yards, conf.level = .925)
# t.test(webpage_data_table$yards, conf.level = .95)
# t.test(webpage_data_table$yards, conf.level = .975)
# t.test(webpage_data_table$yards, conf.level = .99)

# 9. The following contains data on the calorie counts of four types
# of foods. Perform an ANOVA and determine the Pr(>F)
food1 <- c(164,   172,   168,   177, 	156, 	195)
food2 <- c(178,   191, 	197, 	182, 	185, 	177)
food3 <- c(175, 	193, 	178, 	171, 	163, 	176)
food4 <- c(155, 	166, 	149, 	164, 	170, 	168)

food_data <- data.frame(food1, food2, food3, food4)


food_data <- stack(food_data)
anova <- aov(values ~ ind, food_data)
summary(anova)
#Output
# Df Sum Sq Mean Sq F value  Pr(>F)   
# ind          3   1636   545.5   5.406 0.00688 **
#   Residuals   20   2018   100.9                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#-----------------------------------------------------------------------
# 10. Determine how many
# Tuesdays fell on the first of the month
# during the 19th century (1 Jan 1801 to 31 Dec 1901).

# we can use the lubridate package for date and ime in R

#install.packages('lubridate')
library('lubridate')
start_date <- as.Date("1801/1/1")
end_date <- as.Date("1901/12/31")
months <- seq(start_date, end_date, "months")
#print(months)
#print(wday(months, label=TRUE))
sum(wday(months, label=TRUE) == "Tue")
#Answer - 173