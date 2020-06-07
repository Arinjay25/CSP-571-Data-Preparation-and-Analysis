# CSP/MATH 571 Homework 2 Part 1
# Enter your email below
yourEmail<-'ajain80@hawk.iit.edu'

# Load in the Boston Housing data set using the code below.
install.packages('mlbench')
library('mlbench')
data(BostonHousing)


# 1. Create a scatterplot matrix of all variables in the data set. Save your output.

pairs(BostonHousing[,1:14], pch = 19)



# 2. For each numeric variable in BostonHousing, create a separate boxplot using
# "Method 2" listed in the class notes. Do this programmatically; meaning do
# not simply hardcode the creation of every boxplot. Instead, loop over the
# approriate columns and create the boxplots. Save your output. Ensure your boxplots
# all have proper titles

#find all columns types
coltype<-sapply(BostonHousing, class)

for ( i in (1:ncol(BostonHousing))){
  if ("numeric" == coltype[i]){
    #box plot of each columns
    boxplot(BostonHousing[i], main =paste("Box Plot",names(BostonHousing[i])), ylab = names(BostonHousing[i]), las=1)
  }
}

# 3. Create a correlation matrix and correlation plot
# for the BostonHousing data set. Save your output.
install.packages("corrplot")
library(corrplot)

cor_Mat<-cor(BostonHousing[,sapply(BostonHousing, is.numeric)], method = c("pearson", "kendall", "spearman"))
cor_Mat<- round(cor_Mat,2)
print(cor_Mat)
corrplot(cor_Mat)



# 4. Identify the top 3 strongest absolute correlations in the data set. Save your output.
# because diagonl and half matrix have self correlation cofficient and same correlation cofficient with others
cor_Mat[lower.tri(cor_Mat,diag=TRUE)]<-NA 
cor_Cof<-as.data.frame(as.table(cor_Mat))
#removing NA
cor_Cof<-cor_Cof[complete.cases(cor_Cof),]
cor_Cof<-cor_Cof[order(abs(cor_Cof$Freq),decreasing = TRUE),]
# TOP 3 STRONGEST ABSOLUTE CORRELATION
cor_Cof[1:3,]


# 5. Create a new variable call ageGroup quartiles. Divide the age variable
# into four even sections and assign it to one quartile.
BostonHousing$ageGroup<-NULL
BostonHousing$ageGroup<-cut(BostonHousing$age, breaks = quantile(BostonHousing$age, probs = seq(0, 1, 0.25)), include.lowest = TRUE)
head(BostonHousing)         


# 6. Go to the website listed below. Convert the html table into a
# dataframe with columns NO, Player, Highlights
library('rvest')
library('tidyr')
url = 'http://www.espn.com/nfl/superbowl/history/mvps'
my_df <- as.data.frame(read_html(url) %>% html_table(trim = TRUE, fill=TRUE))
my_df<-my_df[-(1:2),]
names(my_df)<-c('NO', 'Player', 'Highlights')
head(my_df)


# 7.Extract the names of the MVPs, Position and Team into columns
# MVP1, MVP2, Position, Team
my_df<-separate(my_df, Player, c('MVPs', 'Position', 'Team')
                , sep=', ' # We want to split this where the comma is located
                , remove=TRUE)

my_df$MVPs <- ifelse(!grepl('&', my_df$MVPs), paste0(my_df$MVPs,"&"), my_df$MVPs)

my_df<-separate(my_df, MVPs, c('MVP1', 'MVP2')
                , sep='&' # We want to split this where the & is located
                , remove=TRUE)
#my_df <- sapply(my_df, as.character)
#my_df[is.na(my_df)] <- ""
print(my_df[10:15,])

# 8. Determine the 90th%, 92.5th%, 95th%, 97.5th% and 99th% confidence intervals
# for the mean of passing yards
# (as listed in "Highlights" column) for quarterbacks.
# Note that there are a few intermediate steps you'll probably want to do
# in order to accomplish this. I am purposelly leaving that up to you, so that
# you are starting to develop the types of problem solving skills you'll need
# to tackle these problems in the wild.

#quarterbacks = QB in positions.
df_QB <- subset(my_df, Position == 'QB')

split_Higlights<-unlist(strsplit(df_QB$Highlights, " "))
yards_values<-NULL
for (i in 1:length(split_Higlights)){
  if (grepl("yards",split_Higlights[i])){
    yards_values<-append(yards_values,split_Higlights[i-1])
  }
}
print(yards_values)
# confidence intervals
t.test(as.numeric(yards_values), conf.level = 0.9)
t.test(as.numeric(yards_values), conf.level = 0.925)
t.test(as.numeric(yards_values), conf.level = 0.95)
t.test(as.numeric(yards_values), conf.level = 0.975)
t.test(as.numeric(yards_values), conf.level = 0.99)


# 9. The following contains data on the calorie counts of four types
# of foods. Perform an ANOVA and determine the Pr(>F)
food1 <- c(164,   172,   168,   177, 	156, 	195)
food2 <- c(178,   191, 	197, 	182, 	185, 	177)
food3 <- c(175, 	193, 	178, 	171, 	163, 	176)
food4 <- c(155, 	166, 	149, 	164, 	170, 	168)

food_df <- data.frame(food1,food2,food3,food4)
food_df <- stack(food_df)
anova<-aov(food_df$values ~ food_df$ind, food_df)
print(anova)
summary(anova)


#value<-NULL
#type<-NULL
#for (j in 1:4){
#  for (i in 1:6){
#  value<-append(value,eval(parse(text =paste("food",j,sep ="")))[i])
#  type<-append(type,paste("food",j,sep =""))
#  }
#}
#foodDF<-data.frame(value,type)

# 10. Determine how many
# Tuesdays fell on the first of the month
# during the 19th century (1 Jan 1801 to 31 Dec 1901).
install.packages("lubridate")
library(lubridate)
beginDate<-as.Date("1801-01-01")
endDate<-as.Date("1901-12-31")
sum(wday(seq(beginDate,endDate,"months"), label = TRUE) == "Tue")

