# CSP571_HW3
# Scenario: 
# You have been tasked with analyzing ACME's Temporary Housing spend for senior management. 
# There is a hypothesis that ACME can reduce costs and increase service levels by issuing an RFP.
# ACME currently has three vendors: ‘Sherlock Homes Llc,’ ‘Keepin It Realty Inc,’ and ‘Raynor Shine Llc.’ 
# Prior to issuing an RFP, ACME requested detailed utilization data from all three vendors in Microsoft Excel 
# with the following fields:
# --> Vendor
# --> Type (Condo, Hotel, etc)
# --> # Days
#	--> Current Adjuster
# --> Move-in/Check-In Date
# --> Daily Housing Rate
# --> Claim Number
# --> Move-out/Check-Out Date
# --> Daily Admin Fee
# --> Policyholder Last Name
# --> Occupancy Status
# --> Total Housing Spend
# --> Policyholder City
# --> # ofBedrooms	

# Read in the data from the ACME Corp Spreadsheet
library('readxl')
file <-"F:/Assigmnents/DPA/Assigmnents_3/ACME_Corp.xlsx"
df <- read_excel(file, sheet = "Sheet1")
df <- as.data.frame(df)

# 2 points
# 1. The three vendors each use a different definition of housing type. However, ACME's official types
# are listed on Sheet2 of the Excel sheet. 
# Create a new column called 'Normalized Housing Type' based on the standardized mapping.
sheet2 <- read_excel(file, sheet = "Sheet2")

df$`Normalized Housing Type` <- sheet2$`Clean Value`[match(df$`Housing Type (Condo, Hotel, Apartment, Single Family Home)`, sheet2$`Lookup Value`)]

head(df)

# 2 points
# 2. Compute the total housing spend by Policy holder State 
# and the total percentage of 
# total housing spend by Policy holder State
# listed in descending order by cost. Return this as a dataframe.

houseSpendPolicyState<-sort(tapply(df$`Total Housing Spend`, INDEX = df$`Policy holder State`, FUN = sum), decreasing = TRUE)
percentageHSpolicy<- houseSpendPolicyState/sum(houseSpendPolicyState)
SpendPolicyState_df<- data.frame(houseSpendPolicyState,percentageHSpolicy)
head(SpendPolicyState_df)

# 2 points
# 3. Create a table that has Normalized Housing Type (NHT) on the y-axis 
# and vendor on the x-axis. At the 
# intersection, compute the 
# 'Total Housing Spend of spend for that given and vendor and NHT. Compute and display 
# on the same table, the row-wise and column-wise margins too.

# Load the library
library(reshape2)
new_df<- data.frame(df$Vendor,df$`Normalized Housing Type`, df$`Total Housing Spend`)
names(new_df)[1]<- "Vendor"
names(new_df)[2]<- "Normalized Housing Type"
names(new_df)[3]<- "Total Housing Spend"

# Cast the library into wide format
table_df <- dcast(new_df, `Normalized Housing Type` ~ Vendor, fun.aggregate = sum, value.var = "Total Housing Spend")
head(table_df)

# 2 points
# 4. Obtain top 20 most frequent Policy holder City and Policy holder State combos
combos<- paste(df$`Policyholder City`, df$`Policy holder State`, sep = ', ')

top20<-head(sort(table(combos), decreasing = TRUE), 20)

print(top20)

# 4 points
# 5. Write a function obtains the lat lon for a given city and state
# Note: You'll propsefully need to do some research on how to obtain this.
# There are a few ways of doing this. 
library(ggmap)
key <- 'AIzaSyBf1Md3BLean7Ox_ldHdQwWogCyRY3UhzE'
register_google(key = key)


cityStateLatLon <- function(cityStat){
  return(geocode(cityStat))
}
  

# 4 points 
# 6. Using the function above, obtain the lat lons for each of the top 20 cities.

citystatescombos<-names(top20)
cityStateLatLon_df <- NULL
for (i in citystatescombos){
  cityStateLatLon_df <-rbind(cityStateLatLon_df,data.frame(cityStateLatLon(i)))
}
cityStateLatLon_df<-cbind(data.frame(citystatescombos),cityStateLatLon_df)
cityStateLatLon_df

# 2 points
# 7. Write code to plot the top 20 cities on a map. Include your plot
# with your submission.
install.packages("maps")
library(maptools)
library(maps)
library(ggplot2)

#On USA Map
m = map_data('state')
ggplot()+geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+geom_point(data=cityStateLatLon_df,aes(x=cityStateLatLon_df$lon ,y=cityStateLatLon_df$lat),colour="red",)

#On World Map

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=cityStateLatLon_df$lon,y=cityStateLatLon_df$lat) ,color="blue", size=1) 
mp
    
# 4 points
# 8. There are some misspellings and other issues 
# with the "Current Adjuster" field. Leverage the text
# analysis tools and levenstein distance to clean up 
# the names properly. Put them into a new column called
# "Current Adjuster Cleaned"
# Hint: you must deal with issues of case, whitespace,
# ,name misspellings and common name differences (ie Dave vs David). 
# You will be graded on how well you complete this. 
library(stringdist)
allUpper <- toupper(df$`Current Adjuster`)
unvalidname<-(!grepl("^[a-zA-Z]",allUpper))
sum(unvalidname)#check no of invalid names thoes contain alphanumeric  

allUniques<-unique(allUpper)
worddistance<-NULL
worddistance<-stringdistmatrix(allUniques, allUniques, method = 'lv', useNames = "strings") #similar word distance
worddistance<-subset(melt(worddistance), value>0 & value<5)
orderedwords <- worddistance[order(worddistance$value, decreasing = FALSE),]
orderedwords

# Var1              Var2 value
# 4787  SUSAN CHAMBERLIN SUSAN CHAMBERLAIN     1
# 6141      IRA  DOBBINS       IRA DOBBINS     1
# 5999       JOSH HURLEY     JOSHUA HURLEY     2
# 7016       RON CROWDER    RONALD CROWDER     3
# 3085       LYNN HARVEY   LYNNETTE HARVEY     4

realNames<- sapply(df$`Current Adjuster`, function(name) switch(name,
                                                     'SUSAN CHAMBERLIN' = 'SUSAN CHAMBERLAIN',
                                                     'IRA  DOBBINS' = 'IRA DOBBINS',
                                                     'JOSH HURLEY' = 'JOSHUA HURLEY',
                                                     'RON CROWDER' = 'RONALD CROWDER',
                                                     'LYNN HARVEY' = 'LYNNETTE HARVEY', name))
df[,"Current Adjuster Cleaned"] <-realNames
head(df)

# 4 points
# Question 9:
# You want to give your state-managers a report every month
# that shows of the top 20 adjusters (by row count) in that 
# "Policy holder State", how
# many claims they have in each "Occupancy Status". 
# For example, for a "Move-in/Check-In Date" in
# March 2015, in TX, who were the top
# n current adjusters and how many did they have in 
# "Moved-In" status, etc
# Write a function that takes in a value of state, 
# a date in the form of YYYY-MM, value of n and 
# returns the above
# report for that month.
# Run this function for the parameters below.
library(dplyr)
n = 3
state = "CA"
date = '2015-03'

reportParameter <-function(n, state, date){
  temp_df<-NULL
  temp_df<-df[which(df$`Policy holder State` == state & substr(df$`Move-in/Check-In Date`,1,7) == date), ]
  report_df<- temp_df %>% group_by(temp_df$`Current Adjuster Cleaned`, temp_df$`Occupancy Status`) %>% count()
  names(report_df)[1] <- "Adjuster"
  names(report_df)[2] <- "Occupancy"
  report_df<-data.frame(dcast(report_df, Adjuster ~ Occupancy, fun.aggregate = sum, value.var = 'n'))
  report_df$Total <- report_df$Checked.Out + report_df$Moved.Out
  report_df <- report_df[order(report_df$Total, decreasing = TRUE),]
  return(print(head(report_df,n),row.names = FALSE))
  
}
reportParameter(n,state,date)




