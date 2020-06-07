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
file <-  
df <- read_excel(file, sheet = "Sheet1")
df <- as.data.frame(df)

# 2 points
# 1. The three vendors each use a different definition of housing type. However, ACME's official types
# are listed on Sheet2 of the Excel sheet. 
# Create a new column called 'Normalized Housing Type' based on the standardized mapping.



# 2 points
# 2. Compute the total housing spend by Policy holder State 
# and the total percentage of 
# total housing spend by Policy holder State
# listed in descending order by cost. Return this as a dataframe.




# 2 points
# 3. Create a table that has Normalized Housing Type (NHT) on the y-axis 
# and vendor on the x-axis. At the 
# intersection, compute the 
# 'Total Housing Spend of spend for that given and vendor and NHT. Compute and display 
# on the same table, the row-wise and column-wise margins too.



# 2 points
# 4. Obtain top 20 most frequent Policyholder City and Policy holder State combos




# 4 points
# 5. Write a function obtains the lat lon for a given city and state
# Note: You'll propsefully need to do some research on how to obtain this.
# There are a few ways of doing this. 

cityStateLatLon <- function()

# 4 points
# 6. Using the function above, obtain the lat lons for each of the top 20 cities.




# 2 points
# 7. Write code to plot the top 20 cities on a map. Include your plot
# with your submission.



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
df[,"Current Adjuster Cleaned"] <-
  


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

n = 3
state = "CA"
date = '2015-03'





