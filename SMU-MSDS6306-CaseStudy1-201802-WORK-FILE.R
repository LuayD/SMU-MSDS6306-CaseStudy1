##
# SMU MSDS 6306 Case Study 1: Beers, Breweries, and Bitterness
# Base R Workfile for Case Study
# Work by: Tiger Team - Adil Siraj, Garrett Mozey, Dana Geislinger, Luay Dajani
##

## Analysis 1: Number of Breweries per US State

# Import breweries data and preview data structure.
breweries = read.csv("Raw Data/Breweries.csv", header=TRUE)
str(breweries)

# Entries in 'State' column start with a space.
# Clean data by removing leading white-space and verify new structure.
breweries$State = as.factor(trimws(breweries$State))
str(breweries)

# Print number of breweries in each state.
# Since each row is a brewery, the number of occurences of each state equals the number of breweries in that state.
breweries.by.state = tapply(breweries$State, breweries$State, length)
print(breweries.by.state)

## Analysis 2: Merge of Beer & Breweries Data

# Import beer data and preview data structure.
beers = read.csv("Raw Data/Beers.csv", header=TRUE)
str(beers)

# Change Brewery ID column name for beer data to match brewery data.
names(beers)[5] = "Brew_ID"

# Merge beer and brewery data by 'Brew_ID' and preview naming structure.
brew.data = merge(breweries, beers, by="Brew_ID", all=TRUE)
names(brew.data)

# Rename variables to be meaningful and verify new structure.
names(brew.data) = replace(names(brew.data), c(2, 5), c('Brew_Name', 'Beer_Name'))
str(brew.data)

# Print first 6 and last 6 observations of the merged data for verification.
head(brew.data, 6)
tail(brew.data, 6)

## Analysis 3: Missing Data (NA's in each column)

# Create Vector Dataframe which indicates the missing data (NA's)  in each columns as TRUE or FALSE.
brew.data.missing = is.na(brew.data)

# Null out vector to be used in for loop
number_NAs = NULL

# Though not intened, creates vector of "Named ints" where the name of the row associated with each int value
for (i in colnames(brew.data.missing))
{
  tempvec = brew.data.missing[,i]
  number_NAs[i] = length(tempvec[tempvec != FALSE])
}

print(number_NAs)

## Analysis 4: Compute median alcohol content (ABV) and international bitterness unit (IBU) per each state.

# Use "tapply" to find the the median IBU per state
ABV_medians = tapply(brew.data$ABV   # Take the IBU column (numeric)
                     , brew.data$State # Use the State column as the index (as factor) to associate IBU per State
                     , function (x) {median(x, na.rm = TRUE)} #Run created function finding the median per State removing NULLs
)

#Print out the results
cat("The Median Alcohol Content (ABV) per state:\n") #using cat to preserve the \n
print(ABV_medians)

#barplot of the ABV medians
par(las = 2) #make the x axis name horizontal with the horizontal view
par(mar=c(5,10,4,2)) # increase y-axis margin (c(bottom, left, top, right))
barplot(ABV_medians
        , main="Median Alcohol Content (ABV) per State"
        , xlab="Median ABV"
        , ylab="States"
        , cex.names=0.5 #Font Size for the State names to squeeze them all in
        , col=c("red","green","blue","orange","brown","purple","yellow")
        , horiz=TRUE
     )

# Use "tapply" to find the the median IBU per state
IBU_medians = tapply(brew.data$IBU   # Take the IBU column (numeric)
       , brew.data$State # Use the State column as the index (as factor) to associate IBU per State
       , function (x) {median(x, na.rm = TRUE)} #Run created function finding the median per State removing NULLs
)

#Print out the results
cat("The International Bitterness Unit (IBU) per each state:\n") #using cat to preserve the \n
print(IBU_medians)

#barplot of the ABV medians
par(las = 2) #make the y axis name horizontal with the horizontal view
par(mar=c(5,10,4,2)) # increase y-axis margin (c(bottom, left, top, right))
barplot(IBU_medians
        , main="Median International Bitterness Unit (IBU)  per State"
        , xlab="Median IBU"
        , ylab="States"
        , cex.names=0.5 #Font Size for the State names to squeeze them all in
        , col=c("red","green","blue","orange","brown","purple","yellow")
        , horiz=TRUE
)

## Analysis 5: Show which state has the maximum alcoholic (ABV) beer and well as the most bitter.
ABV=brew.data[c('State','ABV')] # This  will give out the Alcohlic content in beer for every state
ABV[which.max(brew.data$ABV),]['State'] #This  will give out the maximum Alcohlic content in beer out of all states

IBU=brew.data[c('State','IBU')]# This  will give out the bitterness unit in beer for every state
IBU[which.max(brew.data$IBU),]['State'] #This  will give out the most bitter beer out of all states


## Analysis 6: Summary of statistics for the alcohol content (ABV).
summary(ABV$ABV)

## Analysis 7: Prove relationship between the bitterness of the beer nad its alcoholic content/