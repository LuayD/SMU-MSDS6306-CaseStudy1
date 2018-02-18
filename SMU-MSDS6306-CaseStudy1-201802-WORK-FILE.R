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

## Analysis 4: Compute median alcohol content (ABV) and international bitterness unit (IBU) per each state.

## Analysis 5: Show which state has the maximum alcoholic (ABV) beer and well as the most bitter.

## Analysis 6: Summary of statistics for the alcohol content (ABV).

## Analysis 7: Prove relationship between the bitterness of the beer nad its alcoholic content/