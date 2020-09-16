## Part 1: Make a simple histogram of the 30-day death rates from heart attack
## Data set is outcome-of-care-measures.csv from http://hospitalcompare.hhs.gov

## Read the data into outcome:

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Check the first 10 rows:

head(outcome, 10)

## The data we need to plot is in column 11 - We read in as "character" and need to 
## coerce the column to be numeric so we can plot it (will tell us NAs introduced)

outcome[, 11] <- as.numeric(outcome[, 11])

## Plot data in Histogram

hist(outcome[, 11])
