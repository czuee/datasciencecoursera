### 1. Plot the 30-day mortality rates for heart attack

#Read the outcome data into R via the read.csv function and look at the rst few rows.
outcome <- read.csv("~/R_Bootcamp/datasciencecoursera/R-programming/Simulation/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

str(outcome)
ncol(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
summary(outcome[,11])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  10.10   14.50   15.40   15.45   16.40   21.90    1986 
head(outcome[,11])

hist(outcome[, 11])


###2 Finding the best hospital in a state

#How many hospitals per state? Min Mortality rate in a hospital per state?
table(outcome$State)
tapply(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome$State, min, na.rm = TRUE)

##See file best.R
