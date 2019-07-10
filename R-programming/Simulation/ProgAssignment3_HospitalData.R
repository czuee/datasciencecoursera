### 1. Plot the 30-day mortality rates for heart attack

#Read the outcome data into R via the read.csv function and look at the rst few rows.
outcome <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
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

best <- function(state, disease) {
      ## Read outcome data
      # I dont worry about matching the string, upper/lower case etc. (use tidyverse str_detect for that)
      out_vals <- data.frame(c("heart attack", "heart failure", "pneumonia"), c(11, 17, 23))
      col <- for (i in 1:ncol(out_vals)) {
            if (identical(as.character(out_vals[ i, 1]), disease)){
                  out_vals[i , 2]  
            }
      }
      out_data <- outcome[ , col]

      ## Check that state and outcome are valid
      
      ## Return hospital name in that state with lowest 30-day death
      ## rate
}