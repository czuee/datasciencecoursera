
## Function to find the hospital with the best outcomes within a state given by Minimum mortality cases.
## In case of a tie, the first alphabetical option is output

best <- function(state, outcome) {
      ## Read outcome data
      my_data <- read.csv("~/R_Bootcamp/datasciencecoursera/R-programming/Simulation/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", na.strings = c("NA", "Not Available"), stringsAsFactors=FALSE )
      
      # Assign column numbers to get the right index from the data
      out_vals <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      
      ## Check that state and outcome are valid
      # I dont worry about matching the string, upper/lower case etc. (use tidyverse str_detect for that)
      #browser()
      if (outcome %in% names(out_vals)){
         my_data <- my_data[ ,c(2, 7, out_vals[outcome])]
      }
      else { stop("invalid outcome")}
      
      if (state %in% my_data$State){
         my_data <- subset(my_data, my_data$State == state, drop = TRUE)
      }
      else { stop("invalid state")}
      
      #Cleaning
      names(my_data) <- c("hospital", "state", "outcome")
      
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      
      vec <- which(my_data$outcome == min(my_data$outcome, na.rm = TRUE))
      best_hospitals <- sort(my_data[vec, 1])
      bestest <- best_hospitals[1]
      return(bestest)
}

#best("TX", "heart attack")
#best("MD", "heart attack")
