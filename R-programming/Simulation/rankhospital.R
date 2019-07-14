
###Function to rank hospitals by outcome in a state.
##The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
##of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      my_data <- read.csv("~/R_Bootcamp/datasciencecoursera/R-programming/Simulation/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", na.strings = c("NA", "Not Available"), stringsAsFactors=FALSE )
      out_vals <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      
      ## Check that state and outcome are valid
      if (outcome %in% names(out_vals)){
            my_data <- my_data[ ,c(2, 7, out_vals[outcome])]
      }
      else { stop("invalid outcome")}
      
      if (state %in% my_data$State){
            my_data <- subset(my_data, my_data$State == state, drop = TRUE)
      }
      else { stop("invalid state")}
      
      names(my_data) <- c("hospital", "state", "outcome")
      
      
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      
      my_data <- my_data[complete.cases(my_data),]
      ordered_data <- my_data[
            with(my_data, order(state, outcome, hospital)),
      ]
      num_vals <- c("best" = 1, "worst" = nrow(ordered_data))
      if (num %in% names(num_vals)){
            num <- num_vals[num]
      }
      rank_hosp <- ordered_data$hospital[num]
      return(rank_hosp)
}