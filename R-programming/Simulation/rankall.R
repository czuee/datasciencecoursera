### Ranking hospitals in all states
## The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num.

rankall <- function(outcome, num = "best") {
      ## Read outcome data
      my_data <- read.csv("~/R_Bootcamp/datasciencecoursera/R-programming/Simulation/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", na.strings = c("NA", "Not Available"), stringsAsFactors=FALSE )
      out_vals <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      
      ## Check that outcome is valid
      if (outcome %in% names(out_vals)){
            my_data <- my_data[ ,c(2, 7, out_vals[outcome])]
      }
      else { stop("invalid outcome")}
      
      names(my_data) <- c("hospital", "state", "outcome")
      
      ## For each state, find the hospital of the given rank
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
      
      #Remove NA cases
      my_data <- my_data[complete.cases(my_data),]
      
      #Order data
      my_data <- my_data[
            with(my_data, order(state, outcome, hospital)),
            ]
      
      #Split data into a list of dataframes based on states***Interesting trick!!***
      split_mydata <- split(my_data, my_data$state)
      
      #Make a function to assign values to qualitative characters and
      # return the nth hospital
      hospital_rank <- function(df, n){
            num_vals <- c("best" = 1, "worst" = nrow(df))
            if (n %in% names(num_vals)){
                  n <- num_vals[n]
            }
            rank_hosp <- df$hospital[n]
            return(rank_hosp)
      }
      
      #Apply the function to each element of split_mydata and return a dataframe
      hosp_vec <- sapply(split_mydata, hospital_rank, num)
      hosp_df <- data.frame(hospital = hosp_vec, state = names(hosp_vec), row.names = names(hosp_vec))
}