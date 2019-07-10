#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases

complete <- function(directory, id = 1:332){
    filenames <- Sys.glob(paste(directory, "/*.", "csv", sep = ""))
    
    #Inititate a dataframe with 2 columns
    num_cases <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(num_cases) <- c("id", "nobs")
    
    #Loop through the monitor ids
    for (filenumber in id) {
        sample <- read.csv(filenames[filenumber], na.strings = 'NA', header = TRUE)
        comp = sum(complete.cases(sample))
        num_cases[nrow(num_cases)+1, ] <- list(id = filenumber, nobs = comp)
    }
    return (num_cases)
}