#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.
pollutantmean <- function(directory, pollutant, id = 1:332){
    filenames <- Sys.glob(paste(directory, "/*.", "csv", sep = ""))
    sum_pol <- 0
    len_pol <- 0
    for (filenumber in id) {
        sample <- read.csv(filenames[filenumber], na.strings = 'NA', header = TRUE)
        pol <- sample[, pollutant]
        sum_pol = sum_pol  + sum(pol, na.rm = TRUE)
        len_pol = len_pol + sum(!is.na(pol))
    }
    mean(sum_pol/len_pol)
}