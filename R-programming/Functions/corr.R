#Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.

corr <- function(directory, threshold = 0){
    df <- complete(directory)
    filenames <- Sys.glob(paste(directory, "/*.", "csv", sep = ""))
    vec <- vector()
    for (row in 1:nrow(df)) {
        id =  df[row, "id"]
        comp <- df[row, "nobs"]
        if (comp > threshold) {
            sample <- read.csv(filenames[id], na.strings = 'NA', header = TRUE)
            corr_pol = cor(sample$sulfate, sample$nitrate, use = "pairwise.complete.obs")
            vec <- c(vec, corr_pol)
        }
    }
    return (vec)
}