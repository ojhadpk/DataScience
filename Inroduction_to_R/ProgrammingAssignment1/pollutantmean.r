pollutants <- c()
pollutantmean <- function(directory, pollutant, id = 1: 332) {
        ##List of all the files
        filenames <- list.files(directory)
        ##For each csv file
        for (item in id) {
                ##Creating filepath
                filepath = paste(directory,"/",filenames[item], sep = "")
                ##read file and store in data
                data <- read.csv(filepath)
                ##Adding all relevant data in one vector
                pollutants <- c(pollutants, data[, pollutant])
        }
        ##Calculating mean
        pollutants.mean <- mean(pollutants, na.rm = T)
        pollutants.mean
}
ids = c()
observations = c()
complete <- function (directory, id = 1:332) {
        filenames <- list.files(directory)
        for (item in id) {
                filepath = paste(directory,"/",filenames[item], sep = "")
                data <- read.csv(filepath)
                ##Using compete.cases to remove NA data
                completeCases = data[complete.cases(data),]
                ids = c(ids, item)
                observations = c(observations, nrow(completeCases))
        }
        data.frame(id = ids, nobs = observations)
}

corr <- function(directory, threshold = 0) {
        completes = complete(directory, 1:332)
        completes.above.threshold = subset(completes, nobs > threshold)
        correlations <- vector()
        filenames <- list.files(directory)
        for (i in completes.above.threshold$id) {
                filepath = paste(directory,"/",filenames[i], sep = "")
                data <- read.csv(filepath)
                completeCases <- data[complete.cases(data),] 
                count <- nrow(completeCases)
                if (count >= threshold) {
                        correlations = c(correlations, cor(completeCases$nitrate, completeCases$sulfate))
                }
        }
        correlations
}