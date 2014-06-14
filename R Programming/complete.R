complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    result <- data.frame()

    filenames = list()
    for(i in id) {
        filename = sprintf("%s/%03d.csv", directory, i)
        df = read.csv(filename)

        good = complete.cases(df)
        result <- rbind(result, data.frame(id = i, nobs = nrow(airquality[good, ])))
    }

    result
}