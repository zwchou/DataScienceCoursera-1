pollutantmean <- function(directory, pollutant, id = 1:332) {
    require("plyr")

    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)

    filenames = list()
    for(i in id) {
        filenames = c(filenames, sprintf("%s/%03d.csv", directory, i))
    }

    datalist = ldply(filenames, function(filename) {
        dum = read.csv(filename)
        return(dum)
    })

    mean(datalist[[pollutant]], trim = 0, na.rm = TRUE)
}