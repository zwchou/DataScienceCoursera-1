corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations

    frames = complete(directory)
    result = apply(X = frames, MARGIN = 1, FUN = function(x){
        if(x["nobs"] > threshold){
            filename <- sprintf("%s/%03d.csv", directory, x["id"])
            dum = read.csv(filename)
            r = cor(dum["sulfate"], dum["nitrate"], use = "na.or.complete")
            return (as.numeric(r[1,1]))
        } else {
            return (as.numeric(NA))
        }
    })

    good = complete.cases(result)
    as.vector(result[good])
}