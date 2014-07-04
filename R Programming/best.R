best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    if (nrow(subset(data, State==state)) <= 0) {
        stop ("invalid state")
    }

    data2 <- data[data$State==state, ]
    switch(outcome,
        "heart attack"={
            col <- 11
        },
        "heart failure"={
            col <- 17
        },
        "pneumonia"={
            col <- 23
        },
        stop ("invalid outcome")
    )

    data2[, col] <- suppressWarnings(as.numeric(data2[, col]))
    data <- data2[order(data2[col], data2[2], na.last=TRUE), ]

    ## Return hospital name in that state with the lowest 30-day death rate
    print(data[1, 2])
}