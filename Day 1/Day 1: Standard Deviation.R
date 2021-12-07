#
# Complete the 'stdDev' function below.
#
# The function accepts INTEGER_ARRAY arr as parameter.
#
library(dplyr)

stdDev <- function(arr) {
    # Print your answers to 1 decimal place within this function
    m <- mean(arr)
    arr <- sapply(arr, function(x){
        (x - m) ** 2
    })
    (sum(arr) / length(arr)) ** 0.5
}

stdin <- file('stdin')
open(stdin)

n <- as.integer(trimws(readLines(stdin, n = 1, warn = FALSE), which = "both"))

vals <- strsplit(trimws(readLines(stdin, n = 1, warn = FALSE), which = "right"), " ")[[1]]
vals <- as.integer(vals)

stdDev(vals) %>%
    round(digits = 1) %>%
    format(nsmall = 1) %>%
    cat()

close(stdin)
