#
# Complete the 'weightedMean' function below.
#
# The function accepts following parameters:
#  1. INTEGER_ARRAY X
#  2. INTEGER_ARRAY W
#
# I imported dplyr just for "pipe" operator (%>%) and read the code in steps
library(dplyr)

weightedMean <- function(X, W) {
    sum(X * W) / sum(W)     # Apply the formula
}

stdin <- file('stdin')
open(stdin)

n <- as.integer(trimws(readLines(stdin, n = 1, warn = FALSE), which = "both"))

vals <- strsplit(trimws(readLines(stdin, n = 1, warn = FALSE), which = "right"), " ")[[1]]
vals <- as.integer(vals)

weights <- strsplit(trimws(readLines(stdin, n = 1, warn = FALSE), which = "right"), " ")[[1]]
weights <- as.integer(weights)

weightedMean(vals, weights) %>%     # Call weightedMean function with X and W parameters
    round(digits = 1) %>%           # Round result to 1 digit
    format(nsmall = 1) %>%          # Format result to 1 digit
    cat()                           # Print in console

close(stdin)
