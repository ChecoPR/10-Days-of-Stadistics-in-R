

#
# Complete the 'interQuartile' function below.
#
# The function accepts following parameters:
#  1. INTEGER_ARRAY values
#  2. INTEGER_ARRAY freqs
#
# I imported dplyr just for "pipe" operator (%>%) and read the code in steps
library(dplyr)

interQuartile <- function(values, freqs) {
    # Print your answer to 1 decimal place within this function
    S <- sort(rep(values, freqs))
    if(length(S) %% 2 == 0){
        u <- length(S) / 2
        q1 <- median(S[1:u])
        q3 <- median(S[(u+1):length(S)])
    }else{
        u <- floor(length(S) / 2)
        q1 <- median(S[1:u])
        q3 <- median(S[(u+2):length(S)])
    }
    q3 - q1
}

stdin <- file('stdin')
open(stdin)

n <- as.integer(trimws(readLines(stdin, n = 1, warn = FALSE), which = "both"))

val <- strsplit(trimws(readLines(stdin, n = 1, warn = FALSE), which = "right"), " ")[[1]]
val <- as.integer(val)

freq <- strsplit(trimws(readLines(stdin, n = 1, warn = FALSE), which = "right"), " ")[[1]]
freq <- as.integer(freq)

interQuartile(val, freq) %>%     # Call weightedMean function with X and W parameters
    round(digits = 1) %>%           # Round result to 1 digit
    format(nsmall = 1) %>%          # Format result to 1 digit
    cat()                           # Print in console

close(stdin)
