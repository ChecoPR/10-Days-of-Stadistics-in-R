

#
# Complete the 'quartiles' function below.
#
# The function is expected to return an INTEGER_ARRAY.
# The function accepts INTEGER_ARRAY arr as parameter.
#
library(dplyr)

quartiles <- function(arr) {
    # Write your code here
    arr <- sort(arr)
    n <- length(arr)
    q2 <- median(arr)
    if(n %% 2 == 0) {
        rl <- n / 2
        q1 <- median(arr[1:rl])
        q3 <- median(arr[(rl+1):n])
    }else{
        rl <- floor(n / 2)
        q1 <- median(arr[1:rl])
        q3 <- median(arr[(rl+2):n])
    }
    cat(arr)
    data <- c(q1, q2, q3)
}

stdin <- file('stdin')
open(stdin)

fptr <- file(Sys.getenv("OUTPUT_PATH"))
open(fptr, open = "w")

n <- as.integer(trimws(readLines(stdin, n = 1, warn = FALSE), which = "both"))

data <- strsplit(trimws(readLines(stdin, n = 1, warn = FALSE), which = "right"), " ")[[1]]
data <- as.integer(data)

res <- quartiles(data)

writeLines(paste(res, collapse = "\n"), con = fptr)

close(stdin)
close(fptr)
