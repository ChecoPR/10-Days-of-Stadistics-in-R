# Day 0: Mean, Median, and Mode

# I will try without base libraries like mean() and median()

# I imported dplyr just for "pipe" operator (%>%) and read the code in steps
library(dplyr)

# Setting the input vector
v <- file('stdin') %>%  # Read "standar input"
    readLines() %>%     # Read all lines 
    strsplit(" ") %>%   # Split string every space
    unlist() %>%        # The last step create a list but this simplifies it to a vector
    as.numeric()        # Cast character vector to numeric

v <- v[-1]     # Remove first element that is "N"

# Custom mean function 
mean_custom <- function(X) {
    sum(X) / length(X)
}

# Custom median function
median_custom <- function(X) {
    X <- sort(X)                                # Sort the vector
    if((length(X) %% 2) != 0) {                 # Check if "n" is odd
        as.integer(X[length(X) / 2])            # Only get the middle element
    }
    else {                                      # "n" is even, get two middle elements
        x1 <- X[as.integer(length(X) / 2)]
        x2 <- X[as.integer(length(X) / 2) + 1]
        (x1 + x2) / 2                           # average the two middle elements
    }
}

# Custom mode function
mode_custom <- function(X) {
    X <- sort(X)                    # Sort the vector
    n <- length(X)                  # Save n
    N <- vector("integer", n)       # Vector to count the number of occurrence of each element
    for(i in 1:n){
        for(j in 1:n){              # "for" nested to check "i"st element with "j"st
            if(X[i] == X[j]){       # if "i"st and "j"st are equal, add 1 to N"i"st
                N[i] = N[i] + 1
            }
        }
    }
    X[which(N == max(N))[[1]]]      # Get the max occurrence index in N to get X element 
}

# Print in console
cat(
    paste(                          # Concatenate strings
        mean_custom(v),                    # Call custom mean function
        median_custom(v),                  # Call custom median function
        mode_custom(v),                    # Call custom mode function
        sep = '\n'                  # Add line break separation in each concatenation
    )
)