library(RUnit)

# Load source files
source("rSquared.R")

# Test the rSquared function and that it correctly tests the accuracy of a predicted vector of values.
rSquared.test <- function() {
  actualValues <- c(1, 2, 3)
  predictedValues <- c(1.5, 2, 2.5)
  result <- 1 - (
    ((1 - 1.5)^2 + (2 - 2)^2 + (3 - 2.5)^2) /
    ((1 - 2)^2 + (2 - 2)^2 + (3 - 2)^2)
  )
    
  checkEquals(rSquared(actualValues = actualValues, predictedValues = predictedValues), result)
}

# Print test outputs
print(rSquared.test())