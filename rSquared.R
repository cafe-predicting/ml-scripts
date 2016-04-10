# Calculates the R squared value of two vectors: actualValues and predictedValues.
rSquared <- function(actualValues, predictedValues) {
  # R^2 = 1 - (sum_(i=1 to n) of (actualValues[i] - predictedValues[i])^2) / (sum_(i=1 to n) of (actualValues[i] - mean of actualValues)^2)
  value <- 1 - (sum((actualValues - predictedValues)^2) / sum((actualValues - mean(actualValues))^2))
  
  return(value)
}