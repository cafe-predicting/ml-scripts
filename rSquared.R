# rSquared.R
# Ryan Zembrodt
# Calculates the R squared value of two vectors: actualValues and predictedValues.
# See https://en.wikipedia.org/wiki/Coefficient_of_determination#Definitions
rSquared <- function(actualValues, predictedValues) {
  # R^2 is calculated by 1 minus the residual sum of squares divided by the total sum of squares.
  # The residual sum of squares is the sum of (y_i - f_i) squared
  #   where y is the vector 1 to n of actual values
  #   and f is the vector 1 to n of predicted values
  # The total sum of squares is the sum of (y_i - y_mean) squared
  #   where y_mean is the mean of the y vector.
  # In the code below, actualValues is the vector y and predictedValues is the vector f.
  value <- 1 - (sum((actualValues - predictedValues)^2) / sum((actualValues - mean(actualValues))^2))
  
  return(value)
}