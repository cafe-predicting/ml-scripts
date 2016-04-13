# mase.R
# Ryan Zembrodt
# Mean Absolute Scaled Error
# See https://en.wikipedia.org/wiki/Mean_absolute_scaled_error
mase <- function(actualValues, predictedValues) {
  # Throw exception if actualValues or predictedValues are of length 1.
  if (length(actualValues) < 2 || length(predictedValues) < 2) {
    stop("Incorrect number of rows")
  }
  
  # Mean Absolute Scaled Error is calculated by dividing the forecast error by the average forecast error.
  # The forecast error is the sum of the absoulte value of y_i - f_i
  #   where y is the vector 1 to n of actual values
  #   and f is the vector 1 to n of predicted values
  # The average forecast error is calculated by n / (n - 1)
  #   multiplied by the sum from i = 2 to n of the absolute value of y_i - y_(i-1).
  
  # The denominator value calculates the average forecast error.
  denominator <- 0
  
  # The for loop calculates the summation portion first.
  for (i in 2:length(actualValues)) {
    denominator <- denominator + (abs(actualValues[i] - actualValues[(i-1)]))
  }
  
  # Here the n/(n-1) portion is calculated and multiplied to the summation portion.
  denominator <- (length(actualValues) / (length(actualValues)-1)) * denominator
  
  # Calculate the forecast error and divide it by the average forecast error defined above.
  return((sum(abs(actualValues - predictedValues))) / denominator)
}