# mase.R
# Ryan Zembrodt
# Mean Absolute Scaled Error
# See https://en.wikipedia.org/wiki/Mean_absolute_scaled_error
mase <- function(actualValues, predictedValues) {
  if (length(actualValues) < 2 || length(predictedValues) < 2) {
    stop("Incorrect number of rows")
  }
  
  denominator <- 0
  for (i in 2:length(actualValues)) {
    denominator <- denominator + (abs(actualValues[i] - actualValues[(i-1)]))
  }
  
  denominator <- (length(actualValues) / (length(actualValues)-1)) * denominator
    
  return((sum(abs(actualValues - predictedValues))) / denominator)
}