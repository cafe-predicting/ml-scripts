# weatherPredictor.R
# Ryan Zembrodt
# Creates a model to predict how many customers will be in the cafeteria based on time and weather

library(RWeka)

source("rSquared.R")
source("mase.R")

weatherPredictor <- function(dataset) {
  # Randomize the dataset
  randData <- dataset[sample(1:nrow(dataset)),]
  
  # Train data is first 70%
  trainData <- randData[1:(floor(nrow(randData)*0.7)),]
  # Test data is the last 30%
  testData <- randData[(floor(nrow(randData)*0.7)+1):nrow(randData),]
  
  # Create a model and prediction vector for M5Prime
  m <- M5P(InCount ~ Minute+Temp+Precipitation+DayOfWeek, data = trainData)
  p <- predict(m, testData)
  
  print(sprintf("rSquared accuracy: %f", rSquared(testData$InCount, p)))
  print(sprintf("MASE accuracy: %f", mase(actualValues = testData$InCount, predictedValues = p)))
    
  return(m)
}

# Input data from csv (generated by parser)
weatherData <- read.csv("weatherGatesData.csv")

# Create the model
weatherData.m <- weatherPredictor(weatherData)