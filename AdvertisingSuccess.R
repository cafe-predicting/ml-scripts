# Chris Arnold
# Advertising Success Predictor
# Requires libraries: C50, e1071 and gmodels installed
# These can be obtained using command:
# install.packages("packageName")

# Set Working Directory
setwd("~/CS499/PosSimData/PosSimData")

# Call libraries to access
library(C50)
library(e1071)
library(gmodels)

# Read CSV and pull out subset
# Split 70/30 into train and test sets
pos_raw <- read.csv("PointOfSaleSimulation.csv")
pos <- subset(pos_raw, select = c(1,3,12,13,21))
pos_train <- pos[1:103989,]
pos_test <- pos[103990:138652,]

# Currently Running c5.0 and naive Bayes
# m_Name are models, p_Name are test predictions
m_C50 <- C5.0(pos_train[-5], pos_train$PurchasedAdvertisedProduct, trials = 10)
m_Bayes <- naiveBayes(pos_train, pos_train$PurchasedAdvertisedProduct, laplace = 0)
p_C50 <- predict(m_C50, pos_test)
p_Bayes <- predict(m_Bayes, pos_test)

# Simple checking of success via Confusion Matrix using Cross Tables
CrossTable(p_C50, pos_test$PurchasedAdvertisedProduct, prop.chisq = FALSE, prop.t = FALSE, dnn = c('Predicted','Actual'))
CrossTable(p_Bayes, pos_test$PurchasedAdvertisedProduct, prop.chisq = FALSE, prop.t = FALSE, dnn = c('Predicted','Actual'))
