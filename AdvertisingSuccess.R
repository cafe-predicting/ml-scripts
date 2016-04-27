# Chris Arnold
# Advertising Success Predictor
# Requires libraries: sata.table, e1071 and gmodels installed
# These can be obtained using command:
# install.packages("packageName")

# Set Working Directory
setwd("~/CS499/PosSimData/PosSimData")

# Install library packages (if needed)
# install.packages("data.table")
# install.packages("e1071")
# install.packages("gmodels")

# Call libraries to access
library(data.table)
library(e1071)
library(gmodels)

# Read CSV
pos_raw <- read.csv("PointOfSaleSimulation.csv")

# Use data table to combine POS entries
# into customer entries by Datetime
# Pull out subset of relevant data
cust_raw <- as.data.table(pos_raw)[,lapply(.SD,max), by = list(Datetime,CustomerAgeGroup,CustomerGender)]
cust <- subset(cust_raw, select = c(2,3,13,14,21))

cust <- cust[!(cust$CustomerAgeGroup == "Unknown")]
cust_rand <- cust[order(runif(25161))]
items <- subset(cust, select = c(3,4,5))

# Split 80/20 into train and test sets
items_train <- items[1:20128,]
items_test <- items[20129:25161,]

# Currently Running c5.0 and naive Bayes
# m_Name are models, p_Name are test predictions
m_item_Bayes <- naiveBayes(items_train, items_train$PurchasedAdvertisedProduct, laplace = 0)
p_item_Bayes <- predict(m_item_Bayes, items_test)

# Simple checking of success via Confusion Matrix using Cross Tables
CrossTable(p_item_Bayes, items_test$PurchasedAdvertisedProduct, prop.chisq = FALSE, prop.t = FALSE, dnn = c('Predicted','Actual'))
