
# Loading libraries
# Please install libraries if they are not installed/vailable in your R
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library("rpart.plot")
library(knitr)
library(corrplot)
library(plotly)
library(randomForest)

# Data
## Loading Data
setwd("~/Desktop/PracticeML-data")

d_train <- read.csv("~/Desktop/PracticeML-data/pml-training.csv", stringsAsFactors = F,na.strings = c("","NA","#DIV/0!"), header = TRUE)
d_test <- read.csv("~/Desktop/PracticeML-data/pml-testing.csv", stringsAsFactors = F,na.strings = c("","NA","#DIV/0!"), header = TRUE)

dim(d_train)
dim(d_test)

## Data cleaning

d_train<-d_train[,colSums(is.na(d_train)) == 0]
d_test <-d_test[,colSums(is.na(d_test)) == 0]

# Subset data
d_train   <-d_train[,-c(1:7)]
d_test <-d_test[,-c(1:7)]
dim(d_train)
dim(d_test)

## Slice data/Cross-validation
# Set seed for reproducability
set.seed(7878)

# cross validation
crval <- caret::createDataPartition(d_train$classe, p = 0.8, list = F)
d_val <- d_train[-crval,]
d_train <- d_train[crval,]
dim(d_train); dim(d_val)

# Modeling
# ML Algorithm_ Decision tree
# Fit model
modFitDT <- rpart(classe ~ ., data=d_train, method="class")
# Perform prediction
predictDT <- predict(modFitDT, d_val, type = "class")
table(d_val$classe, predictDT)
confusionMatrix(table(d_val$classe, predictDT))

## ML Algorithm_ Random forest
modFitRF <- randomForest(as.factor(classe) ~ ., data=d_train, method="class")
predictRF <- predict(modFitRF, d_val, type = "class")
confusionMatrix(table(d_val$classe, predictRF))

## Predicting on Test dataset

result <- predict(modFitRF, d_test[, -length(names(d_test))])
result
# Plot result_ Decision tree 
rpart.plot(modFitDT)
# Plot Correlation Matrix
corrPlot <- cor(d_train[, -length(names(d_train))])
corrplot(corrPlot, method="color")

