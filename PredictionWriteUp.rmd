---
title: "Assignment - Practical Machine Learning"
author: "Roland Pfeifer"
date: "February 5, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Environment, loading required libraries
```{r, warn.conflicts = FALSE}
library(knitr)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(gbm)
library(plyr)
set.seed(1234)
```


## Data preparation

```{r}
# download and load datasets Train and Test
MyTrainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
MyTestUrl  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
MyTrainFile<-"pml-traininig.csv"
MyTestFile<-"pml-testing.csv"

# download datasets
if(!file.exists(MyTrainFile))
{
    download.file(MyTrainUrl,destfile = MyTrainFile)
}
training <- read.csv(MyTrainFile, na.strings=c("NA","#DIV/0!",""))

if(!file.exists(MyTestFile))
{
    download.file(MyTestUrl,destfile = MyTestFile)
}
testing <- read.csv(MyTestFile, na.strings=c("NA","#DIV/0!",""))

dim(training)
dim(testing)
```

```{r}
# Data cleansing, remove 1st 7 columns and any that contain NA.
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

training <- training[, -c(1:7)]
testing <- testing[, -c(1:7)]
dim(training)
dim(testing)
```
```{r}
# partitioning the datasets
traningPartitionData <- createDataPartition(training$classe,  p = 0.7, list = F)
trainingDataSet <- training[traningPartitionData, ]
testingDataSet <- training[-traningPartitionData, ]
dim(training); dim(testingDataSet)
```

## Prediction model 1 - decision tree

```{r}
decisionTreeModel <- rpart(classe ~ ., data = trainingDataSet, method = "class")
decisionTreePrediction <- predict(decisionTreeModel, testingDataSet, type = "class")

# Plot Decision Tree
rpart.plot(decisionTreeModel, main = "Decision Tree", under = T, faclen = 0)
```

```{r}
# test results using confusion matrix
confusionMatrix(decisionTreePrediction, testingDataSet$classe)
```
## Prediction Model2: random forest

```{r}
randomForestModel <- randomForest(classe ~. , data = trainingDataSet, method = "class")
randomForestPrediction <- predict(randomForestModel, testingDataSet, type = "class")

confusionMatrix(randomForestPrediction, testingDataSet$classe)
```


## Results

We conclude that the Random Forest accuracy is higher than Decision tree which is 0.993 > 0.7546. 


