---
title: "Prediction Assignment Writeup"
author: "Theoneste"
date: "10/13/2020"
output:
  html_document: default
  pdf_document: default
---
## Overview
This document is the final report of the Peer Assessment project from Coursera’s course Practical Machine Learning at Johns Hopkins University.

The goal of this project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. I have may use any of the other variables to predict with. I created this a report describing how i have  built my model and how I used cross validation. Here is how I was expected out of sample error is, I used prediction model to predict 20 different test cases.

## Required Packages 

Here are all packages i will need in this assignment and I want to setup the enviroment 

```{r}
library(knitr)
library(caret)
library(rpart)
library(e1071)
library(rpart.plot)
library(rattle)
library(randomForest)
library(corrplot)
set.seed(333)

```

## Getting Data and Loading Data

This is Data i have used in this assignment
```{r}
# Download the data
Trai_Url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
Tes_Url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(Trai_Url, destfile = "pml-training.csv")

download.file(Tes_Url, destfile = "pml-testing.csv")

#Reading the Data
training <- read.table("pml-training.csv", sep = ",", header = TRUE)
testing <- read.table("pml-testing.csv", sep = ",", header = TRUE)
```
NOw we need to split the original training set into our training set and a validation set

```{r}
inTrain <- createDataPartition(y=training$classe, p=0.7, list=F)
trai_one <- training[inTrain, ]
trai_two <- training[-inTrain, ]
```
Now we may see the dimension of our data

```{r}
dim(trai_one)
dim(trai_two)
```

As we see  both datasets have 160 variables and trai_one has 13737 observations and trai_two has 5885 observations. 

Now let us removes all  near-zero variance predictors

```{r}
nzv <- nearZeroVar(training)
trai_one <- trai_one[, -nzv]
trai_two <- trai_two[, -nzv]
```
Removing with NA values

```{r}
trai_one <- trai_one[, colSums(is.na(trai_one)) == 0]
trai_two <- trai_two[, colSums(is.na(trai_two)) == 0]
```
Removing identification only variables (columns 1 to 5)

```{r}
trai_one <- trai_one[, -(1:5)]
trai_two  <- trai_two[, -(1:5)]
```
Since we have cleaned our data we can continue with analysis.
## Correlation analysis

This correlation analysis between the variables is necessary before any analysis to avoid any unrelated conclusion 

```{r}
corMatrix <- cor(trai_one[, -54])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", tl.cex = 0.8, tl.col = rgb(0, 0, 0))
```
The highly correlated variables are shown in dark colors in the graph above. To make an even more compact analysis, a PCA (Principal Components Analysis) could be performed as pre-processing step to the datasets. Nevertheless, as the correlations are quite few, this step will not be applied for this assignment.

## Choissing Model

We have to deal we different regression models to select for our data. Let us two most popular models which are decision tree and random forest. 

## Decision Tree Model
```{r}
set.seed(1813)
ct <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
model1  <- train(classe ~ ., data = trai_one, method = "rf",trControl = ct, verbose = FALSE)
model1$finalModel
```

Predictions of the Random Forest model on test_set

```{r}
pred1 <- predict(fit_RF, newdata = trai_two)
conf_matrix_RF <- confusionMatrix(pred1, trai_two$classe)
conf_matrix_RF
```
The predictive accuracy of the Random Forest model is excellent at 99.8 % and The random Forest model is selected and applied to make predictions on the 20 data points from the original testing dataset (testing)
```{r}
predict_quiz <- predict(fit_RF, newdata = data_quiz)
predict_quiz
```