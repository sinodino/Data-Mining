# Data-Mining
Course projects of Data mining

## Homework 1

### Problem 1 

Descriptive analysis of facial recognition data from Yale Faces B Dataset http://vision.ucsd.edu/~leekc/ExtYaleDatabase/download.html

### Problem 2 

Determine the threshold for the good lighting condition.

## Homework 2

### Problem 1 

Manually go through all of the steps for PCA. Basic computations like finding the eigenvalues for a matrix may be done using R.

### Problem 2

Use PCA to find the "mean face", "eigen faces" and analysis on the result

## Homework 3

Problem 1 and 2 are theorectical problems.

### Problem 3

Manually go through all of the steps for kNN. The data set is hw03 q3.csv with 15000 observations and 101 objects. Use the first 10,000 observations as a training set and the last 5,000 observations as a testing set.

Write a functionknnregression(xtrain,ytrain,xtest,ytest,kvec) that takes the arguments: x train, a matrix of training covariates; y train, a vector of train- ing responses; x test, a vector of testing covariates; y test, a vector of testing responses; and kvec, a vector (or scalar) of k-values. The output should be a list of the following elements: train mse, a vector of training MSE for each value of k; and test mse, a vector of testing MSE for each value of k. Use Euclidean distance to find the nearest neighbors. Run this function on the data using only the first covariate, X1, and on the same graph plot the MSE for training and testing sets as a function of k for k={1, 5, 10, 50, 100, 500, 1000, 5000, 10000}.

### Problem 4

Use 1NN classification and PCA to do facial recognition based on the Yale Faces B dataset. Discuss the result of different approaches.
