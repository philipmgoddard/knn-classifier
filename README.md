# knn-classifier

Usage is:

```R
data("iris")
outcome <- iris[, 5]
data <- iris[, 3:4]

# data partition
trainRows <- dataPartition(data, seedIn = 5662)
outcomeTrain <- outcome[trainRows]
outcomeTest <- outcome[-trainRows]
dataTrain <- data[trainRows, ]
dataTest <- data[-trainRows, ]

# run the classifier
knnResults <- knnClassifier(dataTrain, dataTest, outcomeTrain, k = 11)

# confusion matrix of results
results <- data.frame(predicted = knnResults[3],
                      actual = outcomeTest)
table(results)
```

This is for education purposes, I suggest using the function in the caret package if you need to apply a robust k-nn algorithm.
