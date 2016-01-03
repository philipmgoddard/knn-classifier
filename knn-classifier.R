dataPartition <- function(df, seedIn = 1234) {
  set.seed(seedIn)
  sample(nrow(df), floor(nrow(df) * 0.75), replace = FALSE)
}

euclDist <- function(x, y) {
  return(sqrt(sum(x - y) ^ 2))
}

knnClassifier <- function(covTrain, covTest, outcomeTrain, k = 5) {
  
  # keep a copy of original for output, will use normalised data for 
  # fitting though
  meanTrain <- lapply(covTrain, mean)
  sdTrain <- lapply(covTrain, sd)
  scaleTrain <- Map(function(x, y) {(covTrain - x) / y}, meanTrain, sdTrain)[[1]]
  scaleTest <- Map(function(x, y) {(covTest - x) / y}, meanTrain, sdTrain)[[1]]
  
  kClosest <- apply(scaleTest, 1, function(x) {
    tmp <- apply(scaleTrain, 1, function(y) euclDist(x, y))
    order(tmp)[1:k]
  })
  
  outcomeClass <- apply(kClosest, 2, function(x) {
    tmpTab <- table(outcomeTrain[x])
    testClass <- names(which(tmpTab == max(tmpTab)))[1]
  })
  
  out <- list(training = cbind(covTrain, outcomeTrain),
              test = cbind(covTest, outcomeClass),
              predClass = outcomeClass)
  return(out)
}

########################
# test the classifier

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

knnResults[[2]]$correct <- ifelse(results$predClass == results$actual, 1, 0)

library(ggplot2)
ggplot(knnResults[[2]], aes(x = Petal.Length, 
                            y = Petal.Width,
                            color = factor(correct),
                            shape = factor(outcomeClass))) +
  geom_point(alpha = 0.6, size = 4) +
  theme_bw() +
  scale_color_manual(values = philTheme()[c(1, 4)], name = "Correct", guide = FALSE) +
  scale_shape_manual(values=c(15, 17, 19), name = "Species") +
  theme(legend.position = c(0.1, 0.8),
        legend.text.align = 0) 


