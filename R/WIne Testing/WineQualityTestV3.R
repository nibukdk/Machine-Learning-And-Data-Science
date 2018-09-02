data <- read.csv('winequality-red.csv')

quality <- c(data$quality)

category <- character()

#for()



for (x in quality) {
  if (x <= 5)
  {
    category = append(category, "Bad")
  }
  else if (x >= 6 & x <= 7) {
    category = append(category, "Good")
  }
  else{
    category = append(category, "Excellent")
  }
}


data$quality <- NULL
data$category <- category

data$category <-
  factor(
    data$category,
    levels = c("Excellent", "Good", "Bad"),
    labels = c(1, 2, 3)
  )
str(data)

#Split the data
#library('caTools')
#library('e1071')
#library('ggplot2')
#library('caret')
set.seed(123)
split <- sample.split(data$category, SplitRatio = 0.75)
train_set <- subset(data, split == T)
test_set <- subset(data, split == F)

#Standard Scaling
train_set[, -12] <- scale(train_set[, -12])
test_set[, -12] <- scale(test_set[, -12])


##Application of Grid Search
folds <- createFolds(train_set$category, k = 10)
cv <- lapply(folds, function(x) {
  train_fold <- train_set[-x, ]
  test_fold <- train_set[x, ]
  classifer <-  svm(
    formula = category ~ .,
    data = train_fold,
    type = "C-classification",
    kernel = "radial"
  )
  y_pred <- predict(classifer, test_fold)
  cm <- table(test_fold$category,y_pred)
  accuracy <- (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
  
  return(accuracy)
  
})

accuracy<- mean(as.numeric(cv))

#Hence accuracy of this Algoeithm is 97% 





