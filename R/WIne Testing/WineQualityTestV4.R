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
#str(data)

#Split the data
#library('caTools')
#library('e1071')
#library('ggplot2')

set.seed(123)
split <- sample.split(data$category, SplitRatio = 0.75)
train_set <- subset(data, split == T)
test_set <- subset(data, split == F)

#Standard Scaling
train_set[, -12] <- scale(train_set[, -12])
test_set[, -12] <- scale(test_set[, -12])

#library('caret')
#classifier <- train(form=category~.,train_set, method="rf")
classifier <- train(form= category~.,data=train_set, method="svmRadial")
