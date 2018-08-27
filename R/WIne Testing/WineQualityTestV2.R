
#using Caret package to predit from KNN
data = read.csv("winequality-red.csv")

quality <- c(data$quality)

category <-character()


for( x in quality){
  if(x < 7)
  {
    category=append(category, "Bad") 
  } 
  else{
    category= append(category,"Good")
  }
}

data$quality <- NULL
data$category <- category

data$category <- factor(data$category, levels = c("Good", "Bad"), labels = c(1,0))

library('caret')

set.seed(123)
#Data Partition,
split <- createDataPartition(y=data$category, p=0.75, list = FALSE)
train_set <- data[split,]
test_set <- data[-split,]
#Change to category 


#Train with KNN
#library('e1071')
ctrl <-trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(456)
knn_fit <- train(category~.,data=train_set, method="knn", 
                 trControl=ctrl,
                 preProcess=c("center", "scale"),
                 tuneLength=10 )




y_pred <- predict(knn_fit, test_set)

confusionMatrix(test_set$category,  y_pred)

#Accuracy= 88%


