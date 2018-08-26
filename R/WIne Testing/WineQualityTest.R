data = read.csv("winequality-red.csv")

quality <- c(data$quality)

category <-character()

#for()


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

#One hot encoding
data$category <- factor(data$category, levels = c("Good", "Bad"), labels = c(1,0))
str(data)

# Datatest split
#library("caTools")

set.seed(123)
split <- sample.split(data$category,SplitRatio = 0.8)
train_set <- subset(data,split==T)
test_set <- subset(data, split==F)

#Standard Scaling
train_set[,-12] <- scale(train_set[,-12])
test_set[,-12] <- scale(test_set[,-12])


#Apply KNN algorith first
#library(class)

y_pred <- knn(train_set[,-12], test_set[,-12], cl=train_set[,12], k=5)

#Confusion Matrix

cm <- table(test_set[,12],y_pred)











