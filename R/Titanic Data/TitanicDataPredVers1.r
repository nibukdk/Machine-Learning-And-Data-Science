#   This project is practice project for machine learning

train_data = read.csv('train.csv')
test_data = read.csv('test.csv')
train_data[, 'Cabin'] = as.character(train_data[, 'Cabin'])
#replace the availabe Cabin letters with their fist letters
train_data[, 'Cabin'] <-
  lapply(
    train_data[, 'Cabin', drop = F],
    FUN = function(x) {
      x = substr(x, 1, 1)
      
      
    }
  )
#Replace the missing Embarked with respect to Pclass and most occured Embarked with respect to class
# train_data$Embarked <- as.character(train_data$Embarked)

train_data[train_data$Embarked == "" , 'Embarked'] <-
  lapply(
    train_data[train_data$Embarked == "" , 'Embarked', drop = F],
    FUN = function(x) {
      x = "S"
      
      
    }
    
  )


#Fill missing age with mean
train_data$Age <- ifelse(is.na(train_data$Age),
                         ave(
                           train_data$Age ,
                           FUN =  function(x)
                             
                             round(mean(x, na.rm = T))
                         ),
                         train_data$Age)

train_data$Sex <-
  factor(
    train_data$Sex,
    levels = c('male', 'female'),
    labels = c('Male', 'Female')
  )
train_data$Survived <-
  factor(train_data$Survived)


train_data$Ticket <- factor(train_data$Ticket)
train_data$Cabin <- factor(train_data$Cabin)
train_data$Embarked <- factor(train_data$Embarked)
train_data$PassengerId <- factor(train_data$PassengerId)
train_data$Pclass <- factor(train_data$Pclass)


#train_data_with_cabin = train_data[train_data$Cabin != "",c('PassengerId','Pclass', 'Fare', 'Cabin') ]
# dataframe_with_Cabin$Fare= train_data$Fare[train_data$Cabin != ""]
# dataframe_with_Cabin$Fare = round(dataframe_with_Cabin$Fare, 1)

#train_data_without_cabin = train_data[train_data$Cabin == "", c('PassengerId','Pclass', 'Fare', 'Cabin')]

#Lets drop Unnecessary Columns
train_data$Name <- NULL
train_data$Ticket <- NULL
#Cabin has lots of missing data so dropping is good option
train_data$Cabin <- NULL


train_data$Fare <- scale(train_data$Fare)

str(train_data)


#library(caTools)
set.seed(123)
split <- sample.split(train_data$Survived, SplitRatio = 0.7)
data_for_training <- train_data[split == T,]
data_for_test <- train_data[split == F,]


classifier <- svm(Survived ~ .,
                  data = data_for_training,
                  type = "C-classification",
                  kernel = 'radial')

y_pred <- predict(classifier, data_for_test[-2])

confusionMatrix <- table(data_for_test[, 2], y_pred)

accuracy <-
  (confusionMatrix[1, 1] + confusionMatrix[2, 2]) / (confusionMatrix[1, 1] + confusionMatrix[1, 2] +
                                                       confusionMatrix[2, 1] + confusionMatrix[2, 2]) * 100
