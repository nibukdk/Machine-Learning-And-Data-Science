
library(class)
library(ggplot2)

classifier <-
  knn(
    train = train_data_with_cabin[, c(-11, -4)],
    test = train_data_without_cabin[, c(-11, -4)],
    cl = train_data_with_cabin[, 11],
    k = 8
  )


#EDA
sexVsSurvived <-
  ggplot(train_data, aes(Sex, fill = factor(Survived))) + xlab('Sex') + ylab('Count') + ggtitle("Surival by Sex") +
  scale_x_discrete(labels = c('Male', 'Female')) +
  scale_fill_discrete(
    name = "Legend",
    breaks = c(0, 1),
    labels = c('Died', 'Survived')
  ) +
  geom_bar(aes(Sex, fill = factor(Survived)), position = "dodge") +
  theme(
    axis.title.x = element_text(color = "Red"),
    axis.title = element_text(family = "Courier"),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(color = "Red"),
    axis.text.y = element_text(size = 20) ,
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    plot.title = element_text(
      size = 20,
      colour = "DarkBlue",
      hjust = 0.5
    )
  )

pClassVsSurvival <-  ggplot(train_data) +
  geom_bar(aes(Pclass, fill = factor(Survived)), position = "dodge") +
  theme(
    axis.title.x = element_text(color = "Red"),
    axis.title = element_text(family = "Courier"),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(color = "Red"),
    axis.text.y = element_text(size = 20) ,
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    plot.title = element_text(
      size = 20,
      colour = "DarkBlue",
      hjust = 0.5
    )
  ) +
  xlab('Passenger Class') +
  ylab('Number of passenger') +
  
  ggtitle("Surival by Sex") +
  scale_fill_discrete(
    name = "Legend",
    breaks = c(0, 1),
    labels = c('Died', 'Survived')
  )

parchVsSurvival <-  ggplot(train_data) +
  geom_bar(aes(factor(Parch), fill = factor(Survived)), position = "dodge") +
  theme(
    axis.title.x = element_text(color = "Red"),
    axis.title = element_text(family = "Courier"),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(color = "Red"),
    axis.text.y = element_text(size = 20) ,
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    plot.title = element_text(
      size = 20,
      colour = "DarkBlue",
      hjust = 0.5
    )
  ) +
  xlab('Parents And Children') +
  ylab('Number of passenger') +
  
  ggtitle("Surival by Sex") +
  scale_x_discrete(labels = c('0', '1', '2', '3', '4', '5', '6')) +
  scale_fill_discrete(
    name = "Legend",
    breaks = c(0, 1),
    labels = c('Died', 'Survived')
  )


ageVsSurvived <-  ggplot(train_data, aes(Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 10, color = "black") +
  theme(
    axis.title.x = element_text(color = "Red"),
    axis.title = element_text(family = "Courier"),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(color = "Red"),
    axis.text.y = element_text(size = 20) ,
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    plot.title = element_text(
      size = 20,
      colour = "DarkBlue",
      hjust = 0.5
    )
  ) +
  xlab('Age group') +
  ylab('Number of passenger') +
  
  ggtitle("Surival By Age") +
  #(labels = c('0', '1', '2', '3', '4', '5', '6')) +
  scale_fill_discrete(
    name = "Legend",
    breaks = c(0, 1),
    labels = c('Died', 'Survived')
  )

destinationVsSurvival <- ggplot(train_data, aes(Embarked, fill = factor(Survived))) +
  geom_bar( color = "black") +
  theme(
    axis.title.x = element_text(color = "Red"),
    axis.title = element_text(family = "Courier"),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(color = "Red"),
    axis.text.y = element_text(size = 20) ,
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = c(0.2,1),
    legend.justification = c(1, 1),
    plot.title = element_text(
      size = 20,
      colour = "DarkBlue",
      hjust = 0.5
    )
  ) +
  xlab('Age group') +
  ylab('Number of passenger') +
  
  ggtitle("Destination vs Survival") +
  scale_x_discrete(labels = c(" Cherbourg", "Queenstown", "Southampton")) +
  scale_fill_discrete(
    name = "Legend",
    breaks = c(0, 1),
    labels = c('Died', 'Survived')
  )

#Lets predic Cabin for data first
library(caTools)
library(caret)
train_data_with_cabin$Fare = scale(train_data_with_cabin$Fare)
set.seed(123)
split = sample.split(train_data_with_cabin$Cabin, SplitRatio=0.7)
train_set_for_Cabin = subset(train_data_with_cabin, split==T)
test_set_for_cabin = subset(train_data_with_cabin, split==F)

library(e1071)
# classifier = svm(Cabin~., 
#                  data= train_set_for_Cabin,
#                  kernel='radial')
classifier <- svm(Cabin~.,data=train_set_for_Cabin, method="radial")

y_pred= predict(classifier, test_set_for_cabin[-4])   

confusion_matrix = table(test_set_for_cabin$Cabin, y_pred)
train_data_without_cabin$Fare = scale(train_data_without_cabin$Fare)

y_pred2= predict(classifier, train_data_without_cabin[-4]) 
confusion_matrix2 = table(train_data_without_cabin$Cabin, y_pred2)
train_data_without_cabin$Cabin = y_pred2


#CabinVsPclass <-
ggplot(train_data_without_cabin, aes(Cabin, fill=)) +
  geom_bar( ) +
  theme(
    axis.title.x = element_text(color = "Red"),
    axis.title = element_text(family = "Courier"),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(color = "Red"),
    axis.text.y = element_text(size = 20) ,
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = c(1,1),
    legend.justification = c(1, 1),
    plot.title = element_text(
      size = 20,
      colour = "DarkBlue",
      hjust = 0.5
    )
  ) +
  xlab('Cabin') +
  ylab('Number of passenger') +
  
  ggtitle("Cabin Vs Pclass") +
  #scale_x_discrete(labels = c(" Cherbourg", "Queenstown", "Southampton")) +
  scale_fill_discrete(
    name = "Legend"
    #breaks = c(0, 1),
    #labels = c('Died', 'Survived')
  )


