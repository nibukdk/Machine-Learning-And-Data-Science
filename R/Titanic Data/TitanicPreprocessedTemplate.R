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

#Lets Explore Data
#library(ggplot2)
ggplot(x, aes(Pclass)) + geom_bar()


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


ageVsSurvived <-
  ggplot(train_data, aes(Age, fill = factor(Survived))) +
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
  ) + facet_grid(Sex ~ .)


