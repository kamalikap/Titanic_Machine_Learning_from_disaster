setwd("~/Desktop/cs235/project")

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <- NA
 
titanic.full <- rbind(titanic.train,titanic.test) 

titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'

age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

#clean missing values of fare
#fare.median <- median(titanic.full$Fare, na.rm = TRUE)
#titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

#cabin.median <- median(titanic.full$Cabin, na.rm = TRUE)
#titanic.full[is.na(titanic.full$Cabin), "Cabin"] <- cabin.median



# regression for fare- least mean square method
upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker
titanic.full[outlier.filter,]


fare.equation = "Fare ~Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(formula= fare.equation,data = titanic.full[outlier.filter,])


fare.row <-titanic.full[is.na(titanic.full$Fare),c("Pclass","Sex","Age","SibSp","Parch","Embarked")]
fare.predictions <-predict(fare.model, newdata = fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"]<- fare.predictions

#categorical casting(every columnn except survive because there are three columns in survive-1,0,NA)
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#split dataset back out into train and test 

titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,] 
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,] 

titanic.train$Survived <- as.factor(titanic.train$Survived)     

survived.equation <- "Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula= survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 *nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

ggplot(output.df[1:417,], aes(x = fare.predictions, fill = factor(Survived))) +
  geom_bar( position = "dodge") +
  ggtitle("Fare Prediction for test data ") +
  xlab("Fare")+
  ylab("Total Count") +
  labs(fill = "Survived")

write.csv(output.df, file= "kaggle_submission2.csv", row.names = FALSE)
