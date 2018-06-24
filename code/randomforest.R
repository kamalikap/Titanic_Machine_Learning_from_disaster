 setwd("~/Desktop/cs235/project")

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

#cleaning Together
median(titanic.train$Age)
median(titanic.train$Sex)
median(titanic.train$SibSp)
median(titanic.train$Ticket)
median(titanic.train$Age,na.rm= TRUE)
median(titanic.test$Age,na.rm= TRUE)


titanic.train$IsTrainSet <- TRUE
tail(titanic.train&IsTrainSet)
titanic.test$IsTrainSet <- FALSE

ncol(titanic.train)
ncol(titanic.test)
names(titanic.train)
names(titanic.test)


titanic.test$Survived <- NA
 
titanic.full <- rbind(titanic.train,titanic.test) 
tail(titanic.full)
table(titanic.full$IsTrainSet)
table(titanic.full$Embarked)

titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'
table(titanic.full$Embarked)
table(is.na(titanic.full$Age))

age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median
table(is.na(titanic.full$Age))


#clean missing values of fare
fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median
table(is.na(titanic.full$Fare))
table(titanic.full$Survived)


cabin.median <- median(titanic.full$Cabin, na.rm = TRUE)
titanic.full[is.na(titanic.full$Cabin), "Cabin"] <- cabin.median
#categorical casting(every columnn except survive because there are three columns in survive-1,0,NA)
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#split dataset back out into train and test 
str(titanic.full)

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
ggplot(output.df[1:417,], aes(x = age.median, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  ggtitle("Random Forest-Age vs survival") +
  xlab("Age")+
  ylab("Total Count") +
  labs(fill = "Survived")

write.csv(output.df, file= "kaggle_submission.csv", row.names = FALSE)
