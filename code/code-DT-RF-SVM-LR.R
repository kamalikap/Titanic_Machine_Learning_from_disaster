library('ggplot2')
library("caret")
library("dplyr")
library('randomForest')
library("rpart")
library("rpart.plot")
library('car')
library('e1071')
library("scales")
train.tit<-read.csv('/Users/kamalika/Desktop/cs235/project/train.csv', stringsAsFactors = F)
test.tit<-read.csv ('/Users/kamalika/Desktop/cs235/project/test.csv',stringsAsFactors = F)
test.tit$Survived<- NA
full_titanic<- rbind(train.tit,test.tit)
full_titanic$Embarked[full_titanic$Embarked==""]="S"
apply(full_titanic,2,function(x) length(unique(x)))
cols=c("Survived","Pclass","Sex", "Embarked")


ggplot(full_titanic[1:891,],aes(x=Pclass,fill=factor(Survived))) +
  geom_bar(position="fill")+ 
  ggtitle("Pclass v/s Survival Rate") +
  xlab("Pclass") +
  ylab("Total Count")+ 
  labs(fill="Survived")


ggplot(full_titanic[1:891,],aes(x=Sex,fill=factor(Survived))) +
  geom_bar(position = "fill")+ 
  facet_wrap(~Pclass)+ 
  ggtitle("3D view of sex,pclass and survival")+ 
  xlab("Pclass") +
  ylab("Total Count")+ 
  labs(fill="Survived")


names<-full_titanic$Name
title<- gsub ("^.*, (.*?)\\..*$", "\\1", names)
full_titanic$title <- title
table(title)
full_titanic$title[full_titanic$title == 'Mlle']<- 'Miss' 
full_titanic$title[full_titanic$title == 'Ms']<- 'Mrs'
full_titanic$title[full_titanic$title == 'Mme']<- 'Mrs' 
full_titanic$title[full_titanic$title == 'Lady']<- 'Mrs'
full_titanic$title[full_titanic$title == 'Dona']<- 'Mrs'
full_titanic$title[full_titanic$title == 'Capt']<- 'Master' 
full_titanic$title[full_titanic$title == 'Col'] <- 'Master' 
full_titanic$title[full_titanic$title == 'Major']<- 'Master'
full_titanic$title[full_titanic$title == 'Dr']<- 'Mr'
full_titanic$title[full_titanic$title == 'Rev']<- 'Master'
full_titanic$title[full_titanic$title == 'Don']<- 'Mr'
full_titanic$title[full_titanic$title == 'Sir']<- 'Mr'
full_titanic$title[full_titanic$title == 'the Countess']<- 'Mr'
full_titanic$title[full_titanic$title == 'Jonkheer']<- 'Mr'

ggplot(full_titanic[1:891,],aes(x = title,fill=factor(Survived))) +
  geom_bar(position= "fill") +
  ggtitle("Title V/S Survival rate")+
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

ggplot(full_titanic[1:891,], aes(x = title, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  facet_wrap(~Pclass) + 
  ggtitle("3-way relationship of Title, Pclass, and Survival") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")   


#exploratory analysis on family

full_titanic$FamilySize <-full_titanic$SibSp + full_titanic$Parch + 1
full_titanic$FamilySized[full_titanic$FamilySize == 1]   <- 'Single'
full_titanic$FamilySized[full_titanic$FamilySize < 5 & full_titanic$FamilySize >= 2]   <- 'Small'
full_titanic$FamilySized[full_titanic$FamilySize >= 5]   <- 'Big'
full_titanic$FamilySized=as.factor(full_titanic$FamilySized)

ggplot(full_titanic[1:891,],aes(x = FamilySized,fill=factor(Survived))) +
  geom_bar(position = "fill") +
  ggtitle("Family Size V/S Survival Rate") +
  xlab("FamilySize") +
  ylab("Total Count") +
  labs(fill = "Survived")

ggplot(full_titanic[1:891,], aes(x = FamilySized, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  facet_wrap(~Pclass) + 
  ggtitle("3D View of Family Size, Pclass and Survival rate") +
  xlab("family.size") +
  ylab("Total Count") +
  labs(fill = "Survived")

#checking ticket

ticket.unique <- rep(0, nrow(full_titanic))
tickets <- unique(full_titanic$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full_titanic$Ticket == current.ticket)
  
  
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}

full_titanic$ticket.unique <- ticket.unique
full_titanic$ticket.size[full_titanic$ticket.unique == 1]<- 'Single'
full_titanic$ticket.size[full_titanic$ticket.unique < 5 & full_titanic$ticket.unique>= 2]<- 'Small'
full_titanic$ticket.size[full_titanic$ticket.unique >= 5]<- 'Big'

##Lets check the Ticket size through grpah
ggplot(full_titanic[1:891,],aes(x = ticket.size,fill=factor(Survived))) +
  geom_bar(position = "fill") +
  ggtitle("ticket.Size VS Survival")+
  xlab("ticket.size") +
  ylab("Total Count") +
  labs(fill = "Survived")


ggplot(full_titanic[1:891,], aes(x = ticket.size, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  facet_wrap(~Pclass) + 
  ggtitle("3D View of Ticket, Pclass and Survival rate") +
  xlab("ticket.size") +
  ylab("Total Count") +
  labs(fill = "Survived")



#exploratory analysis on Embarked

ggplot(full_titanic[1:891,],aes(x = Embarked,fill=factor(Survived))) +
  geom_bar(position = "fill") +
  ggtitle("Embarked vs Survival") +
  xlab("Embarked") +
  ylab("Total Count") +
  labs(fill = "Survived") 

ggplot(full_titanic[1:891,], aes(x = Embarked, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass vs Embarked vs survival") +
  xlab("Embarked") +
  ylab("Total Count") +
  labs(fill = "Survived")

full_titanic$ticket.size <- as.factor(full_titanic$ticket.size)
full_titanic$title <- as.factor(full_titanic$title)


#Divide data into train and set for internal validation
feature1<-full_titanic[1:891, c("Pclass", "title","Sex","Embarked","FamilySized","ticket.size")]
response <- as.factor(train.tit$Survived)
feature1$Survived=as.factor(train.tit$Survived)
set.seed(500)
ind=createDataPartition(feauter1$Survived,times=1,p=0.8,list=FALSE)
train_val=feature1[ind,]
test_val=feature1[-ind,]
round(prop.table(table(train.tit$Survived)*100),digits = 1)



#Predictive Analysis and Cross Validation
#decision tree
set.seed(1234)
Model_DT=rpart(Survived~.,data=train_val,method="class")
rpart.plot(Model_DT,extra =  3,fallen.leaves = T)
PRE_TDT=predict(Model_DT,data=train_val,type="class")
confusionMatrix(PRE_TDT,train_val$Survived)
set.seed(1234)
cv.10 <- createMultiFolds(train_val$Survived, k = 10, times = 10)

# Control
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     index = cv.10)
##Train the data
Model_CDT <- train(x = train_val[,-7], y = train_val[,7], method = "rpart", tuneLength = 30,
                   trControl = ctrl)
rpart.plot(Model_CDT$finalModel,extra =  3,fallen.leaves = T)
PRE_VDTS=predict(Model_CDT$finalModel,newdata=test_val,type="class")
confusionMatrix(PRE_VDTS,test_val$Survived)


#random forest
set.seed(1234)
rf.1 <- randomForest(x = train_val[,-7],y=train_val[,7], importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)
train_val1=train_val[,-4:-5]
test_val1=test_val[,-4:-5]
set.seed(1234)
rf.2 <- randomForest(x = train_val1[,-5],y=train_val1[,5], importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)
set.seed(2348)
cv10_1 <- createMultiFolds(train_val1[,5], k = 10, times = 10)

# Set up caret's trainControl object per above.
ctrl_1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv10_1)
set.seed(1234)
rf.5<- train(x = train_val1[,-5], y = train_val1[,5], method = "rf", tuneLength = 3,
             ntree = 1000, trControl =ctrl_1)
rf.5
pr.rf=predict(rf.5,newdata = test_val1)
confusionMatrix(pr.rf,test_val1$Survived)

