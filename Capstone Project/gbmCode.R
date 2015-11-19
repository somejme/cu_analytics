library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(gbm)

train <- read.csv("~/GitHub/cu_analytics/Capstone Project/train.csv")
test <- read.csv("~/GitHub/cu_analytics/Capstone Project/test.csv")

PREDICT = TRUE
data=train
if(PREDICT){data=test}

set.seed(420)

########### Feature Engineering #####################
data$Name = as.character(data$Name)
data$Ticket = as.character(data$Ticket)
data$Cabin = as.character(data$Cabin)
data$Child <- 0
data$Child[data$Age < 18] <- 1
data$Fare2 <- 'high'
data$Fare2[data$Fare < 100 & data$Fare >= 40] <- 'mid'
data$Fare2[data$Fare < 40 & data$Fare >= 10] <- 'low'
data$Fare2[data$Fare < 10] <- 'vlow'
data$Fare2 = as.factor(data$Fare2)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare2 + Embarked, data=data[!is.na(data$Age),], method="anova")
data$Age[is.na(data$Age)] <- predict(Agefit, data[is.na(data$Age),])

########### EDA ############
# if(!PREDICT){
#   table(train$Survived)
#   prop.table(table(train$Survived))
#   table(train$Sex)
#   prop.table(table(train$Sex, train$Survived),2)
#   summary(train$Age)
#   summary(train$Fare)
#   aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
#   fancyRpartPlot(Agefit)
# }

?hist
hist(data[data$Fare<50,]$Fare,breaks = 100)
# str(data)
str(data)
table(data$Parch)


if(!PREDICT){
  fit = gbm(Survived ~ Pclass + Sex + Fare2 + Age + SibSp + Parch + Embarked,
            data=data, distribution = "bernoulli", n.trees = 8000, interaction.depth = 4, shrinkage = 0.0005, n.cores = 2)
  gbm.perf(fit,plot.it = FALSE)
}


if(PREDICT){
  Prediction <- predict(fit, data, n.trees=6102, type="response")
  Prediction = ifelse(Prediction<0.45,0,1)
  submit <- data.frame(PassengerId = data$PassengerId, Survived = Prediction)
  write.csv(submit, file = "submission.csv", row.names = FALSE)
          }


## Jamie's RF Stuff
library(ElemStatLearn)
library(MASS)
library(randomForest)
library(tree)
?randomForest

########### Feature Engineering #####################
train$Survived = as.factor(train$Survived)
levels(test$Sex) <- levels(train$Sex)
levels(test$Embarked) <- levels(train$Embarked)

rf_titanic = randomForest(Survived ~ Pclass + Sex + Fare + Age + SibSp + Parch + Embarked,
                          data=train, ntree=2000, mtry=3, na.action=na.omit, method = "class")
rf_predict = predict(rf_titanic, test, method ="class")
varImpPlot(rf_titanic)
plot(rf_titanic, log="y")
rfsubmit <- data.frame(PassengerId = data$PassengerId, Survived = rf_predict)
write.csv(rfsubmit, file = "rfsubmission.csv", row.names = FALSE)
