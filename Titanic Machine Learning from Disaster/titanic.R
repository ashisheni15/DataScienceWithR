library(rpart)
library(rpart.plot)
library(caTools)
set.seed(123)

# Titanic Machine Learning
train <- read.csv("train.csv")
test <- read.csv("test.csv")

test$Survived <- NA
combi <- rbind(train,test)

head(combi)

namesplit <- function(x){
trimws(strsplit(as.character(x), split='[,.]')[[1]][2])
}

combi$Title <- sapply(combi$Name,namesplit)

combi$Title[combi$Title %in% c( 'the Countess', 'Mlle', 'Ms', 'Miss','Master') & combi$Sex == "female"] <- 'Miss'
combi$Title[combi$Title %in% c( 'Master') & combi$Sex == "male"] <- 'Master'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Col', 'Jonkheer', 'Rev', 'Dr') & combi$Sex == "male" ] <- 'Mr'
combi$Title[combi$Title %in% c('Dona',"Dr",'Lady',"Mme") & combi$Sex == "female"] <- 'Mrs'

table(combi$Title)
combi$Fare <- as.numeric(combi$Fare)
combi[is.na(combi$Fare),"Fare"] <- 0
combi[combi$Fare ==0.00,"Fare"] <- NA

combi[is.na(combi$Age) & combi$Title=="Miss","Age"]   <- mean(combi[combi$Title=="Miss","Age"],na.rm = TRUE)
combi[is.na(combi$Age) & combi$Title=="Master","Age"] <- mean(combi[combi$Title=="Master","Age"],na.rm = TRUE)
combi[is.na(combi$Age) & combi$Title=="Mr","Age"]     <- mean(combi[combi$Title=="Mr","Age"],na.rm = TRUE)
combi[is.na(combi$Age) & combi$Title=="Mrs","Age"]    <- mean(combi[combi$Title=="Mrs","Age"],na.rm = TRUE)


combi[is.na(combi$Fare) & combi$Pclass==3,"Fare"] <- mean(combi[combi$Pclass==3,"Fare"],na.rm = TRUE)
combi[is.na(combi$Fare) & combi$Pclass==2,"Fare"] <- mean(combi[combi$Pclass==2,"Fare"],na.rm = TRUE)
combi[is.na(combi$Fare) & combi$Pclass==1,"Fare"] <- mean(combi[combi$Pclass==1,"Fare"],na.rm = TRUE)

combi[combi$Embarked=="","Embarked"] <- "C"
table(combi$Embarked)

combi$Cabin <- substr(combi$Cabin,1,1)
combi[combi$Cabin=="NC" ,"Cabin"] <- NA

combi$Sex <- as.character(combi$Sex)
combi[combi$Sex=="female" ,"Sex"] <- 1
combi[combi$Sex=="male" ,"Sex"] <- 0
combi$Sex <- as.integer(combi$Sex)


combi$Family <- combi$SibSp + combi$Parch + 1
combi$Survived <- as.factor(combi$Survived)
combi$Pclass <- as.factor(combi$Pclass)
combi$Cabin <- as.factor(combi$Cabin)
combi$Title <- as.factor(combi$Title)

##########################################################################################
table(is.na(combi))

str(combi)
combi$Cabin <- as.factor(combi$Cabin)
combi$Title <- as.factor(combi$Title)
combi$Pclass <- as.factor(combi$Pclass)
combi$Survived <- as.factor(combi$Survived)

#######################Spliting the Combi#################################################

test <- combi[is.na(combi$Survived),]
train <- combi[!is.na(combi$Survived),]

##################Spliting the train set##################################################

split = sample.split(train$Survived,SplitRatio = 0.8)
training.set = subset(train,split=="TRUE")
test.set = subset(train,split=="FALSE")

#######################Model############################################################

str(training.set)

fit <- rpart(Survived ~ Pclass + Sex + Age + Cabin + Fare + SibSp + Parch +
                 + Embarked + Family + Title, data = training.set,method="class")
summary(fit)
predicted= predict(fit,test.set,type = "class")

tail(predicted)

test.set$Predicted <- predicted

accuracy <- table(test.set$Predicted,test.set$Survived)
sum(diag(accuracy))/sum(accuracy)

rpart.plot(fit)

library(RColorBrewer)
library(rattle)


######################################################################################

train$Survived <- as.factor(train$Survived)
test$Survived <- as.factor(test$Survived)


str(train)

########################Final Model###################################################
fit <- rpart(Survived ~ Pclass + Sex + Age +  Fare + Cabin + SibSp + Parch +
               + Embarked + Title, data = train,method="class")
predicted= predict(fit,test,type = "class")

PassengerId = test$PassengerId
output.df = as.data.frame(PassengerId)
output.df$Survived = predicted

tail(output.df)
write.csv(output.df,file = "kaggle_submission.csv",row.names = FALSE)
