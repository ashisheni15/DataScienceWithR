library(dplyr)
library(rpart)
library(caTools)
library(randomForest)
library(caret)
library(dummies)

train <- read.csv("train_LZdllcl.csv", stringsAsFactors = FALSE)
test <- read.csv("test_2umaH9m.csv", stringsAsFactors = FALSE)

head(train)
str(train)
colnames(train)
colnames(test)
test$is_promoted <- NA

combi <- rbind(train,test)
str(combi)

summary(combi)
sapply(combi, function(x) sum(is.na(x)))

table(combi$department)
table(combi$region)
table(combi$education) # Blank 3443
table(combi$gender)
table(combi$recruitment_channel)
table(combi$previous_year_rating)

categorisation <- combi %>% filter(education!="") %>% select(department,gender,region,education) %>%group_by(department,gender,region,education) %>% 
    summarise(table(department,gender,region,education))	
colnames(categorisation) <- c("department","gender","region","education","count")
categorisation <- arrange(categorisation,department,gender,region,education)

new = aggregate(count~ department + gender + region,categorisation, max)
max = merge(new,categorisation)

max <- max[!duplicated(max[c("department","gender","region")]),]



for (i in 1:nrow(combi)) {
    if(combi[i,]$education == "")
        combi[i,]$education = max[which(max$department==combi[i,]$department &
                                          max$gender==combi[i,]$gender &
                                          max$region==combi[i,]$region),]$education
}

colSums(is.na(combi))

table(combi[which(is.na(combi$previous_year_rating)),]$length_of_service)

table(combi$previous_year_rating)

combi[which(is.na(combi$previous_year_rating)),]$previous_year_rating <- 3

str(combi)
combi$department <- as.factor(combi$department)
combi$region <- as.factor(combi$region)
combi$education <- as.factor(combi$education)
combi$gender <- as.factor(combi$gender)
combi$recruitment_channel <- as.factor(combi$recruitment_channel)
combi$awards_won. <- as.factor(combi$awards_won.)
combi$KPIs_met..80. <- as.factor(combi$KPIs_met..80.)
combi$previous_year_rating <- as.factor(combi$previous_year_rating)

test <- combi[which(is.na(combi$is_promoted)),]
train <- combi[which(!is.na(combi$is_promoted)),]
str(train)

dummy <- train
dummy <- cbind(dummy, dummy(train$education, sep = "_"))
dummy <- cbind(dummy, dummy(train$region, sep = "_"))
dummy <- cbind(dummy, dummy(train$gender, sep = "_"))
dummy <- cbind(dummy, dummy(train$department, sep = "_"))
dummy <- cbind(dummy, dummy(train$previous_year_rating, sep = "_"))
dummy <- cbind(dummy, dummy(train$recruitment_channel, sep = "_"))
name <- colnames(dummy)
name












#Building Model
##########################################################################################
#Logistic Regression
fit_lr <- glm(is_promoted ~ department + education +
                  no_of_trainings + age +
                  previous_year_rating + length_of_service + KPIs_met..80. + 
                  awards_won. + avg_training_score, data = train , family = binomial)

summary(fit_lr)
predict_lr <- predict(fit_lr, type = 'response')
accuracy_lr <- table(train$is_promoted, predict_lr > 0.4)
sum(diag(accuracy_lr))/sum(accuracy_lr)
precision_lr <- diag(accuracy_lr) / rowSums(accuracy_lr)
recall_lr <- diag(accuracy_lr) / colSums(accuracy_lr)
F1_lr <- 2*((precision_lr*recall_lr)/(precision_lr+recall_lr))
train$predict_lr <- predict_lr

predict <- predict(fit_lr,newdata = test, type = 'response')
test$predict <- ifelse(predict  > 0.4,1,0)

output.df = as.data.frame(test$employee_id)
output.df$is_promoted = ifelse(predict  > 0.2,1,0)
write.csv(output.df,file = "Submission.csv",row.names = FALSE)






















##########################################################################################
#Decision Tree

fit_dt <- rpart(is_promoted ~ department + region + education + gender +
                    recruitment_channel + no_of_trainings + age +
                    previous_year_rating + length_of_service + KPIs_met..80. + 
                    awards_won. + avg_training_score, data = train ,method="class")
summary(fit_dt)
predict_dt <- predict(fit_dt, type = 'class')
accuracy_dt <- table(training.set$is_promoted, predict_dt)
sum(diag(accuracy_dt))/sum(accuracy_dt)
precision_dt <- diag(accuracy_dt) / rowSums(accuracy_dt)
recall_dt <- diag(accuracy_dt) / colSums(accuracy_dt)
F1_dt <- 2*((precision_dt*recall_dt)/(precision_dt +recall_dt))
predict <- predict(fit_dt,newdata = test, type = 'class')

output.df = as.data.frame(test$employee_id)
output.df$is_promoted = ifelse(predict>0.4,1,0)
write.csv(output.df,file = "Submission_dt.csv",row.names = FALSE)
##########################################################################################
#RandomForest

fit_rf = randomForest(is_promoted ~ department + region + education + gender +
                                recruitment_channel + no_of_trainings + age +
                                previous_year_rating + length_of_service + KPIs_met..80. + 
                                awards_won. + avg_training_score, data = train ,
                                ntree=500,ntry=3,nodesize=0.01*nrow(train))

summary(fit_rf)
predict_rf <- predict(fit_rf, type = 'class')
accuracy_rf <- table(training.set$is_promoted, predict_rf > 0.5)
sum(diag(accuracy_rf))/sum(accuracy_rf)
precision_rf <- diag(accuracy_rf) / rowSums(accuracy_rf)
recall_rf <- diag(accuracy_rf) / colSums(accuracy_rf)
F1_rf <- 2*((precision_rf*recall_rf)/(precision_rf +recall_rf))

predict <- predict(fit_rf,newdata = test, type = 'class')

output.df = as.data.frame(test$employee_id)
output.df$is_promoted = ifelse(predict>0.4,1,0)
write.csv(output.df,file = "Submission_rf.csv",row.names = FALSE)
