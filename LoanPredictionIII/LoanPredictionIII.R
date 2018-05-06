# Loan Prediction III practice problem from Analytics Vidhya
#
#https://datahack.analyticsvidhya.com/contest/practice-problem-loan-prediction-iii/
#
# Initial analysis
# and cleaning of the data
#
# Some feature engineering
# and an analysis of the importance of various predictors
#
# we'll use the ggplot2 package for some plots
library(ggplot2)
library(data.table)
#we'll use caTools for splitting the training data to test and train the model
library(caTools)

#Load the train & test data from csv 
train <- fread("train_u6lujuX_CVtuZ9i.csv")
test <- fread("test_Y3wMUE5_7gLdaTN.csv")
class(train)

#Have a look at the data
head(train)

#Look at the summery of tha dataset
summary(train)

#Combining the train & test for cleaning 
#addingthe target variable in test dataset with NA
test$Loan_Status <- NA
combi <- rbind(train,test)

#Chack the structure of the each variable in dataset
str(combi)

## Check number of uniques values for each of the column to find out columns
#which we can convert to factors
sapply(combi,function(x) length(unique(x)))


#Converting the Gender, Married, Dependent, Education, Self_Employed, Credit_history, 
#Property_Area, Loan_Status to factor
combi <- as.data.frame(combi)

for(i in c("Gender", "Married", "Dependents", "Education", "Self_Employed", "Credit_History", 
           "Property_Area", "Loan_Status")){
  combi[,i] <- as.factor(combi[,i])
}

#Getting deeper into the granuality of data
colSums(is.na(combi))
#we have multiple missing values in LoanAmount, Loan_Amount_Term, Credit_History
#we have to treat these missing values

#Looking at the different independent valiable sperately
levels(combi$Gender)
ggplot(data = combi, aes(x=Gender,fill = Gender))+geom_bar()
levels(combi$Gender)[1] <- NA

#Looking at the married variable
levels(combi$Married)

ggplot(data = combi, aes(x=Married,fill = Married))+geom_bar()
levels(combi$Married)[1] <- NA


#Looking at Education
levels(combi$Education)
ggplot(data = combi, aes(x=Education,fill = Education))+geom_bar()

#Looking at Self_Employed
levels(combi$Self_Employed)
ggplot(data = combi, aes(x=Self_Employed,fill = Self_Employed))+geom_bar()

levels(combi$Self_Employed)[1]<- NA

#Looking at Dependents
levels(combi$Dependents)
ggplot(data = combi, aes(x=Dependents,fill = Dependents))+geom_bar()

levels(combi$Dependents)[1]<- NA

combi[is.na(combi$Dependents),"Dependents"] <- median(as.numeric(combi$Dependents),na.rm = TRUE)

#Looking at Property_Area

levels(combi$Property_Area)
ggplot(data = combi, aes(x=Property_Area,fill = Property_Area))+geom_bar()

#Looking at Property_Area

levels(combi$Credit_History)
ggplot(data = combi, aes(x=Credit_History,fill = Credit_History))+geom_bar()

combi[is.na(combi$Credit_History),"Credit_History"] <- 
  
median(as.numeric(combi$Credit_History),na.rm = TRUE)

as.numeric(combi$Credit_History)
