# Big Mart Sales III practice problem from Analytics Vidhya
#
#https://datahack.analyticsvidhya.com/contest/practice-problem-big-mart-sales-iii/
#
# Initial analysis
# and cleaning of the data
#
# Some feature engineering
# and an analysis of the importance of various predictors
#
# we'll use the ggplot2 package for some plots
library(ggplot2)

# corrplot is needed for correlation plots
library(corrplot)

# we'll use plyr and dplyr to manipulate the data
library(plyr)
library(dplyr)
library(mice)

#we'll use caTools for splitting the training data to test and train the model
library(caTools)


#Importing the dataset
train <- read.csv('Train_UWu5bXk.csv')
test <- read.csv('Test_u94Q5KV.csv')

#Inspecting the data set
head(train)
head(test)

#check dimesions ( number of row & columns) in data sets
dim(train)
dim(test)

#check the variables and their types in train 
str(test)

# brief summary of train
summary(train)

# Cleaning and pre-processing the data for modeling
# We'll need to combine the train and test data
# We need to add a missing column in the test data

test$Item_Outlet_Sales <- 0

#Combining the test and train for treating the missing values
dataset <- rbind(train, test)

##############################################
#
#Analysing the different levels of fat contents
#
##############################################

table(dataset$Item_Fat_Content)
levels(dataset$Item_Fat_Content)

#By looking the levels, we can conclude that
# 'LF', 'low fat', 'Low Fat' are the same
# 'reg','Regular' both are same

dataset$Item_Fat_Content <- revalue(dataset$Item_Fat_Content,
                                    c("LF" = "Low Fat","low fat" = "Low Fat","reg" = "Regular"))

#Counting the fat content for each item

fat <- as.data.frame(setNames(aggregate(dataset$Item_Fat_Content,
                                        by = list(Category = dataset$Item_Type,
                                                  Category = dataset$Item_Fat_Content),
                                        FUN = length),
                              c("Item_Type", "Item_Fat_Content", "number")))
fat

# Looking at the data we can say that fat content is also assigned to the non-food items
# which is of no sense. So we will introduce a new level of "None" for those items

levels(dataset$Item_Fat_Content) <- c(levels(dataset$Item_Fat_Content), "None")


dataset[which(dataset$Item_Type == "Health and Hygiene"), ]$Item_Fat_Content <- "None"
dataset[which(dataset$Item_Type == "Household"), ]$Item_Fat_Content <- "None"
dataset[which(dataset$Item_Type == "Others"), ]$Item_Fat_Content <- "None"

dataset$Item_Fat_Content <- factor(dataset$Item_Fat_Content)
str(dataset)

#Again Counting the fat content for each item
fat <- as.data.frame(setNames(aggregate(dataset$Item_Fat_Content,
                                        by = list(Category = dataset$Item_Type,
                                                  Category = dataset$Item_Fat_Content),
                                        FUN = length),
                              c("Item_Type", "Item_Fat_Content", "number")))
fat

##############################################
#
#Analysing the different levels of fat contents
#
##############################################
table(dataset$Outlet_Size)
levels(dataset$Outlet_Size)

#We can see that we have 4 levels of outset_size but 1st level is blank.
#We will replace the blank outlet_size as "Other"

levels(dataset$Outlet_Size)[1] <- "Other"


######################################################
#
# We will check for any missing values in the 
# data set and then treat them
#
######################################################

#Checking the missing value
table(is.na(dataset))

#We have multiple missing value, loook for the column having missing value

colSums(is.na(dataset))

#We have identified that Item_Weight have the only missing value and have lot.

#Now visualise the Item_Weight with others
# boxplot of weights vs Item type
ggplot(dataset, aes(Item_Type, Item_Weight)) +
  geom_boxplot(color="red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, color = "blue")) +
  xlab("Item Type") +
  ylab("Item Weight") +
  ggtitle("Item Weight vs Item Type")


# boxplot of weights vs. Outlet Identifier

ggplot(dataset, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot(color="orange") +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,color = "purple")) +
  xlab("Outlet_Identifier") +
  ylab("Item Weight") +
  ggtitle("Item Weight vs Outlet identifier")

#From Item Weight vs Outlet identifier visuals, we concluded that 
#OUT019 & OUT027 doesnot contribute to the weight

#We can depect from the data that each item weight is identified by the unique 
#item_dentifier and have the same weight, So we nee to create a data of each 
#item identifier with their respective weight

weightsByItem <- as.data.frame(ddply(na.omit(dataset),
                                     ~ Item_Identifier,
                                     summarise,
                                     mean = mean(Item_Weight),
                                     sd = sd(Item_Weight),
                                     median = median(Item_Weight)))
weightsByItem
#Now we will use these value to replac the missing weights
dataset$Item_Weight <- ifelse(is.na(dataset$Item_Weight),
                              weightsByItem$mean[match(dataset$Item_Identifier,
                                                       weightsByItem$Item_Identifier)],
                              dataset$Item_Weight)

#Again we will check for the missing values
table(is.na(dataset))
colSums(is.na(dataset))

#Now we dont have any missing values



# let's redo the plots we looked at earlier
# boxplot of weights vs Item type
ggplot(dataset, aes(Item_Type, Item_Weight)) +
  geom_boxplot(color="red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, color = "blue")) +
  xlab("Item Type") +
  ylab("Item Weight") +
  ggtitle("Item Weight vs Item Type")


# boxplot of weights vs. Outlet Identifier

ggplot(dataset, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot(color="orange") +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,color = "purple")) +
  xlab("Outlet_Identifier") +
  ylab("Item Weight") +
  ggtitle("Item Weight vs Outlet identifier")
# Now we can see that the weight is contibuted by all outlets equally.

#################################################
#We will create a dummy variable Year based on the age of the stores

max(dataset$Outlet_Establishment_Year)
dataset$Year <- as.factor(2009-dataset$Outlet_Establishment_Year)

#Now we can drop the Outlet_Establishment_Year variable from the data set

dataset <- select(dataset,-c(Outlet_Establishment_Year))

##############################################################
#Now analyse the item_MRP

summary(dataset$Item_MRP)

ggplot(dataset, aes(x=Item_MRP))+ 
  geom_density(color = "blue",adjust =1/5)+
  geom_vline(xintercept = 69) +
  geom_vline(xintercept = 136)+
  geom_vline(xintercept = 203)+
  ggtitle("Density of Item MRP")

#Here we can clearly see tere are 4 categories of price
#so we will introduce one dummy variable fo this category

dataset$MRP_Category <- as.factor(
  ifelse(dataset$Item_MRP < 69, "Low",
         ifelse(dataset$Item_MRP < 136, "Medium",
                ifelse(dataset$Item_MRP < 203, "High", "Very_High")))
  )


####################################################
#We have introduced a other category in outlet_size
#We need to treat them for correct category
#
####################################################
str(dataset)

#We will look at the frequency of each outlet_identifier

aggregate(dataset$Outlet_Identifier, by = list(category = dataset$Outlet_Identifier, 
                                               category = dataset$Outlet_Type), FUN = length)

#From the above data we can clearly say that the outlet OUT010 & OUT019 have less data compared to others
#We will look at the frequency of each item_identifier
aggregate(dataset$Item_Identifier, by = list(category = dataset$Outlet_Identifier, 
                                               category = dataset$Outlet_Type), FUN = length)

#Here we het the exaclty the same data a above so we can say that the grocery stores
#have small number of items and also the stores are less than supermarkets


# boxplot of  Sales vs. Outlet Type
ggplot(dataset[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet type")

# count the number of outlet per Outlet_Identifier ,Outlet_Type & Location
otherOutlet <- as.data.frame( setNames(
  aggregate(
    dataset$Outlet_Size, 
    by=list(Category=dataset$Outlet_Identifier, 
            Category=dataset$Outlet_Type,
            Category=dataset$Outlet_Location_Type,
            Category=dataset$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))

otherOutlet
#By looking at the data of otherOutlet we can clearly conclude that
# Grocery Stores belongs to Small outlet size
# Supermarket Type1 of  Tier 2 location type should belongs to Small

dataset[ which(dataset$Outlet_Identifier == "OUT010") ,]$Outlet_Size <- "Small"
# "OUT017" and "OUT045" could be small
dataset[ which(dataset$Outlet_Identifier == "OUT017") ,]$Outlet_Size <- "Small"
dataset[ which(dataset$Outlet_Identifier == "OUT045") ,]$Outlet_Size <- "Small"

levels(dataset$Outlet_Size)
table(dataset$Outlet_Size)

#Now we will remove the other level
dataset$Outlet_Size <- factor(dataset$Outlet_Size)

#########################################################################
# There are number of data in the item_visibility is 0 which is certainly wrong
#we will impute those 0 with NA and then treat them for visibility
#############################################################################

table(dataset$Item_Visibility)
dataset[which(dataset$Item_Visibility == 0),]$Item_Visibility <- NA


# boxplot of Visibility vs Item type
ggplot(dataset, aes(Item_Type, Item_Visibility, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Item Type")

# boxplot of Visibility vs. Outlet Identifier
ggplot(dataset, aes(Outlet_Identifier, Item_Visibility)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")


TotVis <- as.data.frame(setNames(
  aggregate(na.omit(dataset)$Item_Visibility, by=list(Category=na.omit(dataset)$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

TotVis

#Creating a new subset for predicting the item visibility based on 
#the regression model

newdata <- na.omit(dataset)
newdata <- select(newdata,-c(Item_Outlet_Sales))
newdata.test <- subset(dataset,is.na(Item_Visibility))

names(newdata)

model.item <-
  lm(
    Item_Visibility ~ Item_Identifier + Item_Weight + Item_Fat_Content +
      Item_Type + Item_MRP + Outlet_Identifier + Outlet_Size +
      Outlet_Location_Type + Outlet_Type + Year + MRP_Category,
    data = newdata
  )

pred <- predict(model.item, newdata = newdata.test)

newdata.test$Item_Visibility <- pred
##########################################################
#Inputing the missing values with the predicted value
##########################################################

outind <- levels(newdata.test$Outlet_Identifier)
itemind <- levels(newdata.test$Item_Identifier)

for(out in outind){
  for(item in itemind){
    dataset[which(dataset$Item_Identifier == item &
                    dataset$Outlet_Identifier == out),]$Item_Visibility <- 
      ifelse(is.na(dataset[which(dataset$Item_Identifier == item &
                                   dataset$Outlet_Identifier == out),]$Item_Visibility),
             newdata.test[which(newdata.test$Item_Identifier == item &
                                  newdata.test$Outlet_Identifier == out),]$Item_Visibility,
             dataset[which(dataset$Item_Identifier == item &
                             dataset$Outlet_Identifier == out),]$Item_Visibility)
  }
}

dataset$Item_Visibility <- abs(dataset$Item_Visibility)

###################################################
##Making the Item_visibility 100% per outlet
##################################################
outNameind <- levels(dataset$Outlet_Identifier)
for (outName in outNameind) {
  dataset[ which(dataset$Outlet_Identifier == outName),]$Item_Visibility <-
    dataset[ which(dataset$Outlet_Identifier == outName),]$Item_Visibility *
    100/TotVis[ which(TotVis$Outlet_Identifier == outName),]$TotVis
}

TotVis <- as.data.frame(setNames(
  aggregate(na.omit(dataset)$Item_Visibility, by=list(Category=na.omit(dataset)$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

TotVis

##############################################################################
#
#
#
##############################################################################

ggplot(dataset[1:nrow(train),],aes(x=Item_Type , y= Item_Outlet_Sales))+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, color = "blue")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")

# We have a lots of outlier here, We can reduce it by dividing the sales by mrp

dataset$Item_Outlet_Sales <- dataset$Item_Outlet_Sales/dataset$Item_MRP

cor(dataset[1:nrow(train),][sapply(dataset[1:nrow(train),], is.numeric)])

names(dataset)

dataset <- select(dataset,c(Item_Identifier,      
                            Item_Weight,     
                            Item_Fat_Content,
                            Item_Visibility,     
                            Item_Type,            
                            Item_MRP,             
                            Outlet_Identifier,
                            Outlet_Size,   
                            Outlet_Location_Type,
                            Outlet_Type,
                            Year,       
                            MRP_Category,
                            Item_Outlet_Sales))

train <- dataset[1:nrow(train),]
test <- dataset[-(1:nrow(train)),]

str(train)

#model <- lm(Item_Outlet_Sales ~ Item_Identifier + Item_Weight + Item_Fat_Content +
 #             Item_Visibility + Item_Type + Item_MRP + Outlet_Identifier +
  #            Outlet_Size + Outlet_Location_Type + Outlet_Type + Year + MRP_Category ,
   #         data = train)
#summary(model)


########################################################################3
##########################################################################
#Using Decision Tree (Score : 1244)
##########################################################################
library(rpart)
library(rpart.plot)

model.dt <- rpart(Item_Outlet_Sales ~ Item_Identifier + Item_Weight + Item_Fat_Content +
                    Item_Visibility + Item_Type + Item_MRP + Outlet_Identifier +
                    Outlet_Size + Outlet_Location_Type + Outlet_Type + Year + MRP_Category ,
                   data = train)
summary(model.dt)

rpart.plot(model.dt)

pred <- predict(model.dt, newdata = test)
test$Item_Outlet_Sales <- pred

test$Item_Outlet_Sales <- test$Item_Outlet_Sales* test$Item_MRP


solution <- test[,c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")]


write.csv(solution,file = "Solution.csv",row.names = FALSE)
####################################################################
####################################################################
#Using Random forest (Score : 1166.9)
##########################################################################
library(randomForest)

model.rf <- randomForest(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content +
                           Item_Visibility + Item_Type + Item_MRP + Outlet_Identifier +
                           Outlet_Size + Outlet_Location_Type + Outlet_Type + Year + MRP_Category ,
                         data = train, ntree=100)

pred <- predict(model.rf, newdata = test)

test$Item_Outlet_Sales <- pred

test$Item_Outlet_Sales <- test$Item_Outlet_Sales* test$Item_MRP


solution <- test[,c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")]


write.csv(solution,file = "Solution_rf.csv",row.names = FALSE)

####################################################################
####################################################################


###################################################################
####Using Regression
####################################################################
####################################################################

combi <- dataset
str(combi)

combi$Item_Fat_LF <- ifelse(combi$Item_Fat_Content=="Low Fat",1,0)
combi$Item_Fat_Reg <- ifelse(combi$Item_Fat_Content=="Regular",1,0)

combi<- select(combi,-c(Item_Fat_Content))
combi.train <- combi[1:nrow(train),]
combi.test <- combi[-(1:nrow(train)),]


model.lm <- lm(Item_Outlet_Sales ~ Item_Weight + 
                 Item_MRP + Outlet_Identifier,
               data = combi.train)
summary(model.lm)

pred<- predict(model.lm, newdata =test )


test$Item_Outlet_Sales <- pred

test$Item_Outlet_Sales <- test$Item_Outlet_Sales* test$Item_MRP


solution <- test[,c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")]
write.csv(solution,file = "Solution_reg_2.csv",row.names = FALSE)
