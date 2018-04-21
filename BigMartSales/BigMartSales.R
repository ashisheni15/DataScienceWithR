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

#check the variables and their types in train & test
str(train)
str(test)

# brief summary of train & test
summary(train)
summary(test)

# Cleaning and pre-processing the data for modeling
#We'll need to combine the train and test data
#We need to add a missing column in the test data

test$Item_Outlet_Sales <- 0

#Combining the test and train for treating the missing values
dataset <- rbind(train, test)

colSums(is.na(dataset))


levels(dataset$Item_Fat_Content)
# "LF"      "low fat" "Low Fat" "reg"     "Regular"
dataset$Item_Fat_Content <- revalue(dataset$Item_Fat_Content,
                                    c(
                                      "LF" = "Low Fat",
                                      "low fat" = "Low Fat",
                                      "reg" = "Regular"
                                    ))


fat <- as.data.frame(setNames(
  aggregate(
    dataset$Item_Fat_Content,
    by = list(
      Category = dataset$Item_Type,
      Category = dataset$Item_Fat_Content
    ),
    FUN = length
  ),
  c("Item_Type", "Item_Fat_Content", "number")
))
fat



levels(dataset$Item_Fat_Content) <-
  c(levels(dataset$Item_Fat_Content), "None")


table(dataset$Item_Type)

dataset[which(dataset$Item_Type == "Health and Hygiene"), ]$Item_Fat_Content <-
  "None"
dataset[which(dataset$Item_Type == "Household"), ]$Item_Fat_Content <-
  "None"
dataset[which(dataset$Item_Type == "Others"), ]$Item_Fat_Content <-
  "None"

dataset$Item_Fat_Content <- factor(dataset$Item_Fat_Content)
str(dataset)


#> levels(dataset$Outlet_Size)
#[1] ""       "High"   "Medium" "Small"
levels(dataset$Outlet_Size)[1] <- "Other"


# boxplot of weights vs Item type
ggplot(dataset, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(
    angle = 70,
    vjust = 0.5,
    color = "black"
  )) +
  xlab("Item Type") +
  ylab("Item Weight") +
  ggtitle("Item Weight vs Item Type")


# boxplot of weights vs. Outlet Identifier
ggplot(dataset, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(
    angle = 70,
    vjust = 0.5,
    color = "black"
  )) +
  xlab("Outlet_Identifier") +
  ylab("Item Weight") +
  ggtitle("Item Weight vs Outlet identifier")



weightsByItem <- as.data.frame(ddply(
  na.omit(dataset),
  ~ Item_Identifier,
  summarise,
  mean = mean(Item_Weight),
  sd = sd(Item_Weight)
))

dataset$Item_Weight <- ifelse(is.na(dataset$Item_Weight),
                              weightsByItem$mean[match(dataset$Item_Identifier,
                                                       weightsByItem$Item_Identifier)],
                              dataset$Item_Weight)

table(is.na(dataset))
colSums(is.na(dataset))



# let's redo the plots we looked at earlier
# boxplot of weights vs Item type
ggplot(dataset, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(
    angle = 70,
    vjust = 0.5,
    color = "black"
  )) +
  xlab("Item Type") +
  ylab("Item Weight") +
  ggtitle("Item Weight vs Item Type")

# boxplot of weights vs. Outlet Identifier
ggplot(dataset, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(
    angle = 70,
    vjust = 0.5,
    color = "black"
  )) +
  xlab("Outlet_Identifier") +
  ylab("Item Weight") +
  ggtitle("Item Weight vs Outlet identifier")
