# Time Series Analysis practice problem from Analytics Vidhya
#
#https://datahack.analyticsvidhya.com/contest/practice-problem-time-series-2/
#
# Initial analysis
# and cleaning of the data
#
#
# Importing required library
library(ggplot2)
library(forecast)
library(tseries)
#we'll use caTools for splitting the training data to test and train the model
library(caTools)


#Importing the dataset
train <- read.csv('Train_SU63ISt.csv')
test <- read.csv('Test_0qrQsBZ.csv')

#Inspecting the data set
head(train)
head(test)

#check the variables and their types in train 
str(train)



train$Datetime <- as.Date(train$Datetime,format= '%d-%m-%Y')

ggplot(train, aes(Datetime, Count)) + geom_line() + scale_x_date('month')  + ylab("Count") +
  xlab("")

count_ts <- ts(train$Count)
count_ts

train$clean_count <- tsclean(count_ts)

ggplot(train, aes(Datetime, clean_count)) + geom_line() + scale_x_date('month')  + ylab("Clean Count") +
  xlab("")

train$weekly_ma <- ma(train$clean_count, order = 7)
train$monthly_ma <- ma(train$clean_count, order = 30)

ggplot()+
  geom_line(data = train , aes(x=Datetime, y= clean_count, colour = "Counts" )) +
  geom_line(data = train , aes(x=Datetime, y= weekly_ma, colour = "weekly Moving Avg" )) +
  geom_line(data = train , aes(x=Datetime, y= monthly_ma, colour = "Monthly Moving Avg" )) +
  ylab("Count")


count_ma <- ts(na.omit(train$weekly_ma), frequency = 30)
decomp <- stl(count_ma,s.window = "periodic")
deseasnl_count <- seasadj(decomp)
plot(decomp)


adf.test(count_ma, alternative = "stationary")


Acf(count_ma,main="")
Pacf(count_ma,main= "")


count_d1 = diff(deseasnl_count, differences = 1)
plot(count_d1)

adf.test(count_d1, alternative = "stationary")

auto.arima(deseasnl_count, seasonal=FALSE)
fit<-auto.arima(deseasnl_count, seasonal=FALSE)

fcast <- forecast(fit, h=5112)
plot(fcast)


acf(fit$residuals, lag.max=20)


test$count <- fcast
head(fcast)

f <- as.data.frame(fcast)
head(f)

test$count <- (f[,4] +f[,5])/2

sol <- test[,c(1,3)]
head(sol)

write.csv(sol,file = "Solution.csv",row.names = FALSE)
