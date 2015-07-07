# I tried to test this one for RF but tooooooo much :()  let see the result form Kaggle    0.85141 
#This one is IMPORTANT FILE

# Project: Predict Bike Sharing Demand using R
# Author : Gopinath Munisifreddy
# Date   : Aug-18-2014
#

#Set your working folder - CHANGE THIS PATH APPROPRIATELY
setwd("D:\\data-science\\bikesharing\\")

#Loading training file as well as test file - CHANGE THIS PATH APPROPRIATELY


#install required libraries
install.packages("lubridate") 
install.packages('randomForest')
library(lubridate)
library(randomForest)

train <- read.csv("train.csv",stringsAsFactors=FALSE,header=TRUE)
train$datetime <- strptime(train$datetime, format="%Y-%m-%d %H:%M:%S")
train$datetime <- strptime(train$datetime, format="%Y-%m-%d %H:%M:%S")
train$weekday <- as.POSIXlt(train$datetime)$wday
train$hour <- as.POSIXlt(train$datetime)$hour
train$year <- as.POSIXlt(train$datetime)$year

test <- read.csv("test.csv",stringsAsFactors=FALSE,header=TRUE)
test$datetime <- strptime(test$datetime, format="%Y-%m-%d %H:%M:%S")
test$weekday <- as.POSIXlt(test$datetime)$wday
test$hour <- as.POSIXlt(test$datetime)$hour
test$year <- as.POSIXlt(test$datetime)$year


test$count<-0


fit <- lm(count ~ season + holiday + weather + weekday + hour + temp + atemp + humidity + windspeed + year , data=train, ntree=500, mtry=5, importance=TRUE)

#Uncomment the following line if you want to see how your model plot looks like
#varImpPlot(fit)

#Predict values and save output
Prediction <- predict(fit, test)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)






#Let us take only three variables at first - humidity, temperature and wind-speed and find out the prediction estimates first. We will examine the other variables  later.
#######################################################################################
#With,  y = temp + windspeed + humidity model resulted with R-squared:  0.162,
#which is very low.

#How can we improve the model? We have to consider other variables in the dataset:
#  1. Weekday
#2. Season
#3. Time
#4. Holiday


#Prediction – Model 1  NOT GOOD AMNAH
#We can include temp, humidity and windspeed variables in our linear regression model:

lm.fit<-lm(registered ~ temp + humidity + windspeed,data=bike_train)
summary(lm.fit)
#the result: Residual standard error: 138.3 on 10882 degrees of freedom
#Multiple R-squared:  0.162, Adjusted R-squared:  0.1618
#F-statistic: 701.3 on 3 and 10882 DF,  p-value: < 2.2e-16
coefficients(lm.fit)
anova(lm.fit)
vcov(lm.fit)
layout(matrix(c(1,2,3,4),2,2))
plot(lm.fit)

#Prediction2
#The regression model with only 3 variables -temp, windspeed and humidity, yielded a predictive power with R-squared value of only 0.16. We have to consider other variables such as season, workingday, holiday, date, and time. It looks like we have to engineer some of the features ourselves.

#Workingday variable provides information only about whether that day is a working day or not? 0 indicates it is not a working day and 1 indicates it is a working day.

table(bike_train$workingday)
table(bike_train$holiday)


#Based on the above analysis, we can include season, weekday, year, and hour, variables in our linear equation and rebuild our model once again.

lm.fit<-lm(registered ~ temp + humidity + windspeed + weekday + season + year + hour, data=bike_train)
summary(lm.fit)
plot(lm.fit)
#Predict values and save output
Prediction <- predict(lm.fit, bike_test)   #0.49020 Kaggle score
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)


lm.fit2 <-lm(casual ~ temp + humidity + windspeed + season + weekday + year + hour, data = bike_train)
summary(lm.fit2)
plot(lm.fit2)
Prediction <- predict(lm.fit2, bike_test)   #0.49020 Kaggle score
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19C.csv", row.names = FALSE)
