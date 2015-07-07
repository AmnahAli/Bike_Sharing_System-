
#install required libraries
library(lubridate)


train <- read.csv("train.csv",stringsAsFactors=FALSE,header=TRUE)
train$datetime <- strptime(train$datetime, format="%Y-%m-%d %H:%M:%S")
train$date<-format(train$datetime,"%d-%m-%d")
train$time<-format(train$datetime,"%H:%M:%S")
train$weekday <- as.POSIXlt(train$datetime)$wday
train$hour <- as.POSIXlt(train$datetime)$hour
train$year <- as.POSIXlt(train$datetime)$year
train$month <- as.POSIXlt(train$datetime)$month

test <- read.csv("test.csv",stringsAsFactors=FALSE,header=TRUE)
test$datetime <- strptime(test$datetime, format="%Y-%m-%d %H:%M:%S")
test$date<-format(test$datetime,"%d-%m-%d")
test$time<-format(test$datetime,"%H:%M:%S")
test$weekday <- as.POSIXlt(test$datetime)$wday
test$hour <- as.POSIXlt(test$datetime)$hour
test$year <- as.POSIXlt(test$datetime)$year
test$month <- as.POSIXlt(test$datetime)$month


test$count<-0


train$season <- factor(train$season)
train$holiday <- factor(train$holiday)
train$weekday <- factor(train$weekday)
train$weather <- factor(train$weather)
train$month <- factor(months(train$datetime)) 


test$season <- factor(test$season)
test$holiday <- factor(test$holiday)
test$weekday <- factor(test$weekday)
test$weather <- factor(test$weather)
test$month <- factor(months(test$datetime))   



fit <- lm(count ~ season + holiday + weather + weekday + hour + temp + atemp + humidity + windspeed + year , data=train)
summary(lm.fit)
plot(lm.fit)
Prediction <- predict(fit, test)  # Kaggle 1.21280   [1]
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)




#Based on the above analysis, we can include season, weekday, year, and hour, variables in our linear equation and rebuild our model once again.

lm.fit<-lm(count ~  season + weather+ temp + atemp +  humidity + windspeed + hour, data=train)
summary(lm.fit)
plot(lm.fit)
#Predict values and save output
Prediction <- predict(lm.fit, test)   #1.21015 Kaggle score  [2]    Very GOOD
Prediction <- abs(Prediction)         #After Factorize  	1.20754
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)
--------------------------------------
library(stats)
library(ROCR)

preds <- predict(as.numeric(lm.fit), as.numeric(test))
#preds <- prediction(as.numeric(hall$log.pred), as.numeric(hall$count))

perf <- performance(preds, "tpr", "fpr")

perf2 <- performance(preds, "auc")
perf2

plot(perf, main="ROC Curve for Hall of Fame Predictions", lwd=2, col="darkgreen")

text(0.5, 0.5, "AUC = 0.9855472", cex=2.2, col="darkred")

----------------------------------------------------

lm.fit<-lm(count ~ temp +  windspeed + atemp + season + weather+ hour, data=train)
summary(lm.fit)
plot(lm.fit)
#Predict values and save output
Prediction <- predict(lm.fit, test)   #1.23278 Kaggle score  [2] [2]   
Prediction <- abs(Prediction)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)


 

lm.fit<-lm(count ~ workingday + year + month + hour, data=train)
summary(lm.fit)
plot(lm.fit)
#Predict values and save output
Prediction <- predict(lm.fit, test)   #1.31761 Kaggle score  [2] [4]   
Prediction <- abs(Prediction)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)



lm.fit<-lm(log(count) ~ temp + humidity + windspeed + atemp + season + weather+ hour, data=train)
summary(lm.fit)
plot(lm.fit)
#Predict values and save output
Prediction <- predict(lm.fit, test)   # 	3.13072 Kaggle score  [2 log ]
Prediction <- abs(Prediction)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)




lm.fit<-lm(count ~ temp+ season+ weekday +weather+hour, data=train)
summary(lm.fit)
plot(lm.fit)
#Predict values and save output
Prediction <- predict(lm.fit, test)   # 	3.13072 Kaggle score  [2 2 log ]
Prediction <- abs(Prediction)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)






#Based on the above analysis, we can include season, weekday, year, and hour, variables in our linear equation and rebuild our model once again.

lm.fit<-lm(count ~ temp + year + weekday + hour, data=train)
summary(lm.fit)
plot(lm.fit)
#Predict values and save output
Prediction <- predict(lm.fit, test)   #1.24892 Kaggle score  [3]
Prediction <- abs(Prediction)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)



fit <- lm(count ~ season + weather + weekday + hour + temp + year , data=train)
summary(lm.fit)
plot(lm.fit)
Prediction <- predict(fit, test)  # Kaggle 1.25053    [4]
Prediction <- abs(Prediction)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)




fit <- lm(count ~ -- + -- + -- + -- + -- + -- , data=train)
summary(lm.fit)
plot(lm.fit)
Prediction <- predict(fit, test)  # Kaggle ------    [5]
Prediction <- abs(Prediction)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19.csv", row.names = FALSE)



lm.fit1 <-lm(casual ~ temp + atemp + humidity + windspeed + season + weather + weekday + year + hour, data = train)
lm.fit2 <-lm(registered ~ temp + atemp + humidity + windspeed + season + weather + weekday + year + hour, data = train)
summary(lm.fit2)
summary(lm.fit1)
plot(lm.fit2)
Prediction1 <- predict(lm.fit1, test)   # 	 1.21326   Kaggle score  [6]
Prediction2 <- predict(lm.fit2, test)
Prediction <- abs(Prediction1 + Prediction2)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19C.csv", row.names = FALSE)



lm.fit1 <-lm(casual ~ temp + humidity + windspeed + atemp + season + weather+ hour, data = train)
lm.fit2 <-lm(registered ~ temp + humidity + windspeed + atemp + season + weather+ hour, data = train)
summary(lm.fit1)
summary(lm.fit2)
plot(lm.fit2)
Prediction1 <- predict(lm.fit1, test)   # 	 1.21015   Kaggle score  [7]   Equal with [2]  This one the better
Prediction2 <- predict(lm.fit2, test)
Prediction <- abs(Prediction1 + Prediction2)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19C.csv", row.names = FALSE)





lm.fit1 <-lm(casual ~ season + holiday + weather + weekday + temp + atemp + humidity + windspeed + year + hour, data = train)
lm.fit2 <-lm(registered ~ season + holiday + weather + weekday + temp + atemp + humidity + windspeed + year + hour, data = train)
summary(lm.fit2)
plot(lm.fit2)
Prediction1 <- predict(lm.fit1, test)   # 	  1.21280    Kaggle score  [8]   
Prediction2 <- predict(lm.fit2, test)
Prediction <- abs(Prediction1 + Prediction2)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19C.csv", row.names = FALSE)


lm.fit1 <-lm(casual ~ season +  weather + temp + windspeed + year + hour, data = train)
lm.fit2 <-lm(registered ~ season +  weather + weekday + temp  + windspeed + year + hour, data = train)
summary(lm.fit2)
plot(lm.fit2)
Prediction1 <- predict(lm.fit1, test)   # 	  1.25492    Kaggle score  [9]   

Prediction2 <- predict(lm.fit2, test)
Prediction <- abs(Prediction1 + Prediction2)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19C.csv", row.names = FALSE)




lm.fit1 <-lm(casual ~ temp +  windspeed +  season + weather+ hour, data = train)
lm.fit2 <-lm(registered ~ temp +  windspeed +  season + weather+ hour, data = train)
summary(lm.fit2)
plot(lm.fit2)
Prediction1 <- predict(lm.fit1, test)   # 	 1.23283   Kaggle score  [10]   
Prediction2 <- predict(lm.fit2, test)
Prediction <- abs(Prediction1 + Prediction2)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "19C.csv", row.names = FALSE)



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


train$season <- factor(train$season
                       ,levels = c(1,2,3,4)
                       ,labels = c("spring", "summer", "fall", "winter")
)

train$holiday <- factor(train$holiday
                           ,levels = c(0,1)
                           ,labels = c("non a holiday", "a holiday")
)

train$weather <- factor(train$weather
                        ,levels = c(4,3,2,1)
                        ,labels = c("very bad", "bad", "good", "very good")
                        ,ordered = TRUE)




train$month <- factor(months(train$datetime)
                      ,levels = c("January"
                                  ,"February"
                                  ,"March"
                                  ,"April"
                                  ,"May"
                                  ,"June"
                                  ,"July"
                                  ,"August"
                                  ,"September"
                                  ,"October"
                                  ,"November"
                                  ,"December")
                      ,ordered = TRUE)

boxplot(train$month)
