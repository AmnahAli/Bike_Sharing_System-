library(xts)
library(gbm)
library(ggplot2)
library(ggdendro)
library(lubridate)
library(plyr) 
library(dplyr)
library(randomForest)
library(scales)
library(caret)
library(readr)


train<-read.csv("train.csv",stringsAsFactors=FALSE,header=TRUE)
str(bike_train)
nrow(bike_train)


train$season <- factor(train$season, c("spring","summer","fall","winter"), ordered=FALSE)
train$holiday <- factor(train$holiday, c("not a holiday","a holiday"), ordered=FALSE)
#train$workingday <- factor(train$workingday, c(0,1), ordered=FALSE)
train$weekday <- factor(train$weekday, c("Monday","Tuesday","Wednesday","Thursday","Friday",
                                         "Saturday","Sunday"), order=FALSE) #order factors
train$weather <- factor(train$weather, c("heavy","light","mist","clear"), ordered=FALSE)




train$season <- factor(train$season, c(1,2,3,4), ordered=FALSE)
train$holiday <- factor(train$holiday, c(0,1), ordered=FALSE)
#train$workingday <- factor(train$workingday, c(0,1), ordered=FALSE)
train$weekday <- factor(train$weekday, order=TRUE, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday",
                                                              "Saturday","Sunday")) #order factors
train$weather <- factor(train$weather, c(1,2,3,4), ordered=FALSE)


bike_train$datetime <- strptime(bike_train$datetime,"%Y-%m-%d %H:%M:%S")
bike_train$datetime[1:10]
class(bike_train$datetime)
bike_train$date <- "0"
bike_train$time <- "0"
bike_train$date<-format(bike_train$datetime,"%d-%m-%d")
bike_train$time<-format(bike_train$datetime,"%H:%M:%S")
train$weekday <- as.POSIXlt(train$datetime)$wday
train$hour <- as.POSIXlt(train$datetime)$hour
train$year <- as.POSIXlt(train$datetime)$year
train$weekday <- weekdays(as.Date(bike_train$workingday))
sapply(bike_train,function(x) sum(is.na(x)))
apply(is.na(bike_train),2,sum)
colSums(is.na(bike_train))
plot(bike_train$temp,bike_train$atemp, main="temp vs. atemp",xlab="Temperature (temp)",ylab="Feels like Temperature (atemp)")

 layout(matrix(c(1,1,2,3,4,5),2,3,byrow=FALSE))
 layout(matrix(c(1,2,3),byrow=FALSE))
layout(matrix(c(1,1,2,2,3,3), 1, 3, byrow = TRUE), respect = TRUE)
layout(matrix(1:4, 2, 2))
layout(matrix(1:1, 1, 1))

boxplot(train$count ~ train$season, outcol="blue",medcol="blue",border="black", ylab="count")
boxplot(train$count ~ train$holiday,outcol="blue",medcol="blue",border="black", ylab="count")
boxplot(train$count ~ train$weather,outcol="blue",medcol="blue",border="black", ylab="count")
boxplot(train$count ~ train$weekday,outcol="blue",medcol="blue",border="black", ylab="count")

table(train$holiday)



boxplot(train$count ~ train$hour ,  outcol="blue",medcol="blue",border="black", xlab="hour",ylab="count")
boxplot(train$casual ~ train$hour , main="bike rent by hour")
boxplot(train$registered ~ train$hour , main="bike rent by hour")

layout(matrix(1:3, 1, 3))

boxplot(train$count,outcol="blue",medcol="blue",border="black", ylab="Total count",xlab="(a)")
boxplot(train$casual,outcol="blue",medcol="blue",border="black", ylab="Casual count",xlab="(b)")
boxplot(train$registered,outcol="blue",medcol="blue",border="black",ylab="Registered count",xlab="(c)")

 boxplot(train$count ~ train$weather, main="Count vs. Weather",xlab="weather",ylab="count")
#train$weekday <- weekdays(train$datetime)
#boxplot(train$counts ~ train$week, main="workinday")
 
 summary(train$count)
 
 
 boxplot(train$count ~ train$workingday, main="workingday",ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
 boxplot(train$count ~ train$season, main="Count vs. Season",xlab="season",ylab="count")
 boxplot(train$count ~ train$holiday, main="Count vs. Holiday",xlab="holiday",ylab="count")
 boxplot(train$count ~ train$weather, main="Count vs. Weather",xlab="weather",ylab="count")
 boxplot(train$count ~ train$weekday, main="Count vs. Workingdays",xlab="workingdays",ylab="count")
 par(mfrow=c(2,2)) 
 plot(bike_train$temp,bike_train$counts, main="temperature",xlab="temp",ylab="count")
plot(bike_train$temp,bike_train$counts, main="feelslike temperature",xlab="atemp",ylab="count")
 plot( bike_train$humidity,bike_train$counts , main="humidity",xlab="humidity",ylab="count")
 plot( bike_train$windspeed,bike_train$counts , main="windspeed",xlab="windspeed",ylab="count")
# How the other variables - holiday, season, working day and time have an impact needs to be explored.
plot(bike_train$temp,bike_train$atemp)
plot(bike_train$temp,bike_train$atemp)
plot(bike_train$temp,bike_train$atemp)
#Let us take only three variables at first - humidity, temperature and wind-speed and find out the prediction estimates first. We will examine the other variables  later.
plot(bike_train$count)
#With,  y = temp + windspeed + humidity model resulted with R-squared:  0.162,
#which is very low.

#How can we improve the model? We have to consider other variables in the dataset:
#  1. Weekday
#2. Season
#3. Time
#4. Holiday


#Prediction – Model 1
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

#Further, datetime variable contains both date and time. For the sake of understanding the data, we will divide the datetime into two variables - date and time. And also, let’s add another variable to include the day of the week - Monday, Tuesday, etc.

bike_train$date <- "0"
bike_train$time <- "0"
bike_train$date<-format(bike_train$datetime,"%Y-%m-%d")
bike_train$time<-format(bike_train$datetime,"%H:%M:%S")
bike_train$weekday <- as.POSIXlt(bike_train$date)$wday
str(bike_train)

#Plot the bike sharing activities. From the plot, it can be seen that sharing activities has increased in the year 2012 (both registered and casual).
bike_train$date<-as.Date(bike_train$date)
plot(bike_train$date,bike_train$registered,col="blue")
plot(bike_train$date,bike_train$casual,col="blue")

bike_train$year<-as.integer(format(bike_train$date,"%Y"))
bike_train$year<- as.factor(bike_train$year)



#Following plot shows the weekdays activities and seasonal activities.
#It is clear that the bike sharing activities are more during the weekdays (Monday - Friday) than over the holidays and weekends. Season 4 has slightly more activities. Also, there are fewer activities during holidays.

#Let's further look at hourly activity. Again, 'datetime' variable should be split into hours. Thanks to 'R', we can do the same using the following functions:

###Breaking down Hours
bike_train$hour<- as.integer(substr(bike_train$time,1,2))
bike_train$hour<- as.factor(bike_train$hour)

#Plot hourly sharing activity:

plot(bike_train$hour,bike_train$registered,col="red")

#As you can see from the plot, the maximum activity is between 7AM and 9AM  and in the evenings between 5PM to 7PM.

#Based on the above analysis, we can include season, weekday, year, and hour, variables in our linear equation and rebuild our model once again.

lm.fit<-lm(registered ~ temp + humidity + windspeed + weekday + season + year + hour, data=bike_train)
summary(lm.fit)
plot(lm.fit)



lm.fit2 ->lm(formula = casual ~ temp + humidity + windspeed + season +
     weekday + year + hour, data = bike_train)
plot(lm.fit2)





