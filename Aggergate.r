library(reshape2)

train$hour = as.numeric(train$datetime$hour)

#pivot table

WeekHour=aggregate(count ~ + hour+ weekday, data =train, FUN=mean)

#ggplot2

library(ggplot2)

#Line chart

ggplot(WeekHour, aes(x=hour, y=count)) + geom_line(aes(group=weekday, color=weekday),size=2,alpha=0.5)

#Heat map

ggplot(WeekHour, aes(x=hour, y=weekday)) + geom_tile(aes(fill = count))+ scale_fill_gradient(low="white", high="red")