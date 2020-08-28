Loading and preprocessing the data
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(ggplot2)
library(lubridate)
## 
## Attaching package: 'lubridate'
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
rawdata<-read.csv("activity.csv")
rawdata$date<-as.Date(rawdata$date)
What is mean total number of steps taken per day?
naomit_data<-na.omit(rawdata)
steps_sum<-tapply(naomit_data$steps, naomit_data$date, sum)
count_table<-data.frame(Date = names(steps_sum), Steps = steps_sum)
count_table$Date<-as.Date(count_table$Date)

hist(count_table$Steps, xlab = "Total Steps Taken per Day", main = "Histogram of Steps Taken per Day")


Mean steps per day
mean_steps<-mean(count_table$Steps)
print(mean_steps)
## [1] 10766.19
Median steps per day
median_steps<-median(count_table$Steps)
print(median_steps)
## [1] 10765
What is the average daily activity pattern?
avg_steps<-tapply(naomit_data$steps, naomit_data$interval, mean)
avgsteps_table<-data.frame(Interval = names(avg_steps), Average = avg_steps)

plot(avgsteps_table$Interval, avgsteps_table$Average, type = "l", xlab = "Interval", ylab = "Average Steps Taken", main = "Average Number of Steps Taken per 5 min Interval (Oct - Nov 2012)")


Interval with maximum steps
maxsteps_interval<-subset(avgsteps_table, avgsteps_table$Average == max(avgsteps_table$Average))[1,1]
print(maxsteps_interval)
## [1] "835"
Number of missing values
num_na<-sum(is.na(rawdata$steps))
print(num_na)
## [1] 2304
Imputing missing values
Use mean for that interval if not available use mean for that day, and if day mean not available use zero

rawdata_fixed<-rawdata
for(i in 1:17568){
  if(is.na(rawdata_fixed[i,1])){
    value<-subset(avgsteps_table$Average, avgsteps_table$Interval == rawdata_fixed[i,3])

    if(!is.null(value)){
      rawdata_fixed[i,1]<-value
    }
    else{
      datetemp<-rawdata_fixed[i,2]
      subset_temp<-subset(count_table$Steps, count_table$Date == datetemp)
      if(!is.null(subset_temp) & length(subset_temp) != 0){
        rawdata_fixed[i,1]<-subset_temp
      }
      else{
        rawdata_fixed[i,1]<-0
      }
    } 
  }
}
Number of steps taken per day with imputed data added
steps_sum2<-tapply(rawdata_fixed$steps, rawdata_fixed$date, sum)
count_table2<-data.frame(Date = names(steps_sum2), Steps = steps_sum2)
count_table2$Date<-as.Date(count_table2$Date)
hist(count_table2$Steps, xlab = "Total Steps Taken per Day", main = "Histogram of Steps Taken per Day (with Imputed Data)")


Mean steps per day
mean_steps2<-mean(count_table2$Steps)
print(mean_steps2)
## [1] 10766.19
Median steps per day
median_steps2<-median(count_table2$Steps)
print(median_steps2)
## [1] 10766.19
Are there differences in activity patterns between weekdays and weekends?
rawdata_fixed$Weekday<-weekdays(rawdata_fixed$date, abbreviate = TRUE)
rawdata_fixed$Is_weekend<-(rawdata_fixed$Weekday == "Sat" | rawdata_fixed$Weekday == "Sun")

sub_wkday<-subset(rawdata_fixed, rawdata_fixed$Is_weekend == FALSE)
sub_wkend<-subset(rawdata_fixed, rawdata_fixed$Is_weekend == TRUE)

avg_steps_wkday<-tapply(sub_wkday$steps, sub_wkday$interval, mean)
avgsteps_table_wkday<-data.frame(Interval = names(avg_steps_wkday), Average = avg_steps_wkday, Is_weekend = "Weekdays")

avg_steps_wkend<-tapply(sub_wkend$steps, sub_wkend$interval, mean)
avgsteps_table_wkend<-data.frame(Interval = names(avg_steps_wkend), Average = avg_steps_wkend, Is_weekend = "Weekends")

finalweekdays_table<-rbind(avgsteps_table_wkday, avgsteps_table_wkend)
finalweekdays_table$Interval<-as.numeric(finalweekdays_table$Interval)

qplot(Interval, Average, data = finalweekdays_table, geom = "line", facets = .~Is_weekend, ylab = "Average Steps Taken", main = "Average
