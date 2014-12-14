---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

StepsData <- read.csv("activity.csv")

StepsData$date <- as.Date(StepsData$date)

Sum_StepsDaily <- aggregate(steps~date, data=StepsData, sum, na.rm=TRUE)

colnames(Sum_StepsDaily) <- c("date", "SumSteps")

AVG_StepsInterval <- aggregate(steps~interval, data=StepsData, mean, na.rm=TRUE)
colnames(AVG_StepsInterval) <- c("interval", "AvgSteps")


## What is mean total number of steps taken per day?


hist(Sum_StepsDaily$SumSteps, xlab ="Number of Steps", main ="Histogram of steps taken each day")

Mean Number of Steps taken each day
mean(Sum_StepsDaily$SumSteps)

Median Number of Steps taken each day
median(Sum_StepsDaily$SumSteps)

## What is the average daily activity pattern?

plot(AvgSteps ~ interval, data = AVG_StepsInterval, type = "l")

The Maxium Average Steps during an Interval is
max(AVG_StepsInterval$AvgSteps)

This occured during the interval of
AVG_StepsInterval[which.max(AVG_StepsInterval$AvgSteps), ]$interval


## Imputing missing values

Calculate and report the total number of missing values in the dataset 
sum(is.na(StepsData$steps))


StepsData2 <- merge(StepsData, AVG_StepsInterval, by="interval")
StepsData2$newsteps <- ifelse(is.na(StepsData2$steps), round(StepsData2$AvgSteps), StepsData2$steps )

Sum_StepsDaily2 <- aggregate(newsteps~date, data=StepsData2, sum, na.rm=TRUE)
colnames(Sum_StepsDaily2) <- c("date", "SumSteps")

hist(Sum_StepsDaily2$SumSteps, xlab ="Number of Steps", main ="Histogram of steps taken each day")


Mean Number of Steps taken each day
mean(Sum_StepsDaily2$SumSteps)

Median Number of Steps taken each day
median(Sum_StepsDaily2$SumSteps)


##4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values ##differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?




## Are there differences in activity patterns between weekdays and weekends?


StepsData2$DayType  <- ifelse(weekdays(StepsData2$date)=="Saturday" | weekdays(StepsData2$date)=="Sunday","weekend","weekday")


toplot <- aggregate(newsteps ~ DayType * interval, data=StepsData2, FUN=mean)



library(lattice)
xyplot( newsteps ~ interval | DayType, toplot, layout = c(1, 2), type = "l")
    
    