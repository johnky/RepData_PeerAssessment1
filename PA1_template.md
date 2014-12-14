---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
keep_md: true
---




## Loading and preprocessing the data

The activity.csv file will be loaded and several matrixes will be set up


```r
StepsData <- read.csv("activity.csv")

StepsData$date <- as.Date(StepsData$date)


Sum_StepsDaily <- aggregate(steps~date, data=StepsData, sum, na.rm=TRUE)
colnames(Sum_StepsDaily) <- c("date", "SumSteps")


AVG_StepsInterval <- aggregate(steps~interval, data=StepsData, mean, na.rm=TRUE)
colnames(AVG_StepsInterval) <- c("interval", "AvgSteps")
```



## What is mean total number of steps taken per day?


```r
hist(Sum_StepsDaily$SumSteps, xlab ="Number of Steps", main ="Histogram of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(Sum_StepsDaily$SumSteps)
```

```
## [1] 10766.19
```

```r
median(Sum_StepsDaily$SumSteps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?


```r
plot(AvgSteps ~ interval, data = AVG_StepsInterval, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
max(AVG_StepsInterval$AvgSteps)
```

```
## [1] 206.1698
```

```r
AVG_StepsInterval[which.max(AVG_StepsInterval$AvgSteps), ]$interval
```

```
## [1] 835
```




## Imputing missing values

Calculate and report the total number of missing values in the dataset 


```r
sum(is.na(StepsData$steps))
```

```
## [1] 2304
```

```r
StepsData2 <- merge(StepsData, AVG_StepsInterval, by="interval")
StepsData2$newsteps <- ifelse(is.na(StepsData2$steps), round(StepsData2$AvgSteps), StepsData2$steps )

Sum_StepsDaily2 <- aggregate(newsteps~date, data=StepsData2, sum, na.rm=TRUE)
colnames(Sum_StepsDaily2) <- c("date", "SumSteps")

hist(Sum_StepsDaily2$SumSteps, xlab ="Number of Steps", main ="Histogram of steps taken each day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mean(Sum_StepsDaily2$SumSteps)
```

```
## [1] 10765.64
```

```r
median(Sum_StepsDaily2$SumSteps)
```

```
## [1] 10762
```

The changing of the NA data to a value seems to have caused the mean and median to shift down just a little bit.
Since you are addding in more points of data and then averaging I can see where this happens.  When you go from 20 values being average to 30 values average where the 10 new values are small numbers this will cause the overall mean to go down.





## Are there differences in activity patterns between weekdays and weekends?



```r
StepsData2$DayType  <- ifelse(weekdays(StepsData2$date)=="Saturday" | weekdays(StepsData2$date)=="Sunday","weekend","weekday")


toplot <- aggregate(newsteps ~ DayType * interval, data=StepsData2, FUN=mean)



library(lattice)
xyplot( newsteps ~ interval | DayType, toplot, layout = c(1, 2), type = "l",  ylab ="Number of Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

There are Differences between weekdays and weekends.
- Much more activity in the early mornings for weekdays
- More activity throughout the whole day on Weekends since most people sit around at a job during the weekday
- Weekends still have activity at night where the weekdays just tapers off




