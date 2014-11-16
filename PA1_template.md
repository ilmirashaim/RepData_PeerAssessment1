---
title: "Reproducible Research: Peer Assessment 1"
author: "Ilmira Shaim"
output: 
  html_document:
    keep_md: true
---

## Version info
R version 3.1.2 (2014-10-31)  
Platform: x86_64-apple-darwin10.8.0 (64-bit)  
attached packages:   
  lattice_0.20-29  
  data.table_1.9.2  

## Loading and preprocessing the data
Reading data from file to data table. We should convert "date" column from character to Date, because "fread" function can't still do it.

```r
library("lattice")
library("data.table")
unzip("activity.zip", overwrite=T)
raw <- fread("activity.csv")

raw <- raw[, date:=as.Date(date)]
dataWithoutNA = raw[complete.cases(raw)]
```

## What is mean total number of steps taken per day?
A histogram of the total number of steps taken each day

```r
sumByDateBeforeFilling <- dataWithoutNA[, sum(steps), by=date]
histogram(sumByDateBeforeFilling$V1,
     main="Histogram of the total number of steps taken each day",
     xlab="Number of steps for each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 
  
The **mean** and **median** of total number of steps taken per day

```r
paste("mean = ", mean(sumByDateBeforeFilling$V1))
```

```
## [1] "mean =  10766.1886792453"
```

```r
paste("median=", median(sumByDateBeforeFilling$V1))
```

```
## [1] "median= 10765"
```

## What is the average daily activity pattern?
Time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meanByInterval = dataWithoutNA[, mean(steps), by=interval]
xyplot(V1~interval,
    data=meanByInterval,
    type='l',
    main="Time series plot of the 5-minute interval\n and the average number of steps taken,\n averaged across all days",
    xlab="Interval",
    ylab="Average number of steps across all days")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
  
Finding 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps:

```r
paste("interval with max steps =", meanByInterval$interval[which.max(meanByInterval$V1)])
```

```
## [1] "interval with max steps = 835"
```


## Imputing missing values
Calculating the total number of missing values in the dataset

```r
paste("number of missing values = ", sum(!complete.cases(raw)))
```

```
## [1] "number of missing values =  2304"
```

###Strategy of imputing
To fill in all of the missing values in the dataset we will do the following: 
 for the row with a missing value we use mean of steps for its interval.
   
Creating a new dataset that is equal to the original dataset but with the missing data filled in.

```r
raw <- raw[, meanSteps := as.integer(mean(steps, na.rm=T)), by=interval]
fill <- function(x, y) {
  if(is.na(x)) {
    y
  } else {
    x
  }
}
dataWithFilledNAs <- data.table(steps=mapply(fill, raw$steps, raw$meanSteps),
                                date=raw$date,
                                interval=raw$interval)
```
  
Making a histogram of the total number of steps taken each day and calculating the **mean** and **median** total number of steps taken per day. These values differ from the estimates from the first part of the assignment. 

```r
sumByDateAfterFilling <- dataWithFilledNAs[, sum(steps), by=date]
histogram(sumByDateAfterFilling$V1,
     main="Histogram of the total number of steps taken each day\n after filling NA values",
     xlab="Number of steps for each day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
paste("mean = ", mean(sumByDateAfterFilling$V1))
```

```
## [1] "mean =  10749.7704918033"
```

```r
paste("median=", median(sumByDateAfterFilling$V1))
```

```
## [1] "median= 10641"
```
  
## Are there differences in activity patterns between weekdays and weekends?

```r
l <- Sys.setlocale("LC_ALL", 'en_US.UTF-8')

weekdayToFactor <- function(x){
  if(x %in% c("Sunday", "Saturday")){
    as.factor("weekend")
  } else {
    as.factor("workday")
  }
}
dataWithFilledNAs <- dataWithFilledNAs[, dayType:=sapply(weekdays(date), weekdayToFactor)]

meanByIntervalAfterFilling = dataWithFilledNAs[, mean(steps), by=list(interval,dayType)]

xyplot(V1~interval|dayType,
     data=meanByIntervalAfterFilling,
     main="Time series plot of the 5-minute interval\n and the average number of steps taken,\n averaged across all days\nafter filling in NA values",
     xlab="Interval",
     ylab="Number of steps",
     layout=c(1,2),
     type="l")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

We can see, that in the morning of workdays the number of steps is very high. But on average the number of steps during workday is less than at weekend.
