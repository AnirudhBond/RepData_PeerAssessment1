# Markdown project


```r
knitr::opts_chunk$set(echo = TRUE)
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.6.3
```

```
## 
## Attaching package: 'plyr'
```

```
## The following object is masked from 'package:DMwR':
## 
##     join
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(data.table)
```

```
## Warning: package 'data.table' was built under R version 3.6.3
```

```
## data.table 1.12.8 using 2 threads (see ?getDTthreads).  Latest news: r-datatable.com
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
library(graphics)
library(grDevices)
library(DMwR)
```

## 1. Code for reading in the dataset and/or processing the data

```r
activity <- read.csv("activity.csv",header=TRUE)
activity_NA <- data.table(activity)
activity <- knnImputation(activity)
activity$date<-as.Date(activity$date)
```

## 2. Histogram of the total number of steps taken each day

```r
total_steps <- aggregate(activity_NA$steps,list(activity_NA$date),sum)
colnames(total_steps)<- c("Date","Total_Steps")
hist(total_steps$Total_Steps,breaks = 10,main = "Histogram of the total number of steps taken each day",xlab = "Mean steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

## 3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- aggregate(activity$steps,list(activity$date),FUN = mean)
median_steps <- aggregate(activity$steps,list(activity$date),FUN = median)
summary <- cbind(mean_steps,median_steps)
colnames(summary) = c("Date","Mean","Date","Median")
summary[,-3]
```

```
##          Date       Mean    Median
## 1  2012-10-01 39.4861111 17.500000
## 2  2012-10-02  0.4375000  0.000000
## 3  2012-10-03 39.4166667  0.000000
## 4  2012-10-04 42.0694444  0.000000
## 5  2012-10-05 46.1597222  0.000000
## 6  2012-10-06 53.5416667  0.000000
## 7  2012-10-07 38.2465278  0.000000
## 8  2012-10-08 44.3093750 22.200000
## 9  2012-10-09 44.4826389  0.000000
## 10 2012-10-10 34.3750000  0.000000
## 11 2012-10-11 35.7777778  0.000000
## 12 2012-10-12 60.3541667  0.000000
## 13 2012-10-13 43.1458333  0.000000
## 14 2012-10-14 52.4236111  0.000000
## 15 2012-10-15 35.2048611  0.000000
## 16 2012-10-16 52.3750000  0.000000
## 17 2012-10-17 46.7083333  0.000000
## 18 2012-10-18 34.9166667  0.000000
## 19 2012-10-19 41.0729167  0.000000
## 20 2012-10-20 36.0937500  0.000000
## 21 2012-10-21 30.6284722  0.000000
## 22 2012-10-22 46.7361111  0.000000
## 23 2012-10-23 30.9652778  0.000000
## 24 2012-10-24 29.0104167  0.000000
## 25 2012-10-25  8.6527778  0.000000
## 26 2012-10-26 23.5347222  0.000000
## 27 2012-10-27 35.1354167  0.000000
## 28 2012-10-28 39.7847222  0.000000
## 29 2012-10-29 17.4236111  0.000000
## 30 2012-10-30 34.0937500  0.000000
## 31 2012-10-31 53.5208333  0.000000
## 32 2012-11-01 35.4753472 11.900000
## 33 2012-11-02 36.8055556  0.000000
## 34 2012-11-03 36.7048611  0.000000
## 35 2012-11-04 28.3461806  8.650000
## 36 2012-11-05 36.2465278  0.000000
## 37 2012-11-06 28.9375000  0.000000
## 38 2012-11-07 44.7326389  0.000000
## 39 2012-11-08 11.1770833  0.000000
## 40 2012-11-09 28.5319444 12.100000
## 41 2012-11-10 31.8541667 11.500000
## 42 2012-11-11 43.7777778  0.000000
## 43 2012-11-12 37.3784722  0.000000
## 44 2012-11-13 25.4722222  0.000000
## 45 2012-11-14 38.1902778 14.850000
## 46 2012-11-15  0.1423611  0.000000
## 47 2012-11-16 18.8923611  0.000000
## 48 2012-11-17 49.7881944  0.000000
## 49 2012-11-18 52.4652778  0.000000
## 50 2012-11-19 30.6979167  0.000000
## 51 2012-11-20 15.5277778  0.000000
## 52 2012-11-21 44.3993056  0.000000
## 53 2012-11-22 70.9270833  0.000000
## 54 2012-11-23 73.5902778  0.000000
## 55 2012-11-24 50.2708333  0.000000
## 56 2012-11-25 41.0902778  0.000000
## 57 2012-11-26 38.7569444  0.000000
## 58 2012-11-27 47.3819444  0.000000
## 59 2012-11-28 35.3576389  0.000000
## 60 2012-11-29 24.4687500  0.000000
## 61 2012-11-30 25.6836871  4.400469
```

## 4. Time series plot of the average number of steps taken

```r
interval <- aggregate(activity$steps,list(activity$interval),FUN = mean)
colnames(interval)=c("interval","average_steps")
with(interval,plot(interval,average_steps,type = "l",xlab="intervals",ylab = "Average Steps", main= "Time series plot of the average number of steps taken"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

## 5. Interval with max average steps 

```r
max = max(interval$average_steps)
filter(interval,average_steps==max)
```

```
##   interval average_steps
## 1      835      203.6589
```

## 6. Code to describe and show a strategy for imputing missing data

```r
##Total missing values
sum(!is.na(activity_NA))
```

```
## [1] 50400
```

```r
activity <- knnImputation(activity_NA)
```

## 7. Histogram of the total number of steps taken each day after missing values are imputed

```r
total_steps <- aggregate(activity$steps,list(activity$date),sum)
colnames(total_steps)<- c("Date","Total_Steps")
hist(total_steps$Total_Steps,breaks =10,main = "Histogram of the total number of steps taken each day",xlab = "Mean steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

## 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
activity$date<-as.Date(activity$date)
activity$weekday <- weekdays(activity$date)
interval_day <- aggregate(activity$steps,list(activity$interval,activity$weekday),mean)
colnames(interval_day) <- c("interval","weekday","average_steps") 
interval_weekday <- filter(interval_day,weekday == "Monday"|weekday == "Tuesday"|weekday == "Wednesday"|weekday == "Thrusday"|weekday=="Friday")
interval_weekend <- filter(interval_day,weekday == "Saturday"|weekday == "Sunday")
interval_weekday <-aggregate(interval_weekday$average_steps,list(interval_weekday$interval),mean)
interval_weekend <-aggregate(interval_weekend$average_steps,list(interval_weekend$interval),mean)
par(mfrow= c(1,2))
with(interval_weekday,plot(Group.1,x,type = "l",xlab = "interval",ylab = "average steps on weekdays"))
with(interval_weekend,plot(Group.1,x,type = "l",xlab = "interval",ylab = "average steps on weekends"))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
