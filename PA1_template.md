---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
data <- download.file(fileUrl, "activity.zip")
data <- unzip("activity.zip")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
# Calculate the total number of steps taken per day #

daySteps<-  aggregate(steps ~ date, activity, sum, na.rm=TRUE)

# Make a histogram of the total number of steps taken per day#

hist(daySteps$steps, col="orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

```r
#Calculate and report the mean and median of the total number of steps taken per day#

meanDS <- mean(daySteps$steps)
medianDS <- median(daySteps$steps)
```
### mean is 10766.19
### median is 10765 


## What is the average daily activity pattern?


```r
#  plot the 5-minute interval and the average number of steps taken#
library(ggplot2)

intSteps <- aggregate(steps ~ interval, activity, mean, na.rm=TRUE)
p <- ggplot(data = intSteps, mapping = aes(x=interval, y=steps))
p <- p+geom_line(size=1, col="blue")
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

```r
# Interval contains the maximum number of steps #

intMaxSteps <- intSteps[which.max(intSteps$steps),]
```
### Max: interval 835	steps 206.1698	


## Imputing missing values


```r
# Calculate and report the total number of missing values #

missingValues <- length(which(is.na(activity$steps)))

#  filling in all of the missing values #
# Create a new dataset that is equal to the original dataset but with the missing data filled in #

meanIS <- meanDS/length(unique(activity$interval))
activityNew <- activity
activityNew[is.na(activity)] <- meanIS

# histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day #

dayStepsNew<- aggregate(steps ~ date, activityNew, sum)
p2<-hist(dayStepsNew$steps, col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

```r
print(p2)
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1]  5 12 36  6  2
## 
## $density
## [1] 1.639344e-05 3.934426e-05 1.180328e-04 1.967213e-05 6.557377e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "dayStepsNew$steps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

```r
meanDSN <- mean(dayStepsNew$steps)
medianDSN <- median(dayStepsNew$steps)
```
### mean is 10766.19
### median is 10766.19 

## Are there differences in activity patterns between weekdays and weekends?


```r
#A new factor variable with two levels “weekday” and “weekend” 
Sys.setlocale("LC_TIME", "en_US")
```

```
## [1] "en_US"
```

```r
activity$date <- as.Date(activity$date)

isweekday<-function(xdate){
   if (weekdays(xdate) %in% c("Sunday", "Saturday"))
       return("weekend")
   else
       return("weekday")
}
activity$weekdays<-mapply(isweekday,activity$date)

# Plot of the interval and the average number of steps taken across weekday days and weekend days #

intStepsWeekdays <- aggregate(steps ~ interval+weekdays, activity, mean)
p3 <- ggplot(data = intStepsWeekdays, mapping = aes(x=interval, y=steps))
p3 <- p3+geom_line(size=1, col="blue")
p3 <- p3+facet_grid(weekdays~.)
print(p3)
```

![](PA1_template_files/figure-html/unnamed-chunk-27-1.png)<!-- -->




