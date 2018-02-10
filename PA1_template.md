---
title: "Reproducible Research - Peer-graded Assignment: Course Project 1"
output:
html_document:
keep_md: true
self_contained: true
author: "Jeroen Broekhuisen"
date: "8 februari 2018"
---



# Loading and preprocessing the data
The code assumes the activity.csv file is located in your working directory
The code in this file requires the dplyr package to be loaded. They are assumed to be installed



### Load the data


```r
ActData <- read.csv("activity.csv", header = TRUE)
```


### Process/transform the data


```r
ActData$date <- as.Date(ActData$date)
```


# What is mean total number of steps taken per day?
NA can be ignored so first remove the NAs


```r
bad <- is.na(ActData$steps)
ActDataClean <- ActData[!bad, ]
```


### Calculate the total number of steps taken per day


```r
StepsPerDay <- ActDataClean %>% group_by(date) %>% summarise(steps_per_day = sum(steps))
```


### Make a histogram of the total number of steps taken each day


```r
hist(StepsPerDay$steps_per_day, xlab = "Steps per day", ylab = "Days", main = "Total number of steps taken each day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-6-1.png" width="672" />

### Calculate and report the mean and median of the total number of steps taken per day


```r
MeanSteps <- as.integer(mean(StepsPerDay$steps_per_day))
MedianSteps <- median(StepsPerDay$steps_per_day)
```

The rounded mean number of steps per day is 10766. The median number of steps is 10765.


# What is the average daily activity pattern?


```r
#ungroup(ActDataClean)
StepsPerInterval <- ActDataClean %>% group_by(interval) %>% summarise(steps_per_interval = mean(steps))
```

### Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
plot(StepsPerInterval$steps_per_interval, xlab = "Intervals", ylab = "Average number of steps taken", type = "l")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-9-1.png" width="672" />

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
StepsPerIntervalOrdered <- arrange(StepsPerInterval, desc(steps_per_interval))
MaxSteps <- as.integer(StepsPerIntervalOrdered$steps_per_interval[1])
MaxStepsInterval <- StepsPerIntervalOrdered$interval[1]
```

The maximum average steps per interval is 206. This is reached at the interval 835.

# Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as ). The presence of missing days may introduce bias into some calculations or summaries of the data.

### Calculate and report the total number of missing values in the dataset


```r
NrBad <- sum(bad)
```

The total number of rows with NAs is 2304.

### Devise a strategy for filling in all of the missing values in the dataset.
First lets have a look at the missing values

```r
MissingValues <- ActData[bad, ]
hist(MissingValues$interval)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```r
hist(ActDataClean$interval) 
```

<img src="PA1_template_files/figure-html/unnamed-chunk-12-2.png" width="672" />

So the missing values are nicely spread across the day. Just like the complete data.

What if we look at which dates there is missing data


```r
unique(MissingValues$date)
```

```
## [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09"
## [6] "2012-11-10" "2012-11-14" "2012-11-30"
```
So there are "only" eight days with missing values.    

How are the missing data spread accros these days?


```r
StepsPerDayNA <- MissingValues %>% group_by(date) %>% summarise(steps_per_day = n())
StepsPerDayNA
```

```
## # A tibble: 8 x 2
##         date steps_per_day
##       <date>         <int>
## 1 2012-10-01           288
## 2 2012-10-08           288
## 3 2012-11-01           288
## 4 2012-11-04           288
## 5 2012-11-09           288
## 6 2012-11-10           288
## 7 2012-11-14           288
## 8 2012-11-30           288
```

So during the days with missing values there are everytime 288 missing values. While the number of measurements per day is 12 times 24 is also 288. So, for the days with missing data these days are completely without measurements. One option is to just ignore these days. Another option is to add the mean for the time interval to these NAs. For the sake of this project we will use the last option.

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
ActDataComplete <- ActData

for (iCounter in (1:nrow(ActDataComplete))) {
    if (is.na(ActDataComplete$steps[iCounter])) {
        iInterval <- ActDataComplete$interval[iCounter]  
        hit <- filter(StepsPerInterval, interval == iInterval) 
        ActDataComplete$steps[iCounter] <- hit$steps_per_interval
    }
}
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

Calculate the total number of steps taken per day


```r
StepsPerDay <- ActDataComplete %>% group_by(date) %>% summarise(steps_per_day = sum(steps))
```


Make a histogram of the total number of steps taken each day


```r
hist(StepsPerDay$steps_per_day, xlab = "Steps per day", ylab = "Days", main = "Total number of steps taken each day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-17-1.png" width="672" />

So we can see that we now have much higher block in the middle which is logical as we added 8 days with the mean values. The other blocks are still the same.

Calculate and report the mean and median of the total number of steps taken per day


```r
MeanSteps <- as.integer(mean(StepsPerDay$steps_per_day))
MedianSteps <- as.integer(median(StepsPerDay$steps_per_day))
```

The rounded mean number of steps per day is now 10766. The median number of steps is 10766. The mean is, as expected, still the same as we just added days with the mean value. The median however has changed slightly as due to our 8 new days with the mean 10766 steps, that is now the value that was measured the most. In conclusion the impact has been minimal which confirms the choosen value to replace the NAs was the right one.


# Are there differences in activity patterns between weekdays and weekends?
Please note that for this part I used the original Dataset again with the NA values removed.
Also I first have to set my local time to English otherwise I get the Dutch names for the days of the week.


```r
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
bWeekend <- (weekdays(ActDataClean$date) == "Saturday" | weekdays(ActDataClean$date) == "Sunday")
ActDataDay <- mutate(ActDataClean, day = factor(case_when(bWeekend ~ "weekend", !bWeekend ~ "weekday")))
```

### Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
ActDataWeekend <- filter(ActDataDay, day == "weekend")
ActDataWeekday <- filter(ActDataDay, day == "weekday")

StepsPerIntervalWeekend <- ActDataWeekend %>% group_by(interval) %>% summarise(steps_per_interval = mean(steps))
StepsPerIntervalWeekday <- ActDataWeekday %>% group_by(interval) %>% summarise(steps_per_interval = mean(steps))

par(mfrow = c(2, 1))
par(oma = c(1,1,1,1))
par(mar = c(4,4,2,2))
plot(StepsPerIntervalWeekend$interval, StepsPerIntervalWeekend$steps_per_interval, xlab = "Intervals", ylab = "Mean number of steps", type = "l", main = "Weekend", col = "blue")

par(mar = c(4,4,2,2))
plot(StepsPerIntervalWeekday$interval, StepsPerIntervalWeekday$steps_per_interval, xlab = "Intervals", ylab = "Mean number of steps", type = "l", main = "Weekday", col = "blue")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-21-1.png" width="672" />




