# Reproducible Research: Peer Assessment 1

The data for this assignment consists of activity measurements:

* _steps_: Number of steps taking in a 5-minute interval (missing values are coded as NA).
* _date_: The date on which the measurement was taken in YYYY-MM-DD format.
* _interval_: Identifier for the 5-minute interval in which measurement was taken.

It is contained in the activity.zip file in this repo.

This knitr document has been run with the following global commands:

* No scientific notation for long numbers.
* No digits after the point for floating point numbers.


```r
options(scipen = 1, digits = 0)
```


## Loading and preprocessing the data

First, all library includes are listed. This way, it is immediately signalled
if all necessary packages are installed or not.


```r
library(plyr)
library(lattice)
```


Then, the activity data is unzipped and read it into a data frame.


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.POSIXct(activity$date)
```


## What is mean total number of steps taken per day?

The step amounts in the activity data are aggregated by day. 


```r
activity_by_day <- ddply(activity, .(date), summarize, steps = sum(steps, na.rm = TRUE))
```


The figure below shows the histogram of steps taken by day.


```r
hist(activity_by_day$steps, xlab = "Steps", ylab = "Days", main = "Steps distribution (by day)")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r

mean <- mean(activity_by_day$steps)
median <- median(activity_by_day$steps)
```


On average, 9354 steps were taken by day, with a median of 10395 
steps.

## What is the average daily activity pattern?

The step amounts are aggregated by (5-minute) interval and averaged over all
days.


```r
activity_by_interval <- ddply(activity, .(interval), summarize, avg_steps = mean(steps, 
    na.rm = TRUE))
```


The figure below shows a time series of average daily step pattern in 5-minute
intervals.


```r
plot(activity_by_interval$avg_steps, type = "l", xaxt = "n", xlab = "Time of day", 
    ylab = "Average steps", main = "Daily activity pattern")
axis(1, at = 1:length(activity_by_interval$interval), labels = activity_by_interval$interval)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r

max_steps_in_day <- activity_by_interval[which.max(activity_by_interval$avg_steps), 
    ]
```


The most steps in a single 5-minute interval on average were at 835.

## Imputing missing values


```r
valid <- complete.cases(activity$steps)
nr_of_NAs <- sum(!valid)
```


The activity data set contains 2304 rows with NA values.

Missing values are imputed by taking the daily average steps for that interval
over the entire dataset.


```r
imputed_activity <- activity
for (i in 1:length(imputed_activity$steps)) {
    if (is.na(imputed_activity$steps[i])) {
        interval <- imputed_activity$interval[i]
        index <- activity_by_interval$interval == interval
        imputed_activity$steps[i] <- activity_by_interval$avg_steps[index]
    }
}
```


The step amounts of the new imputed activity data are again aggregated by day. 


```r
imputed_activity_by_day <- ddply(imputed_activity, .(date), summarize, steps = sum(steps, 
    na.rm = TRUE))
```


The histogram below shows steps taken by day of the imputed activity data.


```r
hist(imputed_activity_by_day$steps, xlab = "Steps", ylab = "Days", main = "Steps distribution (by day)")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r

imputed_mean <- mean(imputed_activity_by_day$steps)
imputed_median <- median(imputed_activity_by_day$steps)
```


The average by day of the imputed data is 10766 steps, with a median
of 10766 steps.  

These values differ from the base activity data. The choice of imputing by mean
steps over all days by 5-minute interval combined with the fact that, when
missing values (NAs) are present, they are present for a full day has resulted
in the same mean and median for the imputed activity dataset. They are also
higher than in the base activity data set.

## Are there differences in activity patterns between weekdays and weekends?

A new factor variable is introduced to distinguish between weekdays and
weekends.


```r
imputed_activity$time_of_week <- as.factor(ifelse(weekdays(imputed_activity$date) %in% 
    c("Saturday", "Sunday"), "weekend", "weekday"))
```


From this, a panel plot containing a time series chart for both weekdays and
weekends is created, showing the average over all days by 5-minute interval.


```r
temp_a <- ddply(imputed_activity[imputed_activity$time_of_week == "weekday", 
    ], .(interval), summarize, avg_steps = mean(steps, na.rm = TRUE), time_of_week = as.factor("weekday"))
temp_b <- ddply(imputed_activity[imputed_activity$time_of_week == "weekend", 
    ], .(interval), summarize, avg_steps = mean(steps, na.rm = TRUE), time_of_week = as.factor("weekend"))

imputed_activity_by_interval <- rbind(temp_a, temp_b)

xyplot(avg_steps ~ interval | time_of_week, data = imputed_activity_by_interval, 
    type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

