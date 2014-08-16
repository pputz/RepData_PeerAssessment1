# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activityData <- read.csv(unz("activity.zip", "activity.csv"), 
                         colClasses=c("integer","Date","integer"))

str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?


```r
totalSteps <- aggregate(steps ~ date, activityData, sum)
hist(totalSteps$steps, breaks=11)
```

![plot of chunk dailysteps](figure/dailysteps.png) 

```r
mean1 <- mean(totalSteps$steps)
median1 <- median(totalSteps$steps)
```

The average number of steps per day (mean) is 10766.2 and the median is 10765.



## What is the average daily activity pattern?


```r
StepsPerInt <- aggregate(steps ~ interval, activityData, mean)

with(StepsPerInt, {
    plot(x = interval, y=steps, type = "l")
    })
```

![plot of chunk activitypattern](figure/activitypattern.png) 

```r
maxSteps <- StepsPerInt$interval[which.max(StepsPerInt$steps)]
```

On average the most steps are taken during the 5-minute interval 835.

## Imputing missing values


```r
apply(activityData, 2, function(x) {sum(is.na(x))})
```

```
##    steps     date interval 
##     2304        0        0
```

There are in total 2304 missing values. All of them are in the column "steps". 



```r
library(plyr)
# define function that replaces NAs with mean
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
# NAs are replaced with the mean number of steps within the same interval
activityData2 <- ddply(activityData, ~ interval, transform, steps = impute.mean(steps))

# calculate the total steps per day for the new data set.
totalSteps2 <- aggregate(steps ~ date, activityData2, sum)
hist(totalSteps2$steps, breaks=11)
```

![plot of chunk imputing](figure/imputing.png) 

```r
mean2 <- mean(totalSteps2$steps)
median2 <- median(totalSteps2$steps)
```

I am replacing missing values in the "steps" variable with the mean number of steps for each time interval. This method has the effect that the number of days with total steps in the mid-range (10000 to 12500 steps) increases. The new mean of steps per day is 10766.2. This is a difference of 0.0 compared to the mean of the original data set. The new median is 10766.2. This is a difference of 1.189 compared to the median of the original data set.


## Are there differences in activity patterns between weekdays and weekends?


```r
# add new column with values "weekend" and "weekday"
activityData2$weekday <- ifelse(
  weekdays(activityData2$date) %in% c("Sunday", "Saturday"), 
  "weekend", "weekday")  

# calculate the mean number of steps for each interval grouped by weekday/weekend.
stepsByWeekday <- aggregate(steps ~ interval + weekday, activityData2, mean)

library(lattice)
xyplot(steps ~ interval | weekday, data = stepsByWeekday, type="l", layout = c(1,2), )
```

![plot of chunk weekends](figure/weekends.png) 

```r
median3 <- tapply(stepsByWeekday$steps, stepsByWeekday$weekday, median)
```

The activity patterns for weekdays vs. weekends look different. During weekends there are more intervals with a high number of steps. Accordingly, the median number of steps per interval on weekdays is 25.8 vs. on weekends 32.34.
