Peer-graded Assignment: Course Project 1
-------------------------------------------------------------------------------------
This is a report that answers the questions detailed below. It represent an assignment for the course "Reproducible Research" by Johns Hopkins University.


# Loading and processing the data

```r
data <-read.csv("activity.csv", header=TRUE, sep=",")
data<-data.frame(data)
library(ggplot2)
```

We have loaded the dataset with read.csv

# What is mean total number of steps taken per day?

```r
TotalSteps <- aggregate(data=data, steps ~ date, sum)
ggplot(TotalSteps, aes(steps, color=date))+ geom_histogram() + labs(title= "Total number of steps taken per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
mean(TotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(TotalSteps$steps)
```

```
## [1] 10765
```

The plot shows the total steps taken per day. For each day we have a given number of steps.
We have also computed the mean and the median of total steps.

# What is the average daily activity pattern?


```r
TimeSteps <- aggregate(data=data, steps ~ interval, mean)
ggplot(TimeSteps, aes(interval, steps))+ geom_line() + labs(title= "Plot of the average number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
TimeSteps[TimeSteps$steps==max(TimeSteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

The plot shows the average number of steps per interval. The interval that have the max number of steps is the interval 835.

# Imputing missing values


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.3
```

```
## 
## Attaching package: 'dplyr'
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
sum(is.na(data))
```

```
## [1] 2304
```

```r
my_na <-is.na(data$steps)
mean_steps<-tapply(data$steps, data$interval, mean, na.rm=TRUE, simplify = TRUE)
data$steps[my_na] <-mean_steps[data$interval[my_na]]
```

```
## Warning in data$steps[my_na] <- mean_steps[data$interval[my_na]]: il numero di elementi da sostituire non è un multiplo della
## lunghezza di sostituzione
```

```r
TotalSteps <- aggregate(data=data, steps ~ date, sum)
ggplot(TotalSteps, aes(steps, color=date))+ geom_histogram() + labs(title= "Total number of steps taken per day after imputation of NA")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

We have 2016 NAs and we have chosen to substitute these NAs of the steps with the mean of the steps for each interval.
Then the plot shows the  histogram of the total number of steps taken each day after the imputation of NA.
Then we have computed the mean and the median.

# Are there differences in activity patterns between weekdays and weekends?


```r
data <-read.csv("activity.csv", header=TRUE, sep=",")
data<-data.frame(data)

Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
days <-weekdays(as.Date(data$date)) 
days[grep("Saturday|Sunday", days)] <- "Weekend" 
days[grep("Monday|Tuesday|Wednesday|Thursday|Friday", days)] <- "Weekday" 
data$weekdays <- days 

stepsWeekdays<-aggregate(data=data, steps ~ interval + weekdays, mean)
ggplot(stepsWeekdays, aes(x=interval, y=steps, color = weekdays)) +  geom_line() + labs(title= "Average number of steps taken per 5-minute interval across weekdays and weekends")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

At first we have restored the original dataset and loaded it. Then we have added a new variable to indicate if each day is a weekday or a weekend, in order to answer the question.
In the picture we can observe the difference between the average number of steps per interval across the weekdays and the weekends. The average number of steps for the weekdays seems to reach higher values at the fist part of the plot and the lower values at the second part of the plot.







