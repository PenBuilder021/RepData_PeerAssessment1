---
title: "PA1_template"
author: "Jaana Y."
date: "August 1, 2016"
output: html_document
---
# Reproducible Research: Peer Assessment 1 - Personal Activity Monitoring

## Introduction
This assignment makes use of data from a personal activity monitoring device.  This devices collects data at 5-minute intervals through out the day.  The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5-minute intervals each day.

The data for this assignment can be downloaded from the course website:
https://d396qusza40orc.cloudfont.net/repdata%2Fdata%2Factivity.zip

The variables included in this dataset are:
  * steps: Number of steps taken in a 5-minute interval (missing values are coded as NA)
  * date: The date on which the measurement was taken in YYYY-MM-DD format
  * interval: Identifier for the 5-minute interval in which measurement was taken
  
The dataset is stored in a comma-separated-value(CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment
### Loading and preprocessing the data
```{r loadData}
## Set working directory & load the data

setwd("~/R/Reproducible_Research/Week2/")
data <- read.csv("activity.csv")

library(ggplot2)

```


### What is the mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

```{r}
## Remove the missing values

dataNoNA <- data[!is.na(data$steps),]

```

1 - Calculate the total number of steps taken per day
```{r}

stepsByDay <- aggregate(steps ~ date, dataNoNA, sum)


```
2 - Make a histogram of the toal number of steps taken per day (Understand the difference between a histogram and a barplot - research)

```{r}
## Create a histogram of stepsByDay

Total_stepsByDay <- ggplot(stepsByDay, aes(x = stepsByDay$steps)) +
   geom_histogram(fill="blue", binwidth = 800) +
   labs(title = "Total Steps Per Day", x = "Total Steps", y = "Frequency")

Total_stepsByDay

```

3 - Calculate and report the mean and median of the total number of steps taken per day

```{r}
## Mean
meanSteps <- mean(stepsByDay$steps)
meanSteps

## Median
medianSteps <- median(stepsByDay$steps)
medianSteps
```


### What is the average daily activity pattern?
1 - Make a time series plot (i.e., type = "1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
interval <- aggregate(steps ~ interval, data, mean)

plot(interval$interval, interval$steps, type = "l", xlab = "Interval", ylab = "Number of Steps", main = "Average Number of Steps per Day by Interval")
```

2 - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` {r}
maxInterval <- interval[which.max(interval$steps), 1]
maxInterval
```

### Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1 - Calculate and report the total number of missing values in the dataset(i.e., the total number of rows with NAs)
``` {r}
sum(is.na(data$steps))
```
2 - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
``` {r}
data2 <- data
NAdata <- is.na(data$steps)
avg_interval <- tapply(data2$steps, data2$interval, mean, na.rm = TRUE, simplify = TRUE)

```
3 - Create a new dataset that is equal to the original dataset but with the missing data filled in.
``` {r}
data2$steps[NAdata] <- avg_interval[as.character(data2$interval[NAdata])]
data2 <- data2[, c("date", "interval", "steps")]

```
4 - Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 
``` {r}

Total_steps2 <- aggregate(steps ~ date, data2, sum)

hist(Total_steps2$steps, col = "blue", xlab = "Number of Steps", ylab = "Frequency", main = paste("Total Steps Per Day"))

## Mean
meanSteps2 <- mean(Total_steps2$steps)
meanSteps2

## Median
medianSteps2 <- median(Total_steps2$steps)
medianSteps2
```

Question: Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Answer: Imputing the missing data with the average number of steps in the same 5-minute interval is that both the mean and median are the same value = 10766.19

### Are there differences in activity patterns between weekdays and weekends?
For this, part of the weekdays() function may be some help here.  Use the dataset with the filled-in missing values for this part.

1 - Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
``` {r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
data2$dayCat = as.factor(ifelse(is.element(weekdays(as.Date(data2$date)),weekdays), "Weekday", "Weekend"))

interval2 <- aggregate(steps ~ interval + dayCat, data2, mean)
```

2 - Make a panel plot containing a time series plot (i.e., type = "1") of the 5-minute interval (x-asis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
``` {r}
library(lattice)

xyplot(interval2$steps ~ interval2$interval|interval2$dayCat, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```


