---
title: "Reproducible Research Assignment 1"
author: "Jack Kramer"
date: "May 27, 2016"
output: 
  html_document: 
    self_contained: no
---
##Loading and preprocessing the data
1. Load the data
```{r}
setwd("\\Users\\kcaj2\\Desktop\\Coursera\\Reproducable Research\\RepData_PeerAssessment1-master")
data <- read.csv("activity.csv", na.strings = "NA")
```

2. Process/transform the data into a format suitable for your analysis
```{r}
data$date <- as.Date(data$date, format = "%Y-%m-%d")
NAdata <- data[complete.cases(data),]

```

##What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```{r}
stepdata <- aggregate(steps~date, data = NAdata, FUN = sum)
head(stepdata)
```

2. Make a histogram of the total number of steps taken each day
```{r, fig.keep='high', warning=FALSE}

library("ggplot2")
ggplot(data = stepdata, aes(x = steps)) + 
    ggtitle("Frequency of Steps") +
    geom_histogram(binwidth = 705.1)

```

3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean(stepdata$steps)
median(stepdata$steps)
```

##What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r, fig.keep='high'}
avgdata <- aggregate(steps~interval, data = NAdata, FUN = mean)
head(avgdata)

ggplot(avgdata, aes(x=interval, y = steps)) + 
    geom_line() + 
    ggtitle("Average Steps at each Interval")

```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
avgdata[which.max(avgdata$steps),]

```

##Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
sum(is.na(data$steps))

```

2. Devise a strategy for filling in all of the missing values in the dataset. 

```{r}
##we will use average steps for each 5-minute interval since it is already generated.

head(avgdata)

```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}

filldata <- data
for(i in 1:nrow(filldata)) {
    if(is.na(filldata[i,1])){
        filldata[i,1] <- avgdata[match(filldata[i,3], avgdata[,1]),2]
    }
}
head(data)
head(avgdata)
head(filldata)

```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r, fig.keep='high'}
aggfilldata <- aggregate(steps~date, data = filldata, FUN = sum)

ggplot(data = aggfilldata, aes(x = steps)) + 
    ggtitle("Frequency of Steps with NAs Replaced") +
    geom_histogram(binwidth = 705.1)

mean(aggfilldata$steps)
median(aggfilldata$steps)
```


##Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
weekdaydata <- filldata
weekendvect <- c("Saturday", "Sunday")
weekdaydata$daytype <- factor((weekdays(weekdaydata$date) %in% weekendvect), levels = c(FALSE, TRUE), labels = c("Weekday", "Weekend"))

head(weekdaydata)

```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r, fig.keep='high'}
aggweekdaydata <- aggregate(steps~daytype+interval, data = weekdaydata, FUN = mean)

ggplot(aggweekdaydata, aes(x=interval, y = steps, color = daytype)) + 
    geom_line() + 
    facet_wrap(~daytype, ncol = 1, nrow = 2)+
    ggtitle("Average Steps at each Interval")


```


