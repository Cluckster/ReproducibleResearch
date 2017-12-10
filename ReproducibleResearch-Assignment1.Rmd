Reproducible Research - Assignment 1
========================================================

Marty Cluck

## Introduction (from assignment specifications)
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data. This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

[Activity monitoring dataset](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Load and preprocess data from the csv file
```{r echo="TRUE"}
extractedData <- read.csv("./data/activity.csv")
```

### What is the mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day.
```{r echo="TRUE"}
dailySteps <- aggregate(steps ~ date, extractedData, sum)
```

2. Make a histogram of the toal number of steps per day.
```{r}
hist(dailySteps$steps, main = "Total Daily Steps", xlab = "Number of Steps")
abline(v = median(dailySteps$steps), col = "blue", lwd = 2)
abline(v = mean(dailySteps$steps), col = "red", lty = 4, lwd = 3)
```

3. Calculate and report the mean and median of the total number of steps per day.
```{r }
dailyMeanSteps <- mean(dailySteps$steps)
dailyMedianSteps <- median(dailySteps$steps)
```
The daily mean of steps = `r dailyMeanSteps`.
The daily median of steps = `r dailyMedianSteps`.


### What is the daily average activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
```{r }
intervalMeanSteps <- aggregate(steps ~ interval, extractedData, mean)
plot(intervalMeanSteps$interval, intervalMeanSteps$steps, type="l", main = "Average Steps per Day by Interval",xlab = "Interval", ylab = "Number of Steps")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r }
maxIntervalIndex <- which.max(intervalMeanSteps$steps)
maxInterval <- intervalMeanSteps[maxIntervalIndex,"interval"]
maxIntervalSteps <- intervalMeanSteps[maxIntervalIndex,"steps"]
```
The maximum interval is interval `r maxInterval` with `r maxIntervalSteps` steps.

### Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. 

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
incompleteCases <- sum(!complete.cases(extractedData))
```

There are `r incompleteCases` rows with NAs.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

What will be used here is the mean for the 5-minute interval. First we need to create a new dataset to store the updated data.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Now populate the NA values with the mean values for the respective intervals.
```{r}
modified <- extractedData

for(i in 1:length(modified$steps))
{
    if(is.na(modified$steps[i]))
    {
        modified$steps[i] <- intervalMeanSteps[which(modified$interval[i] ==  intervalMeanSteps$interval),]$steps
    }
}
```

Recount the the total of steps by day.
```{r}
modifiedDailySteps <- aggregate(steps ~ date, modified, sum)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
par(mfrow = c(1,2))

hist(dailySteps$steps, main = "Total Daily Steps (with NAs)", xlab = "Number of Steps")
abline(v = median(dailySteps$steps), col = "blue", lwd = 2)
abline(v = mean(dailySteps$steps), col = "red", lty = 4, lwd = 3)

hist(modifiedDailySteps$steps, main = "Total Daily Steps (No NAs)", xlab = "Number of Steps")
abline(v = median(modifiedDailySteps$steps), col = "blue", lwd = 2)
abline(v = mean(modifiedDailySteps$steps), col = "red", lty = 4, lwd = 3)
modifiedDailyMean <- mean(modifiedDailySteps$steps)
modifiedDailyMedian <- median(modifiedDailySteps$steps)
dev.off()
```

We can observe that the original daily mean and the mean for the complete data  are the same (original mean = `r dailyMeanSteps`, modified mean = `r modifiedDailyMean`). However the original median = `r dailyMedianSteps` differs from the complete median = `r modifiedDailyMedian`. Also the frequency of days of the mean step count increased.

### Are there any differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
modified$dow <- weekdays(as.Date(modified$date))
modified$dayType[(modified$dow =="Saturday" | modified$dow =="Sunday")]<-"weekend"
modified$dayType[!(modified$dow =="Saturday" | modified$dow =="Sunday")]<-"weekday"
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
```{r}
modifiedIntervalAverageWeekend <- aggregate(steps ~ interval, data = modified, subset = dayType == "weekend", FUN = mean)
modifiedIntervalAverageWeekday <- aggregate(steps ~ interval, data = modified, subset = dayType == "weekday", FUN = mean)

par(mfrow = c(1,2), t)

plot(modifiedIntervalAverageWeekend, type = "l", main = "Weekend Interval Step Avg")
plot(modifiedIntervalAverageWeekday, type = "l", main = "Weekday Interval Step Avg")
```
