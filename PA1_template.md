---
title: "Activity Pattern Analysis"
author: "Mokolade Fadeyibi"
date: "7/19/2020"
output:
  html_document:
    keep_md: yes
---



## Loading and Preprocessing the Data

The data was downloaded, unzipped, and placed in the working directory. It was loaded in to R Studio as below:


```r
mydata <- read.csv("activity.csv")
```


## Calculating the Mean Total Number of Steps Taken per Day
1. This was done by using the dplyr package. The total number of steps was computed after the NAs were first removed.


```r
library(dplyr)
date_summary <- mydata %>%
                filter(!is.na(mydata$steps)) %>%
                group_by(date) %>%
                summarise(steps = sum(steps))
```

2. A histogram of the result is shown below:


```r
hist(date_summary$steps, main = "Total Daily Steps", xlab = "Steps", col = "steelblue")
```

![](https://github.com/koladefad/RepData_PeerAssessment1/blob/master/figure/total%20steps%20histogram-1.png)<!-- -->

3. The mean and median of the total number of daily steps was calculated.  
Mean:

```r
date_summary$steps %>% mean
```

```
## [1] 10766.19
```

Median:

```r
date_summary$steps %>% median
```

```
## [1] 10765
```

## Daily Activity Pattern
1. A time series plot of the 5-minute interval against the average number of steps, averaged across all days was created below:


```r
mydata_noNA <- filter(mydata, !is.na(mydata$steps)) ##Removing the NAs

interval_summary <- mydata_noNA %>%
                group_by(interval) %>%
                summarise(total_steps = sum(steps), average_steps = total_steps/length(unique(mydata_noNA$date)))

with(interval_summary, plot(interval, average_steps, main = "Average Daily Activity Pattern", type = "l", col = "#00AFBB", ylab = "Average Steps", xlab = "Interval"))
```

![](https://github.com/koladefad/RepData_PeerAssessment1/blob/master/figure/time%20series%20plot-1.png)<!-- -->

2. The 5-minute interval with the maximum number of steps was calculated as follows:


```r
interval_summary %>%
        arrange(desc(average_steps)) %>%
        head(1)
```

```
## # A tibble: 1 x 3
##   interval total_steps average_steps
##      <int>       <int>         <dbl>
## 1      835       10927          206.
```

Therefore, the interval "835" has the most number of steps with total steps of 10,927 and average steps of 206.

## Inputing Missing Values
1. First, the total number of missing values in the dataset was caluculated:


```r
mydata %>%
        filter(is.na(mydata$steps)) %>%
        nrow()
```

```
## [1] 2304
```

Hence, there are 2,304 rows with missing values in the dataset.

2. To replace these missing values, the mean of that 5-minute interval was used.

3. A new dataset "NewData" was created:

```r
NewData <- mydata %>%
                group_by(interval) %>%
                mutate_each(funs(replace(., which(is.na(.)),
                                 mean(., na.rm=TRUE))))
```

4. Another histogram was made from the new dataset.

```r
NewData_dailysteps <- NewData %>%
        group_by(date) %>%
        summarise(steps = sum(steps))

hist(NewData_dailysteps$steps, main = "Total Daily Steps", xlab = "Steps", col = "#ff6666")
```

![](https://github.com/koladefad/RepData_PeerAssessment1/blob/master/figure/new%20dataset%20histogram-1.png)<!-- -->

The addition of the missing numbers has increased the length of the middle bar.

The mean and median total number of steps of this new dataset was calculated.  
Mean:

```r
NewData_dailysteps$steps %>% mean
```

```
## [1] 10766.19
```

Median:

```r
NewData_dailysteps$steps %>% median
```

```
## [1] 10766.19
```

The mean remained unchanged but the median is now equal to the mean.

## Exploring Difference in Activity Patterns between Weekdays and Weekends

1. First a new factor variable with two levels "weekday" and "weekend" was created in the dataset.


```r
NewData$date <- as.POSIXct.Date(NewData$date) ##Converting the dates from factor to date format

weekend_days <- c("Saturday", "Sunday") ##Making a vector of weekend days

NewData$weekday <- factor((weekdays(NewData$date) %in% weekend_days), levels = c(TRUE, FALSE), labels = c("Weekend", "Weekday"))
```

2. A panel plot was made of the 5-minute interval and the average number of steps taken, averaged across all weekdays or weekend days.


```r
##Reorganising the data
interval_summary_new <- NewData %>%
        group_by(weekday, interval) %>%
        summarise(weekday = weekday, total_steps = sum(steps), average_steps = total_steps/length(unique(NewData$date)))

##The Plot
library(lattice)
xyplot(interval_summary_new$average_steps ~ interval_summary_new$interval | interval_summary_new$weekday, layout = c(1, 2), type = "l", main = "Weekend vs Weekday Activity Pattern", xlab = "Interval", ylab = "Number of Steps", col = "#651AD8")
```

![](https://github.com/koladefad/RepData_PeerAssessment1/blob/master/figure/weekdays%20and%20weekends%20panel%20plot-1.png)<!-- -->
