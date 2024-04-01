---
title: "Reproducible Research Course


        Data Analysis with R | Johns Hopkins University"
        
author: "Nelson Daniel Rivas"
date: "2024-03-16"
output: html_document
---

In this document in HTML format, the data analysis requested in project number 1 of the “Reproducible Research” course at Johns Hopkins University is carried out.

## Data loading and preprocessing
Show any code that is necessary to:

1. Load the data (i.e. read.csv())



```r
library(tidyverse) # load the libraries
library(formattable)
library(lubridate)
library(lattice)
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# then, I check if the file containing the data exists in my directory; 
# if not, then I download the data:
if (file.exists("Data.zip") == FALSE){
  download.file(url, destfile = "Data.zip")
  unzip("Data.zip")
}
# finally, I read the data and save it as a tibble:
df_activity <- read_csv(file = "activity.csv")
```
2. Process/transform data (if necessary) into a format suitable for analysis

```r
# no es necesario por ahora
```
## What is the average total number of steps taken per day?

For this part of the task, you can ignore missing values ​​in the data set.

1. Calculate the total number of steps taken per day

```r
df_activity %>% 
  group_by(date) %>%
  rename("Date" = date) %>%
  summarise(`Steps per day` = sum(steps)) %>%
  formattable(align = c("c", "r", "r", "r")) %>%
  as.htmlwidget(width = "30%")
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

2. If you don't understand the difference between a histogram and a bar chart, research the difference between them. Make a histogram of the total number of steps taken each day

```r
df_activity %>% 
  group_by(date) %>%
  summarise(steps_per_day = sum(steps)) %>%
  ggplot(aes(x = steps_per_day)) +
  geom_histogram(bins = 30, fill = "coral1", col = "white") +
  labs(title = "Histogram of steps per day", 
       y = "frequency", 
       x = "steps")
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31-1.png)


3. Calculate and report the mean and median of the total number of steps taken per day

```r
df_activity %>% 
  group_by(date) %>%
  summarise(`Steps per day` = sum(steps),
            `Average per day` = mean(steps),
            `Median per day` = median(steps)) %>%
  mutate(date = as.character(date)) %>%
  rename("Date" = date) %>%
  formattable(align = c("c", "r", "r", "r")) %>%
  as.htmlwidget(width = "50%")
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```
## What is the average daily activity pattern?
1. Make a time series graph (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged over all days (y-axis).


```r
df01 <- df_activity %>%
  group_by(interval) %>%
  summarise(`Average per day` = mean(steps, na.rm = TRUE))

xmax <- filter(df01, `Average per day` == max(`Average per day`))[1, 1] %>%
             as.numeric()
ymax <- filter(df01, `Average per day` == max(`Average per day`))[1, 2] %>%
             as.numeric()
df01 %>%
ggplot(aes(interval, `Average per day`)) +
  geom_line(color = "blue") +
  annotate(geom = "point", shape = 21, size = 5, fill = "red", 
           color = "black", alpha = 0.3, x = xmax, y = ymax) +
  annotate(geom = "text", x = xmax + 220, y = ymax, 
           label = paste("(", xmax, ", ", round(ymax, 0), ")", sep = ""))
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-33-1.png)

2. What 5-minute interval, averaged over all days in the data set, contains the maximum number of steps?  

The maximum number of steps as an average of the steps in the five-minute intervals of all days is 206 and is obtained in the interval 835.

## Impute missing values

Please note that there are a number of days/intervals where values ​​are missing (coded as NA). The presence of missing days may introduce bias in some calculations or data summaries.  

1. Calculate and report the total number of missing values ​​in the data set (that is, the total number of rows with NAs)

```r
act_withoutNA <- drop_na(df_activity)
act_withNA <- dplyr::setdiff(df_activity, act_withoutNA)
nrow(act_withNA)
```

```
## [1] 2304
```
The original data set has 2304 rows in which one of the variables has a missing value.

2. Design a strategy to fill in all missing values in the data set. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

In my case, I will use the average of the 5 minute interval to fill the cells with missing values

3. Create a new data set that is the same as the original data set but with the missing data filled in.

So, following the strategy I have defined, I get the following set of data:

```r
df_actFinal <- df_activity %>%
  mutate(steps = ifelse(is.na(steps), 
                        ifelse(interval %in% df01$interval, df01$`Average per day`, NA),
                        steps))

identical(df_actFinal, drop_na(df_actFinal))
```

```
## [1] TRUE
```
There are no longer missing values in the data set.

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median of the total number of steps taken each day. Do these values differ from the estimates from the first part of the task? What is the impact of imputation of missing data on estimates of total daily steps?

```r
df_actFinal %>% 
  group_by(date) %>%
  summarise(steps_per_day = sum(steps)) %>%
  ggplot(aes(x = steps_per_day)) +
  geom_histogram(bins = 30, fill = "coral1", col = "white") +
  labs(title = "Histogram of steps per day", 
       y = "frequency", 
       x = "steps")
```

![plot of chunk unnamed-chunk-36](figure/unnamed-chunk-36-1.png)


```r
df_actFinal %>% 
  group_by(date) %>%
  summarise(`Steps per day` = sum(steps),
            `Average per day` = mean(steps),
            `Median per day` = median(steps)) %>%
  mutate(date = as.character(date)) %>%
  rename("Date" = date) %>%
  formattable(align = c("c", "r", "r", "r")) %>%
  as.htmlwidget(width = "50%")
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```
The impact of imputation of missing values in the data set with the method used is mainly reflected in the median. When I summarize the mean and median steps per day I get a mean for the records that previously had missing values that is similar to the value seen in the other rows of the summary. However, the median of the records that previously had missing values in the calculated summary is a very high value and close to the calculated mean for that summary, while in the other rows of the summary the median value is zero. Additionally, for all rows in the summary where missing values previously appeared, the mean and median value is the same (mean = 37.38, median = 34.11).

## Are there differences in activity patterns between weekdays and weekends?

For this part, the weekdays() function can help. Use the data set with the missing values filled in for this part.

1. Create a new factor variable in the data set with two levels: "weekday" and "weekend" that indicate whether a given date is a weekday or a weekend day.

```r
df_weekdays <- df_actFinal %>%
  mutate(`Weekday` = ifelse(wday(date, label = F) %in% c(6, 7),
                            "weekend", "laborable_day"))
```

2. Make a panel chart containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged over all weekdays or weekends (y-axis). See the README file in the GitHub repository for an example of what this graph should look like using simulated data.

```r
df_wdSummarise <- df_weekdays %>%
  group_by(interval, Weekday) %>%
  summarise(`Average per day` = mean(steps, na.rm = TRUE))

xyplot(`Average per day` ~ interval | Weekday, data = df_wdSummarise, type = "l")
```

![plot of chunk unnamed-chunk-39](figure/unnamed-chunk-39-1.png)

