## Reproducible Research - Prog Assignment 1
## - fork repo RepData_PeerAssessment1, which included the activity data
##   Forked manually from within Github from
##      https://github.com/rdpeng/RepData_PeerAssessment1
##      with SHA-1 hash c0525389ea6e8a244d4766efa36364c84cc1fff8
##      then "git cloned" locally
## - read the human activity data
## - process to key the table for easy summation
## - create markdown file with embedded R code.

library(stat)
library(plyr)
library(data.table)
library(dplyr)

humActData <- data.table(read.csv(unz("activity.zip", "activity.csv"),
                            colClasses = c("date" = "POSIXct", 
                                            "steps" = "integer")))

humActData <- mutate(humActData, dateFac = as.factor(date),
                                intvlFac = as.factor(interval),
                                avail = complete.cases(humActData))
setkey(humActData, dateFac, intvlFac, avail)

## What is mean total number of steps taken per day?
## For this part of the assignment, you can ignore the missing values in the dataset.
## 1. Make a histogram of the total number of steps taken each day
## 2. Calculate and report the mean and median total number of steps taken per day

dayInfo <- humActData[avail == TRUE, sum(steps), by = dateFac]
setnames(dayInfo, 2, "stepsDay")
par(mar = c(5,4,3,1), 
    mfrow = c(1,1),
    cex = 0.8)
hist(dayInfo$stepsDay,22, main = "steps/day ignore NA")

dayMeanNoNA <- mean(dayInfo$stepsDay)
dayMeanNoNA
dayMedNoNA <- median(dayInfo$stepsDay)
dayMedNoNA

## What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
##      and the average number of steps taken, averaged across all days (y-axis)
## 2. Which 5-minute interval, on average across all the days in the dataset, 
##      contains the maximum number of steps?

intvlInfo <- humActData[avail == TRUE, mean(steps), by=intvlFac]
setnames(intvlInfo, 2, "meanStepsIntvl")
par(mar = c(5,4,3,1), 
    mfrow = c(1,1),
    cex = 0.8,
    bty = "n")
plot(intvlInfo, 
     main = "mean steps per daily interval", 
     xlab = "5 minute intervals - hhmm")
lines(intvlInfo)

maxIntvl <- intvlInfo[which.max(intvlInfo$meanStepsIntvl),]
maxIntvl

## Imputing missing values
## 1. Calculate and report the total number of missing values in the dataset 
##      (i.e. the total number of rows with NAs)
## 2. Devise a strategy for filling in all of the missing values in the dataset. 
##      The strategy does not need to be sophisticated. For example, you could use
##      the mean/median for that day, or the mean for that 5-minute interval, etc.
##      Strategy for imputing values:  Calculate an array of means for each time
##          interval.  Whether for a full missing day or an occasional missng 
##          value, replace the NA with the mean for that time interval.
## 3. Create a new dataset that is equal to the original dataset but with the 
##      missing data filled in.
## 4. Make a histogram of the total number of steps taken each day and Calculate 
##      and report the mean and median total number of steps taken per day. Do 
##      these values differ from the estimates from the first part of the assignment? 
##      What is the impact of imputing missing data on the estimates of the total 
##      daily number of steps?

##  Determine the extent of missing data and ensure humActData is not missing intervals
totDays <- length(table(humActData$dateFac))
totIntvlDay <- 24*(60/5)
totIntvlFile <- length(humActData$dateFac)
totIntvlFile == totDays * totIntvlDay
missingValues <- sum(!humActData$avail)
missingValues
missingDays <- totDays - (length(dayInfo$dateFac))
missingDays

##  Use intvlInfo which contains the mean of each interval to fill in values
##  Need to build a vector as long as all the intervals in all the days
repdIntvlInfo <- intvlInfo$meanStepsIntvl
for (i in 1:(totDays - 1)) {
    repdIntvlInfo <- c(repdIntvlInfo,intvlInfo$meanStepsIntvl)
}

humActDataImpNA <- humActData
humActDataImpNA$steps[is.na(humActDataImpNA$steps)] <- 
    repdIntvlInfo[is.na(humActDataImpNA$steps)]

dayInfoImpNA <- humActDataImpNA[, sum(steps), by=dateFac]
setnames(dayInfoImpNA, 2, "stepsDay")
par(oma = c(3,2,2,1), 
    mar = c(1,1,1,1), 
    mfrow = c(1,2),
    cex = 0.6)

hist(dayInfo$stepsDay, 22, 
     main = "steps/day ignore NA",
     ylim = c(0,18))
hist(dayInfoImpNA$stepsDay,22,
     main = "steps/day impute NA",
     ylim = c(0,18))
dayMeanImpNA <- mean(dayInfoImpNA$stepsDay)
dayMeanImpNA
dayMedImpNA <- median(dayInfoImpNA$stepsDay)
dayMedImpNA

## Are there differences in activity patterns between weekdays and weekends?
## For this part the weekdays() function may be of some help here. Use the dataset 
##      with the filled-in missing values for this part.
## 1. Create a new factor variable in the dataset with two levels - "weekday" and 
##      "weekend" indicating whether a given date is a weekday or weekend day.
## 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 
##      5-minute interval (x-axis) and the average number of steps taken, averaged 
##      across all weekday days or weekend days (y-axis).

humActDataImpNA <- mutate(humActDataImpNA, 
                dayType = factor(grepl("Saturday|Sunday", weekdays(date)),
                                 labels = c("weekday", "weekend")))   
setkey(humActDataImpNA, dateFac, intvlFac, avail, dayType)

dayTypeIntvlInfo <- humActDataImpNA[ , mean(steps), by = c("intvlFac", "dayType")]
setnames(dayTypeIntvlInfo, 3, "meanStepsIntvl")
weekdayIntvlInfo <- filter(dayTypeIntvlInfo, dayType == "weekday")
weekdayIntvlInfo <- select(weekdayIntvlInfo, intvlFac, meanStepsIntvl)
weekendIntvlInfo <- filter(dayTypeIntvlInfo, dayType == "weekend")
weekendIntvlInfo <- select(weekendIntvlInfo, intvlFac, meanStepsIntvl)
par(oma = c(1,1,1,1), 
    mar = c(4,4,2,1), 
    mfrow = c(2,1),
    cex = 0.6,
    bty = "n")
plot(weekdayIntvlInfo, 
     main = "weekday mean steps/intvl", 
     xlab = "",
     ylab = "steps/intvl",
     ylim = c(0,250))
lines(weekdayIntvlInfo)
plot(weekendIntvlInfo, 
     main = "weekend mean steps/intvl", 
     xlab = "intervals (hhmm)",
     ylab = "steps/intvl",
     ylim = c(0,250))
lines(weekendIntvlInfo)

dayTypeInfo <- humActDataImpNA[, sum(steps), by = c("dateFac", "dayType")]
setnames(dayTypeInfo, 3, "stepsDay")
weekdayInfo <- filter(dayTypeInfo, dayType == "weekday")
weekendInfo <- filter(dayTypeInfo, dayType == "weekend")
par(oma = c(3,2,2,1), 
    mar = c(1,1,1,1), 
    mfrow = c(1,2),
    cex = 0.6)

hist(weekdayInfo$stepsDay, 22, 
     main = "weekday steps/day",
     ylim = c(0,13))
hist(weekendInfo$stepsDay,22,
     main = "weekend steps/day",
     ylim = c(0,13))


