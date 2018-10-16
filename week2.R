
# set the directory and read in the file 
setwd("C:/Users/Jack/Documents/R/Reproducable Research")
activity <- read.csv("activity.csv")

# group by date and sum the steps
steps.total <- aggregate(steps ~ date, data = activity, sum)

# histogram of steps per day
hist(steps.total$steps)

# Calculate and report the mean and 
#  median of the total number of steps taken per day

steps.mean <- mean(steps.total$steps)
steps.median <- median(steps.total$steps)
summary(steps.total$steps)

# time series of steps grouped by interval
steps.interval <- aggregate(steps ~ interval, data = activity, mean)
plot(steps.interval$interval,steps.interval$steps, type = "l")
print("5 minute interval with max average steps:")
print(steps.interval[which.max(steps.interval$steps),])

# report total NA values
print("total # of missing values = ")
sum(is.na(activity$steps))

# replace NA values with the average for the corresponding interval
averaged.activity <- activity 
i <- 0
for (steps in averaged.activity$steps){
        i <- i +1
        if(is.na(steps)==TRUE){
                y  <- averaged.activity[i,"interval"]
                averaged.activity[i,"steps"] <- 
                        steps.interval[match(y,steps.interval$interval),"steps"]
        } 
}

# group the new dataset by date and sum the steps
new.steps.total <- aggregate(steps ~ date, data = averaged.activity, sum)

# new histogram wih NA values replaced
hist(new.steps.total$steps)

# Calculate and report the mean and 
#  median of the total number of steps taken per day

new.steps.mean <- mean(new.steps.total$steps)
new.steps.median <- median(new.steps.total$steps)
summary(new.steps.total$steps)

# assign observation of weekday vs. weekend, TRUE = Weekend
averaged.activity$date <- as.POSIXct(averaged.activity$date)
library(dplyr)
weekend.days <- c("Saturday","Sunday")
averaged.activity <- averaged.activity %>% 
        mutate(weekend = weekdays(date) %in% weekend.days)

# create one DF for weekdays, one for weekend
weekday.steps <- averaged.activity %>% filter(weekend == FALSE)
weekday.steps <- aggregate(steps ~ interval, data = weekday.steps, mean)
        
weekend.steps <- averaged.activity %>% filter(weekend == TRUE) 
weekend.steps <- aggregate(steps ~ interval, data = weekend.steps, mean)

# plot two time series of steps by interval on weekdays and weekends
par(mfrow=c(2,1))
plot(weekday.steps$interval,weekday.steps$steps, type = "l")
plot(weekend.steps$interval,weekend.steps$steps, type = "l")

