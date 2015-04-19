#To be completed and then pasted into R Markdown file as appropriate

#Read .csv data into R:

activity_data = read.csv("Activity.csv",stringsAsFactors = FALSE)

#Calculate the total number of steps taken per day

steps_by_day = as.data.frame(tapply(activity_data$steps, activity_data$date, sum, na.rm = TRUE))
colnames(steps_by_day)[1] = "total_steps_per_day"

#Make a histogram of the total number of steps taken each day.
hist(steps_by_day$total_steps_per_day, xlab = "Total Number of Steps", ylab = "Frequency", col = "blue", breaks = 20, main = "Steps by Day Histogram")

steps_by_day_mean = mean(steps_by_day$total_steps_per_day)
steps_by_day_median = median(steps_by_day$total_steps_per_day)

#Calculate and report the mean and median of the total number of steps taken per day**    
steps_by_day_mean
steps_by_day_median

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
steps_by_interval = as.data.frame(tapply(activity_data$steps, activity_data$interval, mean, na.rm = TRUE))
steps_by_interval <- cbind(Row.Names = as.numeric(rownames(steps_by_interval)), steps_by_interval)
colnames(steps_by_interval)[1] = "interval"
colnames(steps_by_interval)[2] = "average_steps"

plot(steps_by_interval$interval,steps_by_interval$average_steps, xlab = "Time Interval", ylab = "Average Number of Steps", main="", col = "blue", pch = 20)

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_steps = max(steps_by_interval$average_steps)
max_steps_by_interval = subset(steps_by_interval, average_steps == max_steps)
max_interval = max_steps_by_interval[1,1]
max_interval

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missing = which(is.na(activity_data$steps))
length(missing)

#Create a new dataset that is equal to the original dataset but with the missing data filled in.

adjusted_activity_data = merge(activity_data,steps_by_interval,by.x="interval",by.y="interval")
head(adjusted_activity_data)

adjusted_activity_data$adjusted_steps = ifelse(is.na(adjusted_activity_data$steps),adjusted_activity_data$average_steps,adjusted_activity_data$steps)
head(adjusted_activity_data,20)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
adj_steps_by_day = as.data.frame(tapply(adjusted_activity_data$adjusted_steps, adjusted_activity_data$date, sum, na.rm = TRUE))
colnames(adj_steps_by_day)[1] = "total_adj_steps_per_day"

hist(adj_steps_by_day$total_adj_steps_per_day, xlab = "Total Adjusted Number of Steps, with interval mean imputation", ylab = "Frequency", col = "green", breaks = 20, main = "Steps by Day Histogram (with imputed missing data)")

adj_steps_by_day_mean = mean(adj_steps_by_day$total_adj_steps_per_day)
adj_steps_by_day_median = median(adj_steps_by_day$total_adj_steps_per_day)

#Calculate and report the mean and median of the total number of steps taken per day**    
adj_steps_by_day_mean
adj_steps_by_day_median


#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
adjusted_activity_data$date = as.Date(adjusted_activity_data$date)

adjusted_activity_data$day_of_week = weekdays(adjusted_activity_data$date)
adjusted_activity_data$weekday = as.factor(ifelse(adjusted_activity_data$day_of_week %in% c("Saturday","Sunday"),"Weekend","Weekday"))
adjusted_activity_data$day_of_week = NULL
head(adjusted_activity_data)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
weekday_data = adjusted_activity_data[adjusted_activity_data$weekday == "Weekday",]
weekend_data = adjusted_activity_data[adjusted_activity_data$weekday == "Weekend",]

weekday_steps_by_interval = as.data.frame(tapply(weekday_data$adjusted_steps, weekday_data$interval, mean, na.rm = TRUE))
weekday_steps_by_interval <- cbind(Row.Names = as.numeric(rownames(weekday_steps_by_interval)), weekday_steps_by_interval)
colnames(weekday_steps_by_interval)[1] = "interval"
colnames(weekday_steps_by_interval)[2] = "average_steps"
weekday_steps_by_interval$weekday = "Weekday"

weekend_steps_by_interval = as.data.frame(tapply(weekend_data$adjusted_steps, weekend_data$interval, mean, na.rm = TRUE))
weekend_steps_by_interval <- cbind(Row.Names = as.numeric(rownames(weekend_steps_by_interval)), weekend_steps_by_interval)
colnames(weekend_steps_by_interval)[1] = "interval"
colnames(weekend_steps_by_interval)[2] = "average_steps"
weekend_steps_by_interval$weekday = "Weekend"

#chart_data = rbind(weekday_steps_by_interval,weekday_steps_by_interval)
#head(chart_data)

plot(weekday_steps_by_interval$interval,weekday_steps_by_interval$average_steps, xlab = "Time Interval", ylab = "Average Number of Steps", main="Weekday", col = "blue", pch = 20)

plot(weekend_steps_by_interval$interval,weekend_steps_by_interval$average_steps, xlab = "Time Interval", ylab = "Average Number of Steps", main="Weekend", col = "blue", pch = 20)


#library(ggplot2)

#qplot(interval,average_steps,data = adj_steps_by_interval, facets = .~weekday)