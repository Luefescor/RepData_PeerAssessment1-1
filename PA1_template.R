import(ggplot2)
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
unlink("activity.zip")

steps_per_day <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(steps_per_day, binwidth=1000, xlab="total number of steps taken each day")
mean(steps_per_day, na.rm=TRUE)
median(steps_per_day, na.rm=TRUE)

average_values <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=average_values, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps")

average_values[which.max(average_values$steps),]

missing <- is.na(data$steps)
table(missing)
compute_missing <- function(steps, interval) {
    complete <- NA
    if (!is.na(steps))
        complete <- c(steps)
    else
        complete <- (average_values[average_values$interval==interval, "steps"])
    return(complete)
}
complete_data <- data
complete_data$steps <- mapply(compute_missing, complete_data$steps, complete_data$interval)
total_steps_per_day <- tapply(complete_data$steps, complete_data$date, FUN=sum)
qplot(total_steps_per_day, binwidth=1000, xlab="total number of steps taken each day")
mean(total_steps_per_day)
median(total_steps_per_day)

day_type <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid")
}
complete_data$date <- as.Date(complete_data$date)
complete_data$day <- sapply(complete_data$date, FUN=day_type)

average_values <- aggregate(steps ~ interval + day, data=complete_data, mean)
ggplot(average_values, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")