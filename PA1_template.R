## ----load data, echo = TRUE---------------------------------------------------------
Activity <- read.csv("activity.csv")


## ---- echo = TRUE-------------------------------------------------------------------
library(ggplot2)
totsteps <- tapply(Activity$steps, Activity$date, FUN = sum, na.rm = TRUE)
qplot(totsteps, binwidth = 1000, xlab = "Total Number of Steps Taken per Day")
mean(totsteps, na.rm = TRUE)
median(totsteps, na.rm = TRUE)


## ---- echo = TRUE-------------------------------------------------------------------
library(ggplot2)
average_daily <- aggregate(x = list(steps = Activity$steps), by = list(interval = Activity$interval), FUN = sum, na.rm = TRUE)
ggplot(data = average_daily, aes(x = interval, y = steps)) +
  geom_line()+
  xlab("5-min Interval") + 
  ylab("Average of Steps")


## ---- echo = TRUE-------------------------------------------------------------------
average_daily[which.max(average_daily$steps),]


## ---- echo = TRUE-------------------------------------------------------------------
miss_values <- is.na(Activity$steps)
table(miss_values)


## ---- echo = TRUE-------------------------------------------------------------------
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
filling_values <- function(steps, interval) {
  fill <- NA
  if (!is.na(steps))
    fill <- c(steps)
  else
    fill <- (average_daily[average_daily$interval == interval, "steps"])
  return(fill)
}
fill_Activity <- Activity
fill_Activity$steps <- mapply(filling_values, fill_Activity$steps, fill_Activity$interval)


## ---- echo = TRUE-------------------------------------------------------------------
totsteps <- tapply(fill_Activity$steps, fill_Activity$date, FUN = sum)
qplot(totsteps, binwidth = 1000, xlab = "Total Number of Steps Taken per Day")
mean(totsteps)
median(totsteps)


## ---- echo = TRUE-------------------------------------------------------------------
day.end <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

fill_Activity$date <- as.Date(fill_Activity$date)
fill_Activity$day <- sapply(fill_Activity$date, FUN = day.end)


## ---- echo = TRUE-------------------------------------------------------------------
Average <- aggregate(steps ~ interval + day, data = fill_Activity, mean)
ggplot(Average, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-min interval") + ylab("Number of Steps")

