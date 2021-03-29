## Load the data into R using read.csv creating variable “Activity”

    Activity <- read.csv("activity.csv")

## First question: What is mean total number of steps taken per day?

    library(ggplot2)
    totsteps <- tapply(Activity$steps, Activity$date, FUN = sum, na.rm = TRUE)
    qplot(totsteps, binwidth = 1000, xlab = "Total Number of Steps Taken per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    mean(totsteps, na.rm = TRUE)

    ## [1] 9354.23

    median(totsteps, na.rm = TRUE)

    ## [1] 10395

## Second question: What is the average daily activity pattern?

    library(ggplot2)
    average_daily <- aggregate(x = list(steps = Activity$steps), by = list(interval = Activity$interval), FUN = sum, na.rm = TRUE)
    ggplot(data = average_daily, aes(x = interval, y = steps)) +
      geom_line()+
      xlab("5-min Interval") + 
      ylab("Average of Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

    average_daily[which.max(average_daily$steps),]

    ##     interval steps
    ## 104      835 10927

## Third question: Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

    miss_values <- is.na(Activity$steps)
    table(miss_values)

    ## miss_values
    ## FALSE  TRUE 
    ## 15264  2304

The strategy for filling in all of the missing values in the dataset
will be the mean value of that 5-minute interval.

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

Make a histogram of the total number of steps taken each day and,
calculate and report the mean and median total number of steps taken per
day.

    totsteps <- tapply(fill_Activity$steps, fill_Activity$date, FUN = sum)
    qplot(totsteps, binwidth = 1000, xlab = "Total Number of Steps Taken per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    mean(totsteps)

    ## [1] 84188.07

    median(totsteps)

    ## [1] 11458

Do these values differ from the estimates from the first part of the
assignment? What is the impact of imputing missing data on the estimates
of the total daily number of steps?

The values of the mean and median are higher than the ones calculated in
the first part of the assignment. The missing values NA that used to
make the total number of steps 0, will be removed since NA’s were
imputed with the average daily activity pattern.

## Fourth question: Are there differences in activity patterns between weekdays and weekends?

For this part lets use the weekdays() function to distinguish weekdays
from weekends in the dataset; the filled-in missing values dataset will
be used for this part: \`fill\_Activity´.

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

Creating a panel plot containing a time series plot (i.e. type = “l”) of
the 5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis).

    Average <- aggregate(steps ~ interval + day, data = fill_Activity, mean)
    ggplot(Average, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-min interval") + ylab("Number of Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

## THANK YOU!
