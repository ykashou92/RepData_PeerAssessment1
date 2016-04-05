# Download and unzip dataset
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
f <- file.path(getwd(), "activity_dataset.zip")
download.file(url, f)

if(!file.exists("activity.csv")) {
        unzip(f)
}

# Load necessary libraries
library(ggplot2)

# Import dataset
import <- read.csv("activity.csv", header = TRUE, sep = ",")

# Explore
head(import) 	# Reveals dataset is in the form (steps, date, interval)
str(import) 	# Reveals dataset is 3 Columns and 17568 Rows

# Calculate the steps taken each day
steps_each_day <- tapply(import$steps, as.factor(import$date),sum)

# Plot a histogram showing the steps taken each day
qplot(steps_each_day,
	geom = "histogram",
	main = "Histogram - Total Steps Taken Each Day",
	xlab = "Steps",
	ylab = "Frequency",
	bins = 8,
	fill = I("grey"),
	col = I("black"))

# Calculate the mean and median of the steps taken each day
mean(steps_each_day, na.rm = TRUE)
## [1] 10766.19
median(steps_each_day, na.rm = TRUE)
## [1] 10765

#Aggregate to average the number of steps across all days
interval_steps <- aggregate(steps ~ interval, import, mean)

# Time-Series Plot
tplot <- ggplot(interval_steps, (aes(x = interval_steps$interval, y = interval_steps$steps))) + geom_line(size=1, colour = "#000080") +
xlab("Interval") + ylab("Average Daily Steps") +
ggtitle("Average Daily Activity Pattern")
tplot

# Maximum number of steps on average
max.steps <- which.max(interval_steps$steps)
max.steps
## [1] 104

# Interval at which occurs maximum number of steps on average
interval.max.steps <- interval_steps[max.steps, ]
interval.max.steps
##      interval    steps
##  104      835 206.1698

# We know from the nature of the data that only the 'steps' column contains missing values.
# We can easily use the sum function to find the value of NAs
# as when "is.na = TRUE", it returns a value of 1, and if is.na = FALSE, returns a value of 0.
# Then the sum will be the number of NAs.
# And thus giving us the number of rows where they occur out of 17568
missing <- sum(is.na(import$steps))
missing

# One possible strategy is to use the mean value instead of the NA values.
avg_steps <- mean(interval_steps$steps)

# Duplicate dataset 
import_imputed <- import

# Replace NAs with Average
import_imputed$steps[is.na(import_imputed$steps)] <- avg_steps

# 'import' dataset should be intact
head(import)

# 'import_imputed' dataset should have NAs set = 37.3826 (avg_steps)
head(import_imputed)

# Calculate the steps taken each day
imputed.steps_each_day <- tapply(import_imputed$steps, as.factor(import_imputed$date),sum)

# Plot a histogram showing the steps taken each day
qplot(imputed.steps_each_day,
	geom = "histogram",
	main = "Histogram - Total Steps Taken Each Day",
	xlab = "Steps",
	ylab = "Frequency",
	bins = 8,
	fill = I("grey"),
	col = I("black"))

# Calculate the imputed mean number of the steps taken each day
mean(imputed.steps_each_day)

# Calculate the imputed median number of the steps taken each day
median(imputed.steps_each_day)

# Convert date column to date class
import_imputed$date <- as.Date(import_imputed$date)
# Add the day of the week column
import_imputed$weekday = weekdays(import_imputed$date)
# Add the weekday or weekend column
import_imputed$weekday.type <- ifelse(import_imputed$weekday == "Saturday" | import_imputed$weekday == "Sunday", "Weekend", "Weekday")
# Convert the column to factor
import_imputed$weekday.type <- factor(import_imputed$weekday.type)
# Display the dataset now
head(import_imputed)

#Aggregate to average the number of steps across all days for the imputed dataset
imputed.interval_steps <- aggregate(steps ~ interval + weekday.type, import_imputed, mean)

# Plot the time series Weekdays Vs Weekends
t2plot <- ggplot(imputed.interval_steps, 
	aes(x = imputed.interval_steps$interval, y = imputed.interval_steps$steps)) + 
	geom_line(size=0.8, colour = "#b22222") +
	xlab("Interval") +
	ylab("Average Number of Steps") +
	ggtitle("Weekdays Vs Weekends") +
  	facet_wrap(~ weekday.type, ncol = 1)
t2plot