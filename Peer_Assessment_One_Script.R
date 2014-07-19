######################################################################################
# Author:       Wayward-Economist
# Date:         7/16/14
# Github:       https://github.com/Wayward-Economist/Getting_and_Cleaning_Data_Project
# Description:  This script includes the code for the first peer assessment project in
#               the Coursera course "Reproducible Research."
# Notes:        The script contains the R code chunks that will be imported into the
#               R markdown document "Peer_Assessment_One.Rmd" document; that document
#               will be submited in order to satisfy the course requirements.
######################################################################################

## ---- Load__Preprocessing_Data ----
## Load the data and change the formats of the date and interval variables.
activity          <- read.csv("activity.csv")
activity$date     <- as.Date(activity$date, format = '%Y-%m-%d')

## ---- Hist_Steps ----
## Use the aggregate function to calculate the total number of steps taken per day and plot a histogram.
daily <- aggregate(steps ~ date, activity, sum)
hist(daily[, 2], breaks = 20, xlab = "Steps", main = "Total Steps Taken per Day", col = "aliceblue")
mean(daily[, 2])
median(daily[, 2])

## ---- Daily_Pattern ----
## Use ddply and the plot function to plot the averate number of steps taken per minute.
mean_steps <- ddply(activity, .(interval), summarize, mean(steps, na.rm = TRUE))
plot(mean_steps, type = "l", ylab = "Average Steps", xlab = "Cumulative Minute", lwd = 2, col = "darkred")
mean_steps[which(mean_steps[, 2] == max(mean_steps[, 2])), 1]

## ---- Impute_Missing ----
## Calculate the number of missing values and impute them with the average for the corresponding interval.
missing              <- is.na(activity$steps)
activity$imputed     <- activity$steps
activity[missing, 4] <- rep(mean_steps[,2], times = nlevels(as.factor(activity$date)))[missing]
daily_im             <- aggregate(imputed ~ date, activity, sum)[, 2]
hist(daily_im, breaks = 20, xlab = "Steps", main = "Total Steps Taken per Day", col = "aliceblue")
mean(daily_im)
median(daily_im)

## ---- Compare_Hist ----
## Compare the histogram of average steps taken per day by plotting the two histograms next to each other.
par(mfrow=c(1, 2))
hist(aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)[, 2], breaks = 20, xlab = "Steps", main = "Steps Taken per Interval", col = "aliceblue")
hist(aggregate(activity$imputed, by = list(activity$date), FUN = sum, na.rm = TRUE)[, 2], breaks = 20, xlab = "Steps", main = "Imputed Steps Taken per Interval", col = "aliceblue")
par(mfrow=c(1, 1))

## ---- Compare_Weekday ----
## Compare the activity pattern on weekdays with the pattern on weekends.
activity$weekday <- "Weekday"
activity[(weekdays(activity$date) == "Saturday" | weekdays(activity$date) == "Sunday"), 5] <- "Weekend"
mean_steps <- aggregate(imputed ~ interval + weekday, data = activity, mean)
xyplot(imputed ~ interval | weekday, data = mean_steps, layout = c(1, 2), type = "l", lwd = 2, col = "darkred")