activityRaw <- read.csv("activity.csv")

# Convert date column from factor to date character, in order to plot correctly
activityRaw$date <- as.Date.character(activityRaw$date)

# Filter out all NA values
getNA <- is.na(activityRaw$steps)
activity <- activityRaw[!getNA, ]

stepsDaily <- subset(activity, select=c(steps,date))

activityDailySteps <- aggregate(activity$steps, by=list(activity$date), FUN=sum)
names(activityDailySteps) <- c("date", "stepsDaily")
activityDailySteps$date

#plot(activityDailySteps, type="h")

activityMedian <- aggregate(activity$steps, by=list(activity$date), FUN=median)
names(activityMedian) <- c("date", "stepsDailyMedian")

activityPatternMean <- aggregate(activity$steps, FUN=mean, 
                                 by=list(activity$interval))
names(activityPatternMean) <- c("interval", "stepsMean")

plot(activityPatternMean, type="l", 
     xlab="Interval (1/300s)", 
     ylab="Average Daily Steps Per Interval (-)")

max <- format(max(activityPatternMean$stepsMean), digits=5)
indexMax <- which.max(activityPatternMean$stepsMean)
intervalMaxSteps <- activityPatternMean[indexMax,]$interval

activityNA <- activityRaw[getNA, ]
countNA <- dim(activityNA)[1]

# Set each NA steps value to the mean for each interval 
for (row in 1:countNA){
    interval <- activityNA$interval[row] # get the interval value for the NA row
    # get the mean interval step value and replace the NA value with it
    activityNA$steps[row] <- 
        activityPatternMean[activityPatternMean$interval==interval,]$stepsMean
}
activityAll <- rbind(activityNA, activity)

activityWeekdays <- mutate(activity, 
                           dayOfWeek = factor(
                               substring(weekdays(as.Date(date)),1,1) == "S",
                               labels=c("weekday","weekend")
                               ) 
                           )
activityWeekdaysMean <- setNames(aggregate(activityWeekdays$steps,
                                           by=list(activityWeekdays$dayOfWeek,
                                                   activityWeekdays$interval),
                                           FUN=mean)
                                 ,c("dayOfWeek","interval","steps"))
