## Question 1 
Code for reading in the dataset and/or processing the data

```{r echo=FALSE}
options(warn = -1) 
```

```{r}
setwd("~/")
if(!file.exists('activity.csv')){
  unzip('activity.zip')
}

Activity <- read.csv('activity.csv')
```

## Question 2
Histogram of the total number of steps taken each day

Data frame summing steps per date
```{r}
ActivitySteps <- tapply(Activity$steps, Activity$date, FUN=sum, na.rm=TRUE)
```
Create histogram of steps per day
```{r}
library(ggplot2)
qplot(ActivitySteps, binwidth = 1000, xlab = "Total Steps Per Day")
``` 

## Question 3
Mean and median number of steps taken each day

Create mean and median of steps per day
```{r}
MeanSteps <- mean(ActivitySteps, na.rm=TRUE)
MedianSteps <- median(ActivitySteps, na.rm=TRUE)
```
Mean steps per day
```{r}
MeanSteps
```
Median steps per day
```{r}
MedianSteps
```

## Question 4
Time series plot of the average number of steps taken

Create the interval means
```{r}
MeanByInt <- aggregate(x=list(steps=Activity$steps), 
by=list(interval=Activity$interval), FUN=mean, na.rm=TRUE)
```
Create plot
```{r}
library(ggplot2)

ggplot(data=MeanByInt, aes(x=interval, y=steps)) + geom_line() +
  ggtitle("Average Number of Steps") + xlab("5-minute Interval") +
  ylab("Average Steps")
```

## Question 5
The 5-minute interval that, on average, contains the maximum number of steps

Max
```{r}
MeanByInt[which.max(MeanByInt$steps),]
```

## Question 6
Code to describe and show a strategy for imputing missing data.  The theory here is to replace the NA's by the mean of the corresponding interval.

Copy the data frame
```{r}
Activity2 <- Activity
```
Add a column to the clone for the new index
```{r}
Activity2$Orig <- "original"
```
Number of rows to check
```{r}
l <- nrow(Activity2)
```
Numbers of NAs
```{r}
length(which(is.na(Activity2$steps)))
```
Replace NAs with the mean of the same interval
```{r}
for (i in 1:l) {
  if (is.na(Activity2[i,1])) {
    Activity2[i,1] <- MeanByInt[MeanByInt$interval == Activity2[i,3],2]
    Activity2[i,4] <- "completed"
  }
}
```
Recreate the data frame summing steps per date
```{r}
ActivitySteps2 <- tapply(Activity2$steps, Activity2$date, FUN=sum, na.rm=TRUE )
```
Recreate the mean and median of steps per day
```{r}
MeanSteps2 <- mean(ActivitySteps2)
MedianSteps2 <- median(ActivitySteps2)
```
Compare new mean steps with original mean steps
```{r}
c(MeanSteps2, MeanSteps)
```
Compare new median steps with original median steps
```{r}
c(MedianSteps2, MedianSteps)
```

## Question 7
Histogram of the total number of steps taken each day after missing values are imputed

Setup data for histogram
```{r}
OriginalPlot <- qplot(ActivitySteps, binwidth=1000, ylim=c(0,15),
               main="Original", xlab="Total steps per day")

AmendedPlot <- qplot(ActivitySteps2, binwidth=1000, ylim=c(0,15),
               main="Amended", xlab="Total steps per day")
```

Create histogram of steps per day
```{r}
library(ggplot2)
library(gridExtra)
require(gridExtra)

grid.arrange(OriginalPlot, AmendedPlot, ncol=2)
```

## Question 8
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Reformat and expand our amended data frame to identify a weekday (WDay) and weekend (WEnd)
```{r}
Activity2[,2] <- as.Date(Activity2[,2])
Activity2$WDay <- weekdays(Activity2[,2])
Activity2$WeekGroup <- "week"
```
Fill WeekGroup with weekend identifier
```{r}
for (i in 1:l) {
  if (Activity2[i,5] == "Saturday" | Activity2[i,5] == "Sunday") {
    Activity2[i,6] <- "weekend"
  }
}

Activity2[,6] <- as.factor(Activity2[,6])

Activity2WDay <- subset(Activity2,Activity2[,6] == "week")
Activity2WEnd <- subset(Activity2,Activity2[,6] == "weekend")
```
Recreate the mean by weekday and weekend
```{r}
MeanWDay <- aggregate(steps ~ interval, Activity2WDay, FUN=mean)
MeanWEnd <- aggregate(steps ~ interval, Activity2WEnd, FUN=mean)
```
Prepare the plots
```{r}
PlotWDay <- ggplot(data = MeanWDay, aes(x=interval, y=steps)) +
         geom_line() + ylim(0, 250) + ggtitle("Weekday") +
         xlab("5-minute Interval") + ylab("Average Steps")

PlotWEnd <- ggplot(data = MeanWEnd, aes(x=interval, y=steps)) +
         geom_line() + ylim(0, 250) + ggtitle("Weekend") +
         xlab("5-minute Interval") + ylab("Average Steps")
```
Load and plot charts
```{r}
library(ggplot2)
library(gridExtra)
require(gridExtra)
```
Plot
```{r}
grid.arrange(PlotWDay, PlotWEnd, ncol=2)
```

## Question 9
All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

The underlying R Markdown document contains all of the R code needed to reproduce the report.