---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
1.Load the data (i.e. read.csv())  

```r
	act<-read.csv("activity.csv")
```
2.Process/transform the data (if necessary) into a format suitable for your analysis  

```r
	library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
		act$date<-ymd(act$date)
```
## What is mean total number of steps taken per day?
1.Calculate the total number of steps taken per day  

```r
	stepsS<-tapply(act$steps,act$date,sum,na.rm=TRUE)
```
2.Make a histogram of the total number of steps taken each day  

```r
	hist(stepsS)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
3. Calculate and report the mean and median of the total number of steps taken per day  

```r
	mean(stepsS)
```

```
## [1] 9354.23
```

```r
	median(stepsS)
```

```
## [1] 10395
```
## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```r
	stepsA<-tapply(act$steps,act$interval,mean,na.rm=TRUE)
		plot(stepsA,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
		which(stepsA==max(stepsA))
```

```
## 835 
## 104
```
## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
	summary(act$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
	
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.  

```r
	vect<-rep(0,17568)
	for (i in 1:288) {
		for (j in 1:61) {
			vect[j+61*(i-1)]<-stepsA[i]
		}
	}
	actt<-act
	for (k in 1:17568){
		if (is.na(actt$steps[k])) {
			actt$steps[k]<-vect[k]
		}
	}
```
4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

```r
stepstS<-tapply(actt$steps,actt$date,sum,na.rm=TRUE)
	hist(stepstS)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
	mean(stepstS)
```

```
## [1] 10889.8
```

```r
	median(stepstS)
```

```
## [1] 11015
```
## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  

```r
	week<-weekdays(actt$date)
	weekd<-rep(0,17568)
		for (l in 1:17568){
			if (week[l]=="Saturday" || week[l]=="Sunday") {
				weekd[l]<-"Weekend"
			} else {
				weekd[l]<-"Weekday"
			}
		}
	weekd<-as.factor(weekd)		
	acttt<-cbind(actt,weekd)
```
2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  

```r
	wd<-subset(acttt,acttt$weekd=="Weekday")
		stepsttAwd<-tapply(wd$steps,wd$interval,mean)
	we<-subset(acttt,acttt$weekd=="Weekend")
		stepsttAwe<-tapply(we$steps,we$interval,mean)
	quartz()
	par(mfrow=c(2,1))
	plot(stepsttAwd,type="l")
	plot(stepsttAwe,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
