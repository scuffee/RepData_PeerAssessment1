---
title: "PA1_template.Rmd"
author: "Sandra Cuffee"
date: "November 15, 2015"
output: html_document
---

 This assignment makes use of data from a personal activity monitoring device.
 This device collects data at 5 minute intervals through out the day.
 The data consists of two months of data from an anonymous individual collected during the
 months of October and November, 2012 and include the number of stepstaken in 5 minute
 intervals each day.
 The variables included in this dataset are:
 steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
 date: The date on which the measurement was taken in YYYY-MM-DD format
 interval: Identifier for the 5-minute interval in which measurement was taken
 The dataset is stored in a comma-separated-value (CSV) file and
 there are a total of 17,568 observations in this dataset.


Add packages to execute script


```r
library(data.table)
library(plyr)
library(dplyr)
library (tidyr)
library(stringr)
library(graphics)
```

 Load the function
 

```r
  data_analysis <- function()
  {
  
      # download te data
    
      fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      download.file(fileUrl, destfile = "C:/Users/Owner/Documents/specdata/repdata_data_activity.zip", method = "libcurl")
      unzip("C:/Users/Owner/Documents/specdata/repdata_data_activity.zip")
      
      #Process the data with the NA's removed
     
      activityData <- na.omit(read.csv("C:/Users/Owner/Documents/specdata/activity.csv"))  
      activityDataSteps <- group_by(activityData, date)
      activityDataSteps 
      
      # Calulate the daily steps
      
      png(file = "plotdata_analysis1.png", width = 600, height = 600)
      hist(activityDataSteps$steps,main = " Daily Steps", xlab = "Steps", col ="red")
      dev.off()
      
      # Caluculate the mean number of daily steps
      
      activityDataSteps1 <- summarize(activityDataSteps, Mean = mean(steps))
      activityDataSteps1        
  
      # Create histogram representing the daily mean number of daily steps
      
      png(file = "plotdata_analysis2.png", width = 600, height = 600)
      hist(activityDataSteps1$Mean, main = "Mean Daily Steps", xlab = "Steps", col ="red") 
      dev.off()
      
      #Caluculate the median daily steps
      
      activityDataSteps2 <- summarize(activityDataSteps, Median = median(steps))
      activityDataSteps2        
      
      
      # Create histogram representing the daily median number of daily steps
      
      png(file = "plotdata_analysis3.png", width = 600, height = 600)
      hist(activityDataSteps2$Median, main = "Median Daily Steps", xlab = "Steps", col ="red")
      dev.off()
      
      #A time series plot (i.e. type = "l") 5-minute interval (x-axis), average number of steps taken, averaged across all days (y-axis)
     
      png(file = "plotdata_analysis4.png", width = 600, height = 600)
      plot(activityDataSteps$interval,activityDataSteps$steps, type = "l", main = "Daily Steps by Intervals", xlab = "Intervals", ylab = "Number of Steps")
      dev.off()
      
      #Calculate and report the total number of missing values in the dataset
      
      activityData1 <- read.csv("C:/Users/Owner/Documents/specdata/activity.csv")
      NAS <- sum(is.na(activityData1$steps))
      print("The number of NA's are: ")
      print(NAS)
      
  }    
```



```r
    data_analysis()
```

```
## [1] "The number of NA's are: "
## [1] 2304
```