setwd("C:/Users/Dangermonger/Documents/GitHub/RepData_PeerAssessment1")

library(dplyr) 
library(ggplot2)

unzip("./activity.zip") 

actdata <- read.csv("activity.csv")

================================================================================
##What is mean total number of steps taken per day?
    
convertbl <- tbl_df(actdata) ## convert table to tbl
rm("actdata") ## remove other handle

sumtable <- convertbl %>% 
    group_by(date)%>% ##group by date then... 
    ##(SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP)
    ##summarise the grouped data in columns and name and sum steps
    summarise(steps = sum(steps))

##1. Calculate the total number of steps taken per day

print.data.frame(sumtable)

##2. Make a histogram of the total number of steps taken each day

##convert date column to date format
sumtable$date <- as.Date(sumtable$date, format = "%Y-%m-%d")

ggplot(data=sumtable, 
    aes(x = date, y = steps)) + 
    labs(title="Histogram of Steps per Day") +
    labs(x="Date", y="Steps Taken") + 
    geom_histogram(stat = "identity")

##3. Calculate and report the mean and median of the total number of steps taken
##per day

mean(sumtable$steps, na.rm = TRUE)
median(sumtable$steps, na.rm = TRUE)

================================================================================
##What is the average daily activity pattern?
    
##1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
##and the average number of steps taken, averaged across all days (y-axis)
    
actdata <- read.csv("activity.csv") ## read file
convertimetbl <- tbl_df(actdata) ## convert table to tbl
rm("actdata") ## remove other handle

timetable <- convertimetbl %>% 
    group_by(interval)%>% ##group by interval then... 
    ##(SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP)
    ##summarise the grouped data in columns and name and sum steps
    summarise(steps = mean(steps, na.rm = TRUE))

ggplot(data=timetable, 
    aes(x = interval, y = steps)) + 
    labs(title="Time Series of Average Steps per 5-minute interval") +
    labs(x ="5-minute interval", y ="Average Steps Taken") + 
    geom_line()

##2. Which 5-minute interval, on average across all the days in the dataset, 
##contains the maximum number of steps?

timetable[which(timetable$steps == max(timetable$steps)),] 

================================================================================
##Imputing missing values
    
##1. Calculate and report the total number of missing values in the dataset 
##(i.e. the total number of rows with NAs)

actdata <- read.csv("activity.csv") 
sum(is.na(actdata$steps))

##2. Devise a strategy for filling in all of the missing values in the dataset. 

##Group actdata by interval, add steps column where nas are replaced by the 
##mean of the interval group.

##3. Create a new dataset that is equal to the original dataset but with the 
##missing data filled in.

convertblnonas <- tbl_df(actdata) ## convert table to tbl
rm("actdata") ## remove other handle

nonas <- convertblnonas %>% 
    group_by(interval) %>%
    mutate(steps = ifelse(is.na(steps), +
    mean(steps, na.rm = TRUE), steps))

##4. Make a histogram of the total number of steps taken each day and calculate 
##and report the mean and median total number of steps taken per day. 

sumnonas <- nonas %>% 
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    group_by(date)%>% ##group by date then... 
    ##(SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP)
    ##summarise the grouped data in columns and name and sum steps
    summarise(steps = sum(steps)) 


sumnonas$date <- as.Date(sumnonas$date, format = "%Y-%m-%d")

ggplot(data=sumnonas, 
       aes(x = date, y = steps)) + 
       labs(title="Histogram of Steps per Day") +
       labs(x="Date", y="Steps Taken") + 
       geom_histogram(stat = "identity")

mean(sumnonas$steps)
median(sumnonas$steps)

mean(sumtable$steps, na.rm = TRUE)
median(sumtable$steps, na.rm = TRUE)

##Do these values differ from the estimates from the first part of the 
##assignment? 

##The mean and the median are now identical.

##What is the impact of imputing missing data on the estimates of the total 
##daily number of steps?

##The mean is exactly the same because mean imputation does not affect the 
##sample mean. Mean imputation brings the median closer to the mean.

================================================================================
##Are there differences in activity patterns between weekdays and weekends?
    
##1. Create a new factor variable in the dataset with two levels - "weekday" and 
##"weekend" indicating whether a given date is a weekday or weekend day.


weekend <- c("Saturday", "Sunday")

weekdaysdata <- nonas %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(date = weekdays(date)) %>%
    mutate(date = ifelse(date %in% weekend, "weekend", "weekday")) %>%
    mutate(date = as.factor(date))%>%
    mutate(steps = round(steps)) %>%
    group_by(date, interval)%>% 
    summarise(steps = mean(steps))

    
##2. Make a panel plot containing a time series plot (i.e. type = "l") of the 
##5-minute interval (x-axis) and the average number of steps taken, averaged 
##across all weekday days or weekend days (y-axis). 

ggplot(data=weekdaysdata, aes(x=interval, y=steps, group=1, colour=date )) + ##identify data, identify labels, single line connecting points, colour by date
    facet_grid(date ~ .)+ ##side by side graphs
    geom_line()+ ##line style
    theme(legend.title=element_blank(), ##remove legend title/background
          legend.background=element_blank(), ##remove legend background
          plot.background=element_blank(), ##make plot background transparent
          strip.background = element_rect(colour="black")) + ##add outline to facet label
    guides(color=FALSE) +
    xlab("5-minute interval") + ylab("Steps Taken") + ##add label titles
    ggtitle("Weekend v Weekday activity patterns\n") ##add chart title with new line at end for space

================================================================================