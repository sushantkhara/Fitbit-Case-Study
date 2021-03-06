# Fitbit-Case-Study
This Repo is about the case study on Fitbit fitness tracker
#============================
# Setting up my R environment
#============================

```{r Installed Packages}
install.packages("tidyverse") 
install.packages("ggplot2")
install.packages("skimr")
install.packages("janitor")
```
## Loading packages

```{r Loading required R packages}
library(tidyverse)  # For data analysis
library(ggplot2)    # For data visualization
library(skimr)      # Generates quick summary of data
library(janitor)    # data cleaning makes easier
library(lubridate) #  helps wrangle date attributes
```

#===============================
# Step 1: PREPARE - Collect Data 
# ==============================
## Download and import required datasets into project folder from data source - https://www.kaggle.com/arashnic/fitbit/
```{r Download Dta sets}
# Set working directory and import datasets
setwd("C:/Users/k96su/Documents/R/Case Study/Fitbit Case Study/CSV Files")
### Confirm the working directory
getwd()
```

## load data sets
```{r Load data sets}
daily_activity <- read.csv("dailyActivity_merged.csv")
daily_calories <- read.csv("dailyCalories_merged.csv")
daily_steps <- read.csv("dailySteps_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
heartrate_secs <- read.csv("heartrate_seconds_merged.csv")
weight_info <- read.csv("weightLogInfo_merged.csv")
```

#=======================================================
# Step 2 : PROCESS - Wrangle and Clean Data for Analysis
#=======================================================
```{r  Get quick summary of datasets}
skim_without_charts(daily_activity)
skim_without_charts(daily_calories)
skim_without_charts(daily_steps)
skim_without_charts(sleep_day)
skim_without_charts(heartrate_secs)
skim_without_charts(weight_info)
```

```{r Explore structure of the datasets, datatypes}
str(daily_activity) 
str(daily_calories)
str(daily_steps)
str(sleep_day)
str(heartrate_secs)
str(weight_info)
```

## Noticed that Id is common column name for all data sets, this can be used to merge datasets
## Datatype of ActivityDate, ActivityDay, SleepDay, Time, Date are Char type which is wrong type changing into proper data types
```{r correct Date and time formats}
# change ActivityDate variable to date format from char format
daily_activity$ActivityDate<- as.Date(daily_activity$ActivityDate, "%m/%d/%Y")

# create new variable 'Activity_dayofweek' to show Day of week
daily_activity$Activity_dayofweek<-format(as.Date(daily_activity$ActivityDate),"%A" )
# Preview dataset
View(daily_activity)

# change ActivityDay variable to date format from char format
daily_calories$ActivityDay<- as.Date(daily_calories$ActivityDay, "%m/%d/%Y")

# create new variable 'calorie_dayofweek' to show Day of week
daily_calories$Calorie_dayofweek<-format(as.Date(daily_calories$ActivityDay),"%A" )
# Preview First 6 rows of data
head(daily_calories)

# change ActivityDay variable to date format from char format
daily_steps$ActivityDay<- as.Date(daily_steps$ActivityDay, "%m/%d/%Y")

# create new variable 'Steps_dayofweek' to show Day of week
daily_steps$Steps_dayofweek<-format(as.Date(daily_steps$ActivityDay),"%A" )
# Preview daily_steps 
view(daily_steps) 

# change SleepDay variable to date format from char format
sleep_day$SleepDay<- as.Date(sleep_day$SleepDay, "%m/%d/%Y")

# create new variable 'Sleep_dayofweek' to show Day of week
sleep_day$Sleep_dayofweek<-format(as.Date(sleep_day$SleepDay),"%A" )
# Preview dataset
View(sleep_day)

# change Time variable to date format from char format
heartrate_secs$Time<- as.Date(heartrate_secs$Time, "%m/%d/%Y")

# create new variable 'Dayofweek' to show Day of week
heartrate_secs$Dayofweek<-format(as.Date(heartrate_secs$Time),"%A" )
# Preview dataset
View(heartrate_secs)

# change Date variable to date format from char format
weight_info$Date <- as.Date(weight_info$Date, "%m/%d/%Y")
# Preview dataset
View(weight_info)
```
```{r Validate changes in column names }

colnames(daily_activity)
colnames(daily_calories)
colnames(daily_steps)
colnames(sleep_day)
colnames(heartrate_secs)
colnames(weight_info)
# Column names looks good now
```

## Find out Unique participants in daily_activity and sleep_day, daily_calories, sleep_day, heartrate_secs and weight_info
```{r Check unique Participants}
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_steps$Id)
n_distinct(heartrate_secs$Id)
n_distinct(weight_info$Id)

# Merging sleep_day with daily_activity by Id
combined_data <- merge(sleep_day, daily_activity, by="Id")
```
#==============================
# Step 3 - DESCRIPTIVE ANALYSIS
#==============================
```{r Exploring statistical summary of dat sets}
# Exploring daily_activity for quick statistical summary
daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()
# Summary statistics of sleep_day
sleep_day %>%
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

# Exploring daily_calories, daily_steps for quick statistical summary
daily_calories %>%
  select(Calories) %>% summary()

daily_steps %>%
  select(StepTotal) %>% summary()

# Exploring heartrate_secs for statistical summary
heartrate_secs %>%
  select(Value) %>% summary()

# Exploring weight_info for statistical summary
weight_info %>%
  select(WeightKg, WeightPounds) %>% summary()
```
#=========================
# Step 4: VISUALIZING DATA
#=========================
```{r Relationship between LightlyActiveMinutes and SedentaryMinutes}
ggplot(data = daily_activity, mapping = aes(x = LightlyActiveMinutes, y = SedentaryMinutes, color = Activity_dayofweek))+
  geom_smooth(se=FALSE, method = 'loess', formula = y~x) + 
  labs(title = "LightlyActiveMinutes Vs SedentaryMinutes", subtitle = "Everyday")
#Participants who spend most time in light activities they spend less time in sitting everyday of the week.
```
![Lightactivity vs Sedementary](https://user-images.githubusercontent.com/74862660/117615220-0951f700-b187-11eb-8b80-bcb9c343e5cd.png)

```{r Relationship between TotalSteps and Calories View(combined_data)}
ggplot(data = combined_data, mapping = aes(x = TotalSteps, y = Calories, color = Activity_dayofweek))+ 
  geom_smooth(se=FALSE, method = 'loess', formula = y~x) + 
  labs(title = "Calories vs TotalSteps", subtitle = "Everyday")
# Participants who takes more calories they tend to walk more steps everyday of the week
```
![Caorie vs totalsteps](https://user-images.githubusercontent.com/74862660/117615225-0bb45100-b187-11eb-9e52-fbfa18e268da.png)

```{r Relationship between minutes asleep and time in bed}
ggplot(data=sleep_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed, color = Sleep_dayofweek)) + 
   geom_smooth( se= FALSE, method = 'loess', formula = y~x) +
  labs(title = "TotalTimeinBed Vs TotalMinutesAsleep", subtitle = "Everyday")
# This indicates that the participants who spent more time on Bed they also sleep more
```
![TotalSteps vs TotalSleep](https://user-images.githubusercontent.com/74862660/117615242-11119b80-b187-11eb-8b32-9c336c54a36a.png)

```{r Relationship between Totalsteps and TotalMinutesAsleep per day}
combined_data%>%
  ggplot(aes(x=TotalSteps, y=TotalMinutesAsleep, color = Sleep_dayofweek)) + 
  geom_smooth( se= FALSE, method = 'loess', formula = y~x) +
  labs(title = "TotalSteps Vs TotalMinutesAsleep", subtitle = "Daily") + facet_wrap(~Sleep_dayofweek)
# The graph shows that the participants who sleep more on Sunday, Tuesday, Thursday and Friday,
# TotalSteps gradually increases. But on Monday and Saturday the TotalSleep time is less,and Totalsteps walked by the participants is more
```
![Totaltimeinbed vs Total sleep](https://user-images.githubusercontent.com/74862660/117615248-140c8c00-b187-11eb-98a6-341c85c68383.png)

```{r Discovering Sleeping patterns of Participants}
# Get summary of 'sleep_day' data and place in variable 'summary_sleep'.
#Summarize 'mean_sleep', 'TotalMinutesAsleepPerNight', 'mean_total_time_in_bed',
#'sleep_date', and 'Sleep_dayofweek'

summary_sleep<-sleep_day %>% #create new variable 'summary_sleep'
  group_by(Id)%>%
  summarise( 
    mean_sleep=mean(TotalMinutesAsleep), #mean of TotalMinutesAsleep based on Id
    TotalMinutesAsleepPerNight = sum(TotalMinutesAsleep),#sum of TotalMinutesAsleep based on Id
    mean_total_time_in_bed = mean(TotalTimeInBed), #average time in bed
    Sleep_date=as.Date(SleepDay), #add sleep date in date format
    Sleep_dayofweek=format(as.Date(SleepDay), "%a")) #add sleep day of week as char.

# Use ifelse statement to label quality of sleep based on value associated with 'mean_sleep'
summary_sleep$overall_mean_level_of_sleep = 
  ifelse(summary_sleep$mean_sleep >= 460, "Too much sleep (OVER 460)",
         ifelse(summary_sleep$mean_sleep >= 360, "Good sleep (360-460)",
                ifelse(summary_sleep$mean_sleep >= 200, "Bad sleep (200-360)",
                       ifelse(summary_sleep$mean_sleep <200, "Not eough sleep (0-200)", ""))))

summary_sleep %>% #summarizing observations per day of week
  group_by(Sleep_dayofweek) %>% 
  summarise(obs = n()) # Number of observations ranged from 47-66 each day

summary_sleep %>%  
  ggplot(aes(x = Sleep_dayofweek, fill = Sleep_dayofweek )) +
  geom_bar() +
  labs(title = "No. of Participants Sleep each day")
# This graph shows how many participants Sleep each day of the week
```
![Total no  of participants sleep everyday](https://user-images.githubusercontent.com/74862660/117618245-47511a00-b18b-11eb-84b8-f039a7249e0b.png)

```{r Categorizing Sleeping patterns of the participants}
summary_sleep %>%
  ggplot(aes(x=Sleep_dayofweek, fill = Sleep_dayofweek)) +
  geom_() +
  facet_wrap(~overall_mean_level_of_sleep) + labs(title = "Everyday Sleeping Habits of Participants")
# graph shows that at most 3 participants doesn't have enough sleep,
# at least 10 participants sleep more than 7.6hrs per day(mean sleep), At least 30 or more participants sleep well
# Maximum 8 participants are sleeping badly(Sleeping hours maximum 3.2hrs)
```
![Sleeping nature of participants](https://user-images.githubusercontent.com/74862660/117615254-153db900-b187-11eb-9a07-eb86acd3f521.png)

```{r Discovering relationship between steps taken in day and SedentaryMinutes}
ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes, 
                                color = Activity_dayofweek)) +
 geom_smooth(se = FALSE, method = 'loess', formula = y~x) + 
  labs(title = "SedentaryMinutes Vs TotalSteps", subtitle = "Everyday")

# Graph tells that participants who walk more everyday, spent less time in sitting
```
![Sedementary vs totalsteps](https://user-images.githubusercontent.com/74862660/117615173-fc350800-b186-11eb-8b85-636ed681b889.png)
