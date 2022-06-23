########## Setup #####################
library(readr)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(funModeling) 
library(dplyr)
library(lubridate)

setwd("C:/Users/willj/OneDrive/Desktop/Internship/YouTube Channel EDA Will")

daily_channel_data <- read_csv("daily_channel_data.csv")
top_authortubers <- read_csv("top_authortubers.csv")
update_data <- read_csv("daily_channel_data_update_06272021.csv")
numbers_data <- read_csv("top_authortubers.csv")

### Update Data
update_data$data_date <- ymd(update_data$data_date)
### view count needs to be changed to a double
update_data$view_count <- as.double(update_data$view_count)
### subscriber count should be a double
update_data$subscriber_count <- as.double(update_data$subscriber_count)
### weird trick, but works, since all we need is false values
update_data$hidden_subscriber_count <- is.logical(update_data$hidden_subscriber_count)
?as.logical
### video count needs to be a double
update_data$video_count <- as.double(update_data$video_count)


update_data <- inner_join( update_data, top_authortubers, by = "channel_id")
### merge data tables ----
merged_authortuber <- inner_join( daily_channel_data, top_authortubers, by = "channel_id" )

merged_authortuber <- union( merged_authortuber, update_data)

dim(merged_authortuber)

### merge data tables
##merged_authortuber <- inner_join( daily_channel_data, top_authortubers, by = "channel_id" )
### trying to merge the two datasets shows that none of the channels match


glimpse(daily_channel_data)
summarise(daily_channel_data, avg=mean(daily_channel_data$view_count))

options(scipen=999)



########## High Level Description of Data ##############
dim(merged_authortuber)
str(merged_authortuber)
summary(merged_authortuber)

#Some of these variables will not be useful for analysis and should be dropped.
merged_authortuber = select(merged_authortuber, -hidden_subscriber_count)
merged_authortuber = select(merged_authortuber, -channel_name)
merged_authortuber = select(merged_authortuber, -validated)
merged_authortuber = select(merged_authortuber, -channel_id)



########## Basic Univariate EDA for Merged Dataset ##########
## Use only most recent observations (2021-06-27)
recent_50 = merged_authortuber %>% filter(data_date == "2021-06-27")


# View Count -  continuous 
recent_50$views_mil = recent_50$view_count/1000000
## central tendency 
summary(recent_50$views_mil)
boxplot(recent_50$views_mil, main = "View Count Distribution (Below 10 mil)", ylim = c(0, 10))
## spread
plot(density(recent_50$views_mil), xlim = c(0, 10), main = "View Count Density (Below 10 mil)")


# Subscriber Count -  continuous 
recent_50$subs_1k = recent_50$subscriber_count/1000
## central tendency 
summary(recent_50$subs_1k)
boxplot(recent_50$subs_1k, main = "Subscriber Count (Below 150 Thousand)", ylim = c(0, 150))
## spread
plot(density(recent_50$subs_1k))


# Video Count - continuous 
## central tendency 
summary(recent_50$video_count)
boxplot(recent_50$video_count, main = "Video Count")
## spread
plot(density(recent_50$video_count))




########## Univariate Bar Plots ###########
#Ranking of View Count by Channel
ggplot(data=recent_50,aes(x=reorder(authortuber,views_mil),y=views_mil)) + 
  geom_bar(stat ='identity',aes(fill=views_mil))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Views (Millions)")+
  labs(title = 'Ranking of View Count by Channel',
       y='Views (Millions)',x='Author')+ 
  geom_hline(yintercept = mean(recent_50$views_mil),size = 1, color = 'blue')

#Ranking of Subscriber Count by Channel
ggplot(data=recent_50,aes(x=reorder(authortuber,subs_1k),y=subs_1k)) + 
  geom_bar(stat ='identity',aes(fill=subs_1k))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Subscribers (Thousands)")+
  labs(title = 'Ranking of Subscriber Count by Channel',
       y='Subscribers (Thousands)',x='Author')+ 
  geom_hline(yintercept = mean(recent_50$subs_1k),size = 1, color = 'red')

#Ranking of Video Count by Channel
ggplot(data=recent_50,aes(x=reorder(authortuber,video_count),y=video_count)) + 
  geom_bar(stat ='identity',aes(fill=video_count))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Video Count")+
  labs(title = 'Ranking of Video Count by Channel',
       y='Number of Videos',x='Author')+ 
  geom_hline(yintercept = mean(recent_50$video_count),size = 1, color = 'green')
########## Ratio Data ##########

#create view per video var
recent_50$view_per_video = recent_50$view_count / recent_50$video_count
summary(recent_50$view_per_video)
boxplot(recent_50$view_per_video, ylim = c(0, 40000))
lm_view = lm(recent_50$view_per_video~video_count, data = recent_50) #Create the linear regression
ggplot(data = recent_50, aes(x=video_count, y=view_per_video))+ ylim(0, 20000) +theme_minimal() +geom_point() + geom_smooth(method=lm)+
  labs(title = 'Views per Video with Growing Count',
       y='Ratio View/Video',x='Number of Videos')

#create variable sub_per_view that gives a ratio
recent_50$sub_per_view = recent_50$subscriber_count / recent_50$view_count
summary(recent_50$sub_per_view)
boxplot(recent_50$sub_per_view, ylim = c(0,0.04))
lm_sub = lm(recent_50$sub_per_view~video_count, data = recent_50) #Create the linear regression
ggplot(data = recent_50, aes(x=video_count, y=sub_per_view)) +theme_minimal() +geom_point() + geom_smooth(method=lm)+
  labs(title = 'Sub/View with Growing Video Count',
       y='Ratio Sub/View',x='Number of Videos')


########## Regression ###########
r1 = lm( merged_authortuber$subscriber_count ~ merged_authortuber$video_count)
r1
summary(r1)

reg = lm( merged_authortuber$subscriber_count ~ merged_authortuber$view_count)
reg
summary(reg)


reg2 = lm( merged_authortuber$subscriber_count ~ merged_authortuber$view_count,merged_authortuber$video_count)
reg2
summary(reg2)