library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)
library(lubridate)
setwd("D:/Bike Share Case Study")
#Read all the csv files as a dataframe
t1<-read.csv("t1.csv")
t2<-read.csv("t2.csv")
t3<-read.csv("t3.csv")
t4<-read.csv("t4.csv")
t5<-read.csv("t5.csv")
t6<-read.csv("t6.csv")
t7<-read.csv("t7.csv")
t8<-read.csv("t8.csv")
t9<-read.csv("t9.csv")
t10<-read.csv("t10.csv")
t11<-read.csv("t11.csv")
t12<-read.csv("t12.csv")

#Bind all the dataframes into one single dataframe
df<-rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)
head(df)
str(df)
summary(df)

#Variables 'started_at' and 'ended_at' are in 'char' format and need to be changed to 'datetime' format
#We then get the day of week from the datetime format
df$started_at<-as_datetime(df$started_at)
df$weekday<-weekdays(df$started_at)
df$ended_at<-as_datetime(df$ended_at)

#duration in hours from the datetime formatted variables
df$ride_length<-as.numeric(difftime(df$ended_at,df$started_at,units=c("hours")))
df$month<-month(df$started_at)

#Filter the variables necessary for our analysis and durations>0.
df1<-df %>%subset(df$ride_length>0,select=-c(ride_id,start_station_name,rideable_type,ended_at,start_lat,start_lng,end_lat,end_lng,start_station_id,end_station_name,end_station_id))

#Then drop rows having one or more 'NA' so we have maximum rows for our analysis
df2<-drop_na(df1)         
summary(df2)

#Check for the distinct values for categorical variables
count(df2,df2$member_casual)  

#check if weekdays is a factor already,if not make split it into factor levels
is.factor(df2$weekday)
df2$weekday<-factor(df2$weekday,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

#Check if month is formatted correctly and change it into factor levels
table(df2$member_casual)
df2$month<-as.factor(df2$month) 
levels(df2$month)
summary(df2)
levels(df2$month)<-c("January","February","March","April","May","June","July","August","September","October","November","December")

weeks_ <-as.numeric(difftime(max(df2$started_at),min(df2$started_at),units="weeks"))


#Analysis

##Average Riders Vs Day of the Week
df2 %>% group_by(weekday,member_casual)%>% summarise(avg_riders=(n()/weeks_)) %>% 
  ggplot(.,aes(x=weekday,y=avg_riders,fill=member_casual))+geom_bar(position="dodge",stat="identity")+
  labs(title="Weekly Average Riders",x="Day of the Week",y="Average Riders",fill="User Type")+
  theme(plot.title=element_text(hjust=0.5,face="bold"))+
  theme(panel.background=element_rect(fill="darkseagreen1",colour="black"),plot.background=element_rect(fill="palegoldenrod"))


#Number of Riders Vs Month
ggplot(df2,aes(x=month,fill=member_casual))+geom_bar(position="dodge")+
  labs(title="Monthly User Trend",x="Month",y="Number of riders",fill="User Type")+
  theme(panel.background=element_rect(fill="darkseagreen1",colour="black"),plot.background=element_rect(fill="palegoldenrod"))+
  theme(plot.title=element_text(hjust=0.5,face="bold",colour="grey3"))+
  scale_y_continuous(labels=function(x) format(x,scientific=FALSE))


##Mean Ride Length vs Day of the Week
df2 %>% group_by(weekday,member_casual)%>% summarise(mean_ride_length=mean(ride_length)) %>%
  ggplot(.,aes(x=weekday,y=mean_ride_length,fill=member_casual))+geom_histogram(position="dodge",stat="identity")+
    labs(title="Weekly Average Ride Length",x="Day of the Week",y="Average Ride Length",fill="User Type")+
    theme(plot.title=element_text(hjust=0.5,face="bold"))+
    theme(panel.background=element_rect(fill="darkseagreen1",colour="black"),plot.background=element_rect(fill="palegoldenrod"))


