#109612054
#2021.10.25_HW

#import data
library(rio)
getwd()
setwd("C:/R/Engineering_Statistics/10.25")
xlsx <- import("weatherdata.xlsx")
View(xlsx)
str(xlsx)
head(xlsx)
names(xlsx)

#convert character to date format
install.packages("lubridate")
library(lubridate)
date <- xlsx$Date
dt <- mdy_hms(date)
str(dt)

#two plots on a page
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
Sys.setlocale("LC_ALL", "English")

data1 <- data.frame(time = dt, RH = xlsx$RH)
str(data1)
data2 <- data.frame(time = dt, Rain = xlsx$Rain)
str(data2)

p1 <- ggplot(data = data1, aes(time, RH))+
  geom_point(size = 0.3, color = "black", shape = 16)+
  geom_smooth()+
  labs(title = "Relative Humidity June-Oct 2020"
       , x = "Date-Time"
       , y = "RH [%]")

p2 <- ggplot(data = data2, aes(time, Rain))+
  geom_bar(stat = "identity", color = "blue")+
  labs(title = "Precipitation June-Oct 2020"
       , x = "Date-Time"
       , y = "PRecipitaiton [mm]")

grid.arrange(p1,p2,nrow = 2)

