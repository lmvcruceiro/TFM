setwd("C:/TFM")
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

date <- as.Date(yourdata$Date_of_order, format = "%Y/%m/%d")
yourdata$WeekDay <- weekdays(date)

stationsData <- fread("Data/3_months_data.csv")
stations <- stationsData %>% filter(activate == 1, no_available == 0) %>% 
  select(dock_bikes, free_bases, id, name, no_available, total_bases, fechahora)

dates <- c()
for (i in 1:length(stations$fechahora)){
  dates <- c(dates, unlist(str_split(string = stations$fechahora[i], pattern = "T"))[1])
}

datesWithFormat <- as_date(dates)
weekDaysData <- weekdays(datesWithFormat)
stations$weekDays <- weekDaysData

stations %>% group_by(name) %>% summarise(Rate =  mean(free_bases/total_bases)) %>% arrange(name)
