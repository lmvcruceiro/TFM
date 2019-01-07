setwd("C:/TFM")
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)


stationsData <- fread("Data/3_months_data.csv")
dates <- c()
for (i in 1:length(stationsData$fechahora)){
  dates <- c(dates, unlist(str_split(string = stationsData$fechahora[i], pattern = "T"))[1])
}

datesWithFormat <- as_date(dates)
weekDaysData <- weekdays(datesWithFormat)
monthsData <- month(datesWithFormat)

stationsData$monthsData <- monthsData
stationsData$weekDay <- weekDaysData
stationsData$date <- datesWithFormat

stations <- stationsData %>% filter(activate == 1, no_available == 0) %>% 
  select(dock_bikes, free_bases, id, name, no_available, total_bases, fechahora, weekDay,date, reservations_count, monthsData)

r <- stations %>% group_by(monthsData, weekDay) %>% summarise(Rate =  mean(free_bases/total_bases)) %>% arrange(desc(monthsData))
stations %>% group_by(monthsData) %>% summarise(Rate =  sum(reservations_count)) %>% arrange(desc(Rate))
