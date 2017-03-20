library(lubridate)
#install.packages("lubridate")
library(stringr)
#install.packages("stringr")
library(dplyr)
#install.packages("dplyr")
library(tidyr)
#install.packages("tidyr")
library(ggplot2)


#Load Data
RawData <- as.data.frame(read.csv("C:/users/Shwetha Hara/Downloads/Crimes_2012-2015.csv", stringsAsFactors = FALSE))

#Rename Columns
names(RawData) <- c("DateReported", "ReportNumber", "DateOfOccurence", "TimeOfOccurence", "AreaCode", "AreaName", "Road", "CrimeCode", "CrimeDescription", "CaseStatusCode", "CaseStatus", "Location", "CrossStreet", "LatLong")

#Changing variables to appropriate data types
RawData$ReportNumber <- as.character(RawData$ReportNumber)
RawData$AreaCode <- as.factor(RawData$AreaCode)
RawData$AreaName <- as.factor(RawData$AreaName)
RawData$Road <- as.character(RawData$Road)
RawData$CrimeCode <- as.character(RawData$CrimeCode)
RawData$CaseStatusCode <- as.factor(RawData$CaseStatusCode)
RawData$CaseStatus <- as.factor(RawData$CaseStatus)

View(RawData)
#Formatting the column TimeOfOccurence
RawData$TimeOfOccurence <- sprintf("%04d",as.numeric(RawData$TimeOfOccurence))
RawData$TimeOfOccurence <- format(strptime(RawData$TimeOfOccurence, format = "%H%M", tz = "America/Los_Angeles"), "%H:%M")


#Creating separate Latitude and Longitude columns
RawData$LatLong <- str_extract(RawData$LatLong, "[-]*\\d+.\\d+[,]\\s[-]*\\d+.\\d+") 
RawData <- RawData %>% separate(LatLong, c("Latitude", "Longitude"), sep = "[,]\\s", extra = "drop", remove = TRUE)

#Separate Time
RawData <- RawData %>% separate(TimeOfOccurence, c("Hour", "Min"), sep = ":", extra = "drop", remove = FALSE)
RawData <- RawData %>% distinct
View(RawData)

#date
RawData$DateOfOccurence <- mdy(RawData$DateOfOccurence)
#Creating a month column
RawData$MonthOfOccurence <- month(RawData$DateOfOccurence, label = TRUE)
#Creating new column for the day of week
RawData$DayOfWeek <- wday(RawData$DateOfOccurence, label = TRUE)

RawData$crime_category <- ifelse(grepl("THEFT*|PICKPOCKET|STOLEN*", RawData$CrimeDescription), "THEFT", 
                                ifelse(grepl("BURGLARY*", RawData$CrimeDescription), "BURGLARY",
                                       ifelse(grepl("HOMICIDE*", RawData$CrimeDescription), "HOMICIDE",
                                              ifelse(grepl("ROBBERY*",RawData$CrimeDescription), "ROBBERY",
                                                     ifelse(grepl("ASSAULT*",RawData$CrimeDescription), "ASSAULT",
                                                            ifelse(grepl("VANDALISM*", RawData$CrimeDescription), "VANDALISM",
                                                                   ifelse(grepl("TRAFFIC*", RawData$CrimeDescription), "TRAFFIC_RELATED",
                                                                          ifelse(grepl("SEXUAL*", RawData$CrimeDescription),"SEXUAL","OTHER"))))))))

#shifts
timeoftheday <- function(hour) {
  if (hour >= 0  && hour < 7) { return (as.factor("12AM - 7AM"))}
  else if (hour >= 7 && hour < 11) { return (as.factor("7AM-12PM"))}
  else if (hour >= 12 && hour < 19) { return (as.factor("12PM-7PM"))}
  else return (as.factor("7PM-12AM"))
}

RawData$TimeOftheDay <- sapply(RawData$Hour,timeoftheday)
View(RawData)
#Hourly Distribution
ggplot(RawData, aes(x = Hour)) +
  geom_histogram(binwidth = 1, aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "green", high = "red")
  

#Weekly Distribution
ggplot(RawData, aes(x = DayOfWeek)) +
  geom_bar(aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "green", high = "red") 

#Hourly Dist through each day of the week
ggplot(RawData, aes(x = Hour)) +
  geom_histogram(binwidth = 1, aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "green", high = "red") + 
  facet_wrap(~DayOfWeek, nrow = 7)
  

RawData$state <- "CA"
