# 1. Include the following libraries
#Loading the required libraries as given in the assignment
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(lubridate)

# 2. Load in the energy dataset, contained in the CT5102 github account
#Loading the required dataset as given in the assignment
ener <- read_excel("IrelandData January 2017.xlsx")
print(ener)# to get the required output as in assignment

# 3. Add new features to the energy data set, based on original values
#using dplyr and tidyr functions including Date(string),time(string)
#hour of day,minute of day,dayofweek and niflow.
#NIFlow should be exporting if the netimport is -ve and otherwise it should be"importing"

ener <- ener %>%
  mutate(Date = as.character(date(DateTime)), Time = strftime(DateTime, format="%H:%M:%S"),  
         HourOfDay = hour(DateTime), DayOfWeek = wday(DateTime, label = TRUE),
         MinuteOfHour = minute(DateTime),NIFlow = case_when(NetImports < 0 ~ "Exporting", NetImports >=0 ~"Importing"))
# initialising pipe to use mututate function to create Date as a character type column to match the output in the assignment
# Time is created using strftime() in the hour minutes second format
#hour is created by using the hour() function and Dayofweek using wday() and MinuteofHour using minute() functions respectively
#NIFlow is created using case_when() function in which the value is exporting if netimports is less than 0 and otherwise it is importing
print(select(ener,DateTime, Date, Time, HourOfDay, MinuteOfHour, DayOfWeek, NIFlow, Demand,everything()))#desired output

# 4.Plot the net import data over time, colour by NIFlow, and facet by day of the week.

ggplot(data=ener)+ggtitle("Time v Net imports by Day of week") + #giving in the data and the heading
  geom_point(aes(x = HourOfDay, y=NetImports, colour = NIFlow)) + # giving the x co ordinates as hour of day and y as net imports with points of colour differentiated using NIFlow
  facet_wrap(~DayOfWeek) + # wrapping based on number of days in a week
  ylab("Net Imports") +  xlab("Time (Hour of Day)") #giving the x axis and y axis names to match as given in the output in the assignment

# 5. Plot	the	wind	generation	vs	CO2 Emissions

ggplot(data=ener)+  ggtitle("Wind Generation v CO2 Emissions") + #giving in the data and the heading
  geom_point(aes(x = Wind, y=CO2, colour = NIFlow)) + # giving x co ordinates as wind and and y co ordinatez as co2 with points differentiated in colour based on NIflow
  ylab("CO2 Emissions") +  xlab("Wind Generation") #giving the x axis and y axis names to match as given in the output in the assignment

# 6. Load in the weather data set

weather <- read_excel("Mac Head Wind Data.xlsx")#Loading the required dataset
print(weather)#getting the desired result as given in the assignment

# 7. Convert the Date (dttm) to (date) format

weather$Date<-as.Date(weather$Date)#converting it to date type
print(weather)#getting the required output

# 8. Plot the average wind speed

ggplot(data=weather, aes(x = Date, y=AVRWind))+  geom_point(colour = "blue") + # providing the data as weather and giving x co ordinates as Date and y as AvrWind with point colours as blue
  geom_line(linetype = "dashed")+ # to get the required dash lines giving line type as dashed in geom line
  ylab("Average Wind Speed (Knots)") +  xlab("Date")#giving the x axis and y axis names to match as given in the output in the assignment

# 9. Generate average daily wind generation from energy data set and ensure that Date Column is of <date> type

ener$Date <- as.Date(ener$Date)#changing Date from character to date type
is.Date(ener$Date)#ensuring to check Date is of the date type
avr_daily_wind <- summarise(group_by(ener,Date),AvrWindGeneration = mean(Wind))#creating avr_daily wind by grouping ener based on date and having mean of wind
print(avr_daily_wind)#getting the required output

# 10. Join the new datasets and produce the following plots

# Plot 1

ggplot(data=left_join(weather,avr_daily_wind), aes(x = AVRWind, y=AvrWindGeneration))+
  #taking the data which is the result of left join of weather and avr_daily_wind and x co ordinates as average winds and y cordinates as average wind generation
  ggtitle("Wind Speed v Wind Power Generated") +  geom_point() + #providing the required title 
  ylab("Average Wind Generation") +  xlab("Average Wind Speed (Mace Head)")#providing the required x axis and y axis

# Plot 2

ggplot(data=left_join(weather,avr_daily_wind), aes(x = AVRWind, y=AvrWindGeneration))+
  #taking the data which is the result of left join of weather and avr_daily_wind and x co ordinates as average winds and y cordinates as average wind generation
  ggtitle("Wind Speed v Wind Power Generated, with linear model") + #providing the required title 
  geom_point()+  geom_smooth(method = "lm") + #giving method as lm to generate the required linear model
  ylab("Average Wind Generation") +  xlab("Average Wind Speed (Mace Head)")#providing the required x axis and y axis

# Plot 3

ggplot(data=left_join(weather,avr_daily_wind), aes(x = AVRWind, y=AvrWindGeneration))+
  #taking the data which is the result of left join of weather and avr_daily_wind and x co ordinates as average winds and y cordinates as average wind generation
  ggtitle("Wind Speed v Wind Power Generated, with loess model") +#providing the required title
  geom_point()+  geom_smooth(method = "loess") + #giving method as loess to get the loess model
  ylab("Average Wind Generation") +  xlab("Average Wind Speed (Mace Head)")#providing the required x axis and y axis