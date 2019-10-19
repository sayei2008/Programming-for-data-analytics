# 1 including the libraries as given in the question
library(nycflights13)
library(dplyr)
library(ggplot2)
library(lubridate)
#2 Create	a	local	copy	of	the	flights	tibble
my_flights <- flights
print(my_flights)
#3.1	 Filter	 out	 missing	 values	 for	 dep_delay	 and	 arr_delay,	 and	 
# select	 the	 following	columns	from	the	data	set:	time_hour,	origin,	dest,	carrier,	dep_delay,	arr_delay,	air_time,	distance
my_flights <- my_flights %>% #using is.na to remove NA values from dep_delay and arr_delay
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  select(	time_hour,	origin,	dest,	carrier,	dep_delay,	arr_delay,	air_time,	
          distance)#selecting the required number of columns as given
print(my_flights)
#3.2 SECOND PAGE Add	 columns	 for	 the	 day	 of	 the	 week	 and	 for	 the	 hour	 of	 the	 day	 
# (use wday(time_hour) and	hour(time_hour) from	lubridate.
#creating DayofWeek and HourofDay and Month using mutate
my_flights <- mutate(my_flights, DayOfWeek = wday(time_hour, label = TRUE), HourOfDay = hour(time_hour), Month = month(time_hour,label = T,abbr = T))
print(select(my_flights,time_hour,DayOfWeek,HourOfDay, Month,everything()))

#4 Average	departure	delay	statistics	by	hour	of	day,	ordered	by	delay.


delay_hourly<-group_by(my_flights,HourOfDay)%>%#creating delay hourly and ordering it based on HourOfDay
  summarise(AvrDelay=mean(dep_delay),SD=sd(dep_delay),MinDelay=min(dep_delay),MaxDelay=max(dep_delay),
            MaxDelayHours=max(MaxDelay/60))%>%arrange(desc(AvrDelay))
#summarising average delay taking mean of dep_delay, standard deviation by sd of dep_delay, Minimum delay using min()
#maximum delay using max()and max delay hourly by dividing max delay by 60 and arranging delay_hourly by descending order of AvrDelay
print(delay_hourly)#required output

#5 Average	departure	delay	statistics	by	month,	ordered	by	delay

delay_monthly<-group_by(my_flights,Month)%>%#grouping based on month
  summarise(AvrDelay=mean(dep_delay),SD=sd(dep_delay),MinDelay=min(dep_delay),MaxDelay=max(dep_delay),
            MaxDelayHours=max(MaxDelay/60))%>%arrange(desc(AvrDelay))
#calculating average min max and max hourly delays and standard deviation with respect to the made order
#arraging the delay_monthly based on descending order of Average Delay
print(delay_monthly)#requried output

#6 Average	departure	delay	statistics	by carrier,	ordered	by	delay

delay_carrier<-group_by(my_flights,carrier)%>% #grouping based on carrier
  summarise(AvrDelay=mean(dep_delay),SD=sd(dep_delay),MinDelay=min(dep_delay),MaxDelay=max(dep_delay),
            MaxDelayHours=max(MaxDelay/60),NObs=n())%>%arrange(desc(AvrDelay))
#calculating average min max and max hourly delays and standard deviation with respect to the made order
#Nobs is calculated as the total n number of it present
#arranging the delay_carrier based on descending order of Average Delay
print(delay_carrier)#required output

#7 Average	departure	delay	statistics	by	airport	by	month,	ordered	by	delay.	

delay_airport_month<-group_by(my_flights,origin,Month)%>%#grouping based on origin and month
  summarise(AvrDelay=mean(dep_delay),SD=sd(dep_delay),MinDelay=min(dep_delay),MaxDelay=max(dep_delay),
            MaxDelayHours=max(MaxDelay/60),NObs=n())%>%arrange(Month)
#calculating average min max and max hourly delays , standard deviation and Nobs with respect to the made order
#arranging based on month
print(delay_airport_month)#required output

#8 Average	departure	delay	statistics	by	airport	by	hour,	ordered	by	hour.	

delay_airport_time<-group_by(my_flights,HourOfDay,origin)%>% #grouping based on Hour and Origin
  summarise(AvrDelay=mean(dep_delay),SD=sd(dep_delay),MinDelay=min(dep_delay),MaxDelay=max(dep_delay),
            MaxDelayHours=max(MaxDelay/60),NObs=n())%>%arrange(HourOfDay)
#calculating average min max and max hourly delays , standard deviation and Nobs with respect to the made order
#arranging based on hour
print(delay_airport_time)#required output

#9 Add	a	new	category,	which	divides	each	day	into	three	sections	(use	case_when)	

my_flights<-mutate(my_flights,DaySection=case_when( #creating DaySection using case_when
 HourOfDay >=5 & HourOfDay<12 ~ "Morning",#when hour is equal to or more than 5 and less than 12
 HourOfDay >=12 & HourOfDay<18 ~"Afternoon",#when hour is equal to or more than 12 and less than 18
 HourOfDay >=18 ~"Evening"#when hour is greater than 18
))
print(select(my_flights,DaySection,everything()))#required output

#10	a	sample	dataset	(using	sample_n()),	removing	all	departure	delay	values	greater	that	180	minutes.

set.seed(99)#creating the seed
myf_sample<-sample_n(my_flights,10000)%>%filter(!dep_delay>180)
#creating a sample of 10000 from myflights such that it doesnt have departure delay greater than 180
print(myf_sample)# required output

#11 	boxplot	to	visualise	the	departure	delay	by	the	three	different	time	sections.	

ggplot(data=myf_sample,mapping=aes(x=Month,y=dep_delay,colour=DaySection))+
  # taking sample data with month as x axis departure delay as y axis and DaySection to differentiate based on 3 sections 
 geom_boxplot()+ylab("Departure Delay")# providing y axis label to match the output given in assignment

