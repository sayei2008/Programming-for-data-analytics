library(lubridate)
timer<-function(){#creating a closure function named timer()
  initial_time<-now()# getting the initial time
  start_time<-NA #assigning start time as NA
  finish_time<-NA#assigning finish time as NA
list(#creating a list of 4 functions in the timer function
  start=function()start_time<<-now(),#the start function starts the timer and notes down the started time
  stop = function(){#the stop function stops the timer and notes down the ended time
    if (!is.na(start_time))
      finish_time<<-now()#stopped time is noted
    else#error handling if start_time = NA
      stop("Error, Cannot stop as timer was not started...") 
  },#obtaining the state information of all the times in the closure function using get_state()
  get_state=function()list(Init=initial_time,Start=start_time,Finish= finish_time),
  get_time=function(){# get time function is used to obtain the time elapsed
    if(!is.na(finish_time))
      difftime(finish_time,start_time,units="secs")
    else#error handling if finish_time = NA
      stop("Error, Cannot get time as stop was not called...")
  }
)
}
#example 1 create the timer closure
t<-timer()
str(t$get_state())
#example 2 check for error 1
t$stop()
#example 3 start the timer
t$start()
str(t$get_state())
#example 4 check for error 2
t$get_time()
#example 5call the stop function and calculate the overall duration
t$stop()
t$get_time()
#if needed,run each examples one after another giving some gap so that there occurs considerable time lapse between start and stop of the timer function