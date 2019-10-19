my_table <- function(x,na.rm=FALSE){ #initialising the function with ensuring na is not there
 if (na.rm==FALSE) { # considering na is not present in the given vector
      if (is.character(x) == FALSE) # checking if any element is not a character and throwing out error message
     stop("Error, input must be a character vector")
  else if(any(is.na(x))) # checking if any element is NA and throwing out error message
    stop("Error, the input vector has NA element(s)")
  else if(is.character(x))
  {
    x<-c(sum(x=="F"),sum(x=="M")) # creating a vector with sum of Female and Male as its elements
    names(x)<-c("F","M") # providing the index names to match as given in the question
    print(x)# desired output
  }
 }
  else if (na.rm==TRUE) # when na is to be considered while executing
  {
    x<-x[(!is.na(x))] #removing na from the vector
    x<-c(sum(x=="F"),sum(x=="M"))# performing the same step as in the case for previous if
    names(x)<-c("F","M")
    print (x) #desired output
  }
}
set.seed(111)
v<-1:10 #input for test case 1 containing only numbers
my_table(v)
v<-sample(c('M','F'),10,prob =c(.3,.7),replace = T) #input for test case 2 containing na
v<-c(v,NA)
my_table(v)
set.seed(111)
v<-sample(c('M','F'),10,prob =c(.3,.7),replace = T) # input for test case 3 containing na and allowing it
v<-c(v,NA)
my_table(v,na.rm = T)
v<-sample(c('M','F'),10,prob =c(.3,.7),replace = T) #input for test case 4 to give the ouput
my_table(v)