set.seed(1000)#reproducing the initial code given in the question
ids <- rep(as.character(1001:1005,2))
module <- c(rep("CT101",5),rep("CT102",5))
result <- c(rnorm(n = 5,mean = 70,sd = 5),
            rnorm(n = 5,mean = 50,sd = 8))
result<-c(NA,result[2:length(result)])
dataf<- data.frame(ids,module,result)#making a dataframe as per given in the question
print(dataf)#providing the data frame output like the sample one given
my_aggregate <- function(df, group_id, data_id, f, ...)#initialising the my_aggregate function with objects given
{
  if (!is.data.frame(df))#to check if df is a dataframe or not 
    stop("First parameter is not a data frame object")
  if(!is.numeric(df[[data_id]]))#to check if data_id is numeric or not
    stop("Error ",data_id," is not a numeric column")
  if(!is.function(f))#to check if f is a function or not
    stop("Error ",f," is not a function")
  if(group_id!=unique(colnames(df[1]))&&group_id!=unique(colnames(df[2])))
#to check if group_id name is given properly by cross checking it with the names of the column names of given dataframe
    stop("Error ",group_id," is not a valid column")
  Aggregate <- tapply(df[,data_id], df[,group_id], mean, ...)
  #getting aggregate by applying mean to the data using data_id which is grouped based on group_id
  print(Aggregate)#desired output of the aggregate
}#providing all the test cases as given in the question and churning out its corresponding answers
my_aggregate(dataf,"module","result",mean)
my_aggregate(dataf,"module","result",mean,na.rm=T)
my_aggregate(dataf,"ids","result",mean)
my_aggregate(dataf,"ids","result",mean,na.rm=T)
my_aggregate(1:10,"module","result",mean)
my_aggregate(dataf,"modul","result",mean)
my_aggregate(dataf,"module","ids",mean)
my_aggregate(dataf,"module","result",10)