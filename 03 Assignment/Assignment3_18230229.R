set.seed(10)# taking down the values as given in the assignment question
N=10
cs1 <- rnorm(N,72,10)
cs2 <- rnorm(N,65,7)
cs3 <- rnorm(N,80,9)
cs4 <- rnorm(N,55,7)
cs5 <- rnorm(N,61,5)
m<-cbind(cs1,cs2,cs3,cs4,cs5)#performing column bind to bind the 5 vectors with their names as column name in the matrix 
rownames(m)<-c(1:10)#providing the row names as 1 to 10 to match the output given in the question
print(m)#showing out the matrix output as given in the assignment question
# First Problem
rank<-function(x,...){ #creating a function rank to rank the student marks in each subject in descending order
  a<-names(sort.int(x,decreasing = TRUE)) #sorting the x integers or marks of student in a subject in descending order 
  x[a]<-1:10 #assigning the ranks to the marks such that 1 is for the highest and 10 is for the lowest in each subject
  x #implicating it to the corresponding marks for a student in a subject and replacing it in each cell in the matrix m
}
ans<-apply(m, 2,rank)#column by column process of matrix m is done with the function rank
rownames(ans)<-rownames(c(1:10),do.NULL=FALSE,prefix = "Student#")#providing the row name to match with the given output
print(ans)#getting the desired output for the first problem
#Second Problem
#method 1
ans1<-cbind(ans,median=apply(ans,1,median))#using median function directly for row by row process of the matrix ans and storing its results as a new column
print(ans1)# getting the desired output for the second problem
#method 2
med<-function(x,...){ # creating a function med to generate the median rank for each student across all subject
  a<-sort(x) # sorting of the ranks obtained by the student
  med<-a[(length(a)+1)/2]#applying the median forumla to obtain the corresponding result
}
ans2<-cbind(ans,median=apply(ans,1,med))#row by row process of the matrix ans with the med function and value obtained are stored in the median column
print(ans2)#getting the desired output for the second problem