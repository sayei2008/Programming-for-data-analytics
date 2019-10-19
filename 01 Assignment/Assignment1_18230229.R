#Question_1
set.seed(99)
dice1 <- sample(1:6,1000, replace = TRUE) #rolling dice 1 1000 times
dice2<- sample(1:6,1000, replace = TRUE) #dice 2 rolling 1000 times
dicetogether<- rowSums( cbind (dice1,dice2)) # adding both dice
output<-c(sum(dicetogether%%2!=0),sum(dicetogether%%2==0))#getting sum of odd and even
names(output)=c("number odd","number even")#providing index name to match the output given in the assignment qn
print(output)

#Question_2
#repeating the first 3 steps again here as it is a separate question
set.seed(99)
dice1 <- sample(1:6,1000, replace = TRUE) #rolling dice 1 1000 times
dice2<- sample(1:6,1000, replace = TRUE) #dice 2 rolling 1000 times
dicetogether<- rowSums( cbind (dice1,dice2)) # adding both dice
output2=integer()#assigning the desired output variable
#starting for loop to get the dice values of 2 to 12
for (i in 2:12) {
  output2[i-1]<-length(which(dicetogether==i))# getting the frequency of each face value in dice
}
names(output2)<-c(2:12)#allocating the face dice value as corresponding index name
print(output2)
table(dicetogether)#crosscheck as told in question

#Question_3
#repeating the first 3 steps again here as it is a separate question
set.seed(99)
dice1 <- sample(1:6,1000, replace = TRUE) #rolling dice 1 1000 times
dice2<- sample(1:6,1000, replace = TRUE) #dice 2 rolling 1000 times
dicetogether<- rowSums( cbind (dice1,dice2)) # adding both dice
output3<-dicetogether[c(rep(FALSE,99),TRUE)]
#making the rep function to refuse first 99 terms and accept the 100th term till the sample is exhausted
#assigning the index name as the 100th element and its multiples
names(output3)<-c(1:10)*100
print(output3)
#crosschecking it by taking the actual 100th element and its multiples and assigning it to a vector
crosscheck<-c(dicetogether[(1:10)*100])
print(crosscheck)
