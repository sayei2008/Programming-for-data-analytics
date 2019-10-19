myvec <- function(v){#entering the myvec function as given in the assignment
  if(!is.numeric(v))
    stop("Error, data type must be numeric.")
  structure(list(data=v),class="myvec")
}

#extending the basic operators to the myvec and adding these methods to this class myvec
'[.myvec' <- function(a, index){ # Inorder to check the data present in the corresponding index term
  a$data[index]
}


'[<-.myvec' <- function(a, index, value){# Inorder to provide a input value to the correspoding index in the data
  a$data[index] <- value#assigning the value
  a
}


'<.myvec' <- function(a, given_value){#inorder to check which values are lesser than the conditional value given
  a$data < given_value
}


'<=.myvec' <- function(a, given_value){#inorder to check which values are lesser than and equal to the conditional value given
  a$data <= given_value
}


'>.myvec' <- function(a, given_value){#inorder to check which values are greater than the conditional value given
  a$data > given_value
}


'>=.myvec' <- function(a, given_value){#inorder to check which values are greater than and equal to the conditional value given
  a$data >= given_value
}


'!=.myvec' <- function(a, given_value){#inorder to check which values are not equal to the conditional value given
  a$data != given_value
}


'==.myvec' <- function(a, given_value){#inorder to check which values are equal to the conditional value given
  a$data == given_value
}

'print.myvec' <- function(a){#overriding the print function inorder to print the given class of the S3 and number of elements present and the data
  cat("s3 class = ", class(a),"\nNumber of elements = ",length(a$data),"\n",a$data)
}

'sum.myvec' <- function(a, ...){#getting the sum of the values provided and ... inorder to perform additional tasks like na.rm if needed
  sum(a$data,...)
}

'mean.myvec'<- function(a, ...){#getting the mean of the values provided and ... inorder to perform additional tasks like na.rm if needed
  mean(a$data,...)
}
#given conditions in the assignment to test the functions
x<-myvec(1:10)
x
x[1:2]
x[1:2] <- 0
x[x<1]
x[x>1]
x[x!=0]
x[1]<-NA
sum(x)
sum(x,na.rm = T)
mean(x)
mean(x,na.rm=T)