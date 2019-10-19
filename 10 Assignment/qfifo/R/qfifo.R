# Creating qfifo() function
#' The Constructor for qfifo
#' @return An S3 object of class qfifo
#' @usage qfifo()
#' @examples 
#' q <- qfifo()
#' @export

qfifo <- function(){
  structure(list(data = list()), class ="qfifo")#creating qfifo as a function with data as list and class as qfifo
}

#creating add()
#' Add a value to the queue
#' @return The updated queue object
#' @usage add(q,val)
#' @param
#' q is the current fifo queue object
#' @param
#' val is the value to be added to the queue.
#' @examples 
#' q <- qfifo()
#' q <- add(q,1234)
#' @export

add <- function(q,v){
  UseMethod("add")
}
#creating add.qfifo()
#' @export
add.qfifo <- function(q,v){
  q$data[length(q$data)+1] <- v#giving contdition to add data
  q
}
#creating top()
#'Return the top value in the queue
#' @return The top of the queue
#' @usage top(q)
#' @param
#' q is the current fifo queue object
#' @examples 
#' q <- qfifo()
#' q <- add(q,1234)
#' v <- top(q)

#' @export
top <- function(q){
  UseMethod("top")
}
#creating top.qfifo()
#' @export
top.qfifo <- function(q){
  data <- q$data[1]#to show the top data present in the given data
  return(data)
}
#creating process()
#' Delete the top element from the queue
#' @return The modified queue
#' @usage process(q)
#' @param
#' q is the current fifo queue object
#' @examples 
#' q <- qfifo()
#' q <- add(q,1234)
#' q <- add(q,5678)
#' q <- process(q)

#' @export
process <- function(q){
  UseMethod("process")
}
#creating process.qfifo()
#' @export
process.qfifo <- function(q){
  if(length(q$data) == 0)
    stop("No elements on the queue to pop")#if no elements are present in the data
  q$data[1] <- NULL#to remove the first element present in the data
  q
}