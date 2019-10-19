#Loading the requried packages
library(devtools)
library(roxygen2)
#setting up the initial working directory in which the qfifo should be created
setwd("C:/Users/sayei/Documents/Assignment10_18230229")
devtools::create("qfifo")#creating the package qfifo
setwd("C:/Users/sayei/Documents/Assignment10_18230229/qfifo")#setting up the directory inside qfifo inorder to pick the R files and descriptions present
#Now go inside qfifo/R to create the R file with all the required functions along with automatic documentations
#The functions are qfifo(),add(),add.qfifo(),top(),top.qfifo(),process(),process.qfifo().
#Save and run them and after this ,created the folder tests which contains testthat.R and testthat folder
#In testthat.R the test_check() is done for the package qfifo and then in testthat folder the R file is created with all the test conditions
devtools::load_all()#loading all the things present inside the created qfifo folder
options(
  usethis.description = list(
    Title = "Implementation of a FIFO queue package",
    Version = "0.0.1",
    `Authors@R` = 'person("Sai Krishna", "Lakshminarayanan", email = "s.lakshminarayanan1@nuigalway.ie", role = c("aut","cre"))',
    Description = "Package to perform FIFO queue operations",
    License = "MIT",
    Encoding = "UTF-8",
    LazyData = "true"
  )
)#creating description
usethis::use_description()#giving condition to use this created description
devtools::document()#to load and run the documents generated
devtools::test()#to load and run the tests for the qfifo package
#To display the documents of the functions
?qfifo
?add
?top
?process
