library(testthat)
library(qfifo)
context("qfifo tests...")
test_that("Constructor created ok ...", {
  q <- qfifo()
  expect_true(class(q) == "qfifo")
})
test_that("Add a value to the queue ...", {
  q <- qfifo()
  q <- add(q,"456")
  expect_true(q$data[[1]] == "456")
})
test_that("Check that top () works...", {
  q <- qfifo()
  q <- add(q,"456")
  expect_true(top(q) == "456")
})
test_that("Check that process() works...", {
  q <- qfifo()
  q <- add(q,"456")
  q <- add(q,"789")
  q <- process(q)
  expect_true(top(q) == "789")
})