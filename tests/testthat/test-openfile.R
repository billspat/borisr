library(testthat)
library(jsonlite)

context("testing  boris")
# should test this for ALL behaviors - in a loop?
boris_file = '../../data/examplevideotrial.boris'


test_that("can open a boris file", {
  boris.data = read_boris(boris_file)
  expect_gt(length(boris.data),0)
})


