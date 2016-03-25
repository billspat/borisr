library(testthat)
library(jsonlite)
library(borisr)

context("testing  boris")

# TO DO:
boris_file = '../../data/examplevideotrial.boris'
boris.data = borisr::read_boris(boris_file)
test_obs_names = c("animalu22","animalu41")  # to do, make it generic

test_that("can open a boris file", {
  expect_gt(length(boris.data),0)
})

test_that("can read boris data structure ", {
  expect_gt(length(names(boris.data)),0)
  expect_true("observations" %in% names(dat))
})

test_that("can get obs ",{
  obs = get_obslist(boris.data)
  expect_gt(length(obs),0)
  for(ob in test_obs_names) {expect_true(ob %in% obs)}

})

