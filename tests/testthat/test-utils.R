context("utilities")

test_that("split of 3 comma list has 3 values",{
  str = "a,b,c"
  r = strsplitrows(str)
  expect_equal(length(r),3)
})

test_that("strsplitrow returns 1 char string on null",{
  expect_equal(strsplitrows(NULL)," ")
})

test_that("make_two makes 2 from 1", {
  v = c("abc")
  expect_equal(
      length(make_two(v)), 2
      )
})

test_that("make_two makes 2 from 2", {
  v = c("abc", "def")
  expect_equal(
    length(make_two(v)), 2
  )
})
