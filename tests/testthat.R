library(testthat)
library(ROCit)


test_that("trapezoidarea is the area under curve using trapezoid rule", {
  expect_equal(trapezoidarea(c(0,1),c(0,1)), 0.5)
  expect_equal(trapezoidarea(c(0,0,1),c(0,1,1)), 1)
  expect_equal(trapezoidarea(c(1,2,3),c(1,2,3)), 4)
  expect_equal(trapezoidarea(c(1,11,16),c(1,3,2)), 32.5)
})


test_that("invlogit computes logistic transform", {
  expect_equal(plogis(0), 0.5)
  expect_equal(plogis(-Inf), 0)
  expect_equal(plogis(Inf), 1)
  expect_equal(round(plogis(0.5),2), 0.62)
})




