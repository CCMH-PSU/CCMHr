context("remove_empty")

library(CCMHr)
library(testthat)


test_that("Remove empty works", {
  expect_equal(remove_empty(tibble::tibble(a = c(1, 2, 3), b = c(NA, 1, 2), c = c(NA, NA, NA))), tibble::tibble(a = c(1, 2, 3), b = c(NA, 1, 2)))
  })
