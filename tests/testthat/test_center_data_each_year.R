context("center_data_each_year")

library(CCMHr)

test <- data.frame(CcmhID = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3), Data_year = c(2010, 2011, 2012, 2013, 2013, 2010, 2011, 2012, 2013, 2010, 2011, 2012))

test_solution <- tibble::tibble(CcmhID = c(1, 1, 1, 1, 1, 2, 2, 2, 2), Data_year = c(2010, 2011, 2012, 2013, 2013, 2010, 2011, 2012, 2013))

test2 <- data.frame(CcmhID = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3), Data_years = c(2010, 2011, 2012, 2013, 2010, 2011, 2012, 2013, 2010, 2011, 2012))

test3 <- data.frame(CcmhID = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3), Data_year = c(2010, 2011, 2012, 2013, 2010, 2011, 2012, 2013, 2010, 2011, NA))

test4 <- data.frame(CcmhID = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, NA), Data_year = c(2010, 2011, 2012, 2013, 2010, 2011, 2012, 2013, 2010, 2011, 2012))


test_that("Center data each year operates correctly", {
  expect_equal(center_data_each_year(test), test_solution)
})

test_that("Center data each year error- Cols misnamed", {
  expect_error(center_data_each_year(test2))
})

test_that("Center data each year error- Missing data year", {
  expect_error(center_data_each_year(test3))
})

test_that("Center data each year error- Missing CcmhID", {
  expect_error(center_data_each_year(test4))
})
