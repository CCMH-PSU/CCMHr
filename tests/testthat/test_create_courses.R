context("create_courses")

library(CCMHr)

test <- data.frame(UniqueClientID = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3), Date = as.Date(c('2010/1/1','2010/1/2','2011/1/1','2011/1/2','2011/1/3','2011/1/4','2011/1/5','2011/1/6','2011/2/6','2012/1/1',
                                                                                               '2012/1/1','2012/2/1','2013/2/1')))

output_all <- data.frame(
  UniqueClientID = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3),
  UniqueClientID_byCourse = c(1.1, 1.1, 1.2, 1.2, 2.1, 2.1, 2.1, 3.1, 3.1,
                              3.2, 3.2, 3.2, 3.3),
  Date = as.Date(c("2010-01-01", "2010-01-02", "2011-01-01",
           "2011-01-02", "2011-01-03", "2011-01-04",
           "2011-01-05", "2011-01-06", "2011-02-06", "2012-01-01",
           "2012-01-01", "2012-02-01", "2013-02-01")),
  RankCourse = c(1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 3),
  FirstCourse = c(1, 1, NA, NA, 1, 1, 1, 1, 1, NA, NA, NA, NA)
)

output_first <- data.frame(
  UniqueClientID = c(1, 1, 2, 2, 2, 3, 3),
  Date = as.Date(c("2010-01-01", "2010-01-02", "2011-01-03", "2011-01-04",
           "2011-01-05", "2011-01-06", "2011-02-06"))
)

# Tests

test_that("Course creation output is correct- retain all courses", {
  expect_equal(create_courses(test), output_all)
})

test_that("Course creation output is correct- retain all courses explicit", {
  expect_equal(create_courses(test, firstOnly = FALSE), output_all)
})

test_that("Course creation output is correct- first course only", {
  expect_equal(create_courses(test, firstOnly = TRUE), output_first)
})

test_that("Course creation no UniqueClientID error", {
  expect_error(create_courses(dplyr::select(test, -UniqueClientID)), "Data does not contain column: UniqueClientID")
})

test_that("Course creation no Date error", {
  expect_error(create_courses(dplyr::select(test, -Date)), "Data does not contain column: Date")
})
