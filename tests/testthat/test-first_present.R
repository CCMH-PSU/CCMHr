test_that("first_present and last_present select first without order_by", {
  expect_equal(first_present(x = c(NA, 1, 2, NA)), 1)
  expect_equal(last_present(x = c(NA, 1, 2, NA)), 2)
})

test_that("first_present and last_present respect order_by", {
  expect_equal(first_present(x = c(NA, 1, 2, NA), order_by = c(4, 3, 2, 1)), 2)
  expect_equal(last_present(x = c(NA, 1, 2, NA), order_by = c(4, 3, 2, 1)), 1)
})

test_that("first_present and last_present respect default", {
  expect_equal(first_present(x = c(NA, NA), default = "none"), "none")
  expect_equal(last_present(x = c(NA, NA), default = "none"), "none")
})

test_that("first_present and last_present work in summarize", {
  expect_equal(dplyr::summarize(data.frame(x = c(NA, 1, 2, NA), Date = c("2019-01-01", "2020-01-01", "2019-10-10", "2020-02-02")), first = first_present(x, order_by = Date)) %>% dplyr::pull(first), 2)
  expect_equal(dplyr::summarize(data.frame(x = c(NA, 1, 2, NA), Date = c("2019-01-01", "2020-01-01", "2019-10-10", "2020-02-02")), last = last_present(x, order_by = Date)) %>% dplyr::pull(last), 1)
})
