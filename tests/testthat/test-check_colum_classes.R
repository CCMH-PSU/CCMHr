context("check_column_classes")

library(CCMHr)
library(testthat)

test1 <- data.frame(a = c(1, 2, 3), b = c(1, 2, 3), c = c("string"))
test2 <- data.frame(a = c(1, 2, 3), B = c(1, 2, 3), c = c(1, 2, 3))

# test_that("check column classes works", {
#   expect_equal(check_column_classes(test1, test2),
#                tibble::tibble(Column = c("c"), test1 = ("character"), test2 = ("numeric")))
# })
