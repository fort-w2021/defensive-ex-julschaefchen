library(testthat)

context("lag")

test_that("basically works", {
  # check structure:
  expect_vector(lag(mtcars$mpg, 1),
                size = nrow(mtcars))
  
  # check character vector
  expect_vector(lag(names(mtcars), 1),
                size = length(names(mtcars)))
  
  # check factor vector
  expect_vector(lag(as.factor(mtcars$mpg), 1),
                size = nrow(mtcars))

})

test_that("input works", {
  
  # check numeric:
  expect_true(is.na(lag(5)))
  
  # check list:
  expect_equivalent(lag(list(1, 2)),
                    list(NA, 1))
  expect_warning(lag(list(1, 2)))
  
  # check data.frame
  expect_true(is.na(lag(mtcars)))
  
  # check matrix
  expect_warning(lag(matrix(1:9, 3, 3)))
})

test_that("n small enough", {
  expect_error(lag(1:2, 5))
})






