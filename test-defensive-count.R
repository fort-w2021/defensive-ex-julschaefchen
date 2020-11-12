library(testthat)

context("checking count_them")

test_that("does the right thing for sensible inputs", {
  expect_equivalent(count_them(1), 1L)
  expect_equivalent(count_them(0.0), 0L)
  expect_identical(count_them(1.0), 1L)
  expect_warning(count_them(1.4), "rounding")
  expect_identical(suppressWarnings(count_them(2.6)), 3L)
})

test_that("does the right thing for problematic inputs", {
  # add the missing "regexp" arguments to test that
  # these trigger *informative* error messages
  # (i.e., errors that don't come from <round()>,
  # but from your own input checks, preferably.)
  
  # I'm not sure what you mean by regexp arguments.
  
  expect_error(count_them(-1.1), "Only")
  
  # QUESTION: Why is it not supposed to handle vectors?
  # expect_error(count_them(c(1.1, 2))) 
  
  expect_error(count_them(NA_real_), "NA")
  expect_error(count_them(NA), "NA")
  expect_error(count_them(Inf), "Only")
  expect_error(count_them("a"), "Only")
  expect_error(count_them(list("a", "b", 20)), "Only")
})
