library(dplyr)

context("data handling")

test_that("pull works correctly", {
  d <- data_frame(x = 1:10,
                  y = rnorm(10))

  vec <- d %>% pull(x)
  expect_equal(vec, 1:10)
  vec <- d %>% pull("x")
  expect_equal(vec, 1:10)

  v <- "x"
  vec <- d %>% pull_string(v)
  expect_equal(vec, 1:10)

  vec <- d %>% pull_ith(1)
  expect_equal(vec, 1:10)
})
