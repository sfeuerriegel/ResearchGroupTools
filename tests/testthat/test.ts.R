context("Time series")

test_that("returns are computed correctly", {
  r <- returns(1:10)
  expect_equal(r, c(NA, (1/(1:9))))

  r <- returns(1:10, na_padding = FALSE)
  expect_equal(r, (1/(1:9)))

  r <- returns(c(1, 2, 4, 8, 16, 32))
  expect_equal(r, c(NA, 1, 1, 1, 1, 1))

  r <- returns(c(1, 1.5, 1.5^2, 1.5^3))
  expect_equal(r, c(NA, 0.5, 0.5, 0.5))

  r <- returns(c(1, 2, 4, 8, 16, 32), lag = 2)
  expect_equal(r, c(NA, NA, 3, 3, 3, 3))

  r <- returns(c(1, 2, 3, 6, 9, 18), lag = 2)
  expect_equal(r, c(NA, NA, 2, 2, 2, 2))

  r <- returns(c(1, 2, 4, 8, 16, 32), lag = 2, na_padding = FALSE)
  expect_equal(r, c(3, 3, 3, 3))

  r <- returns(c(32, 16, 8, 4, 2, 1))
  expect_equal(r, c(NA, -0.5, -0.5, -0.5, -0.5, -0.5))

})

test_that("log-returns are computed correctly", {
  r <- logReturns(1:10)
  expect_equal(r, log(c(NA, (1/(1:9)))))

  r <- logReturns(1:10, na_padding = FALSE)
  expect_equal(r, log((1/(1:9))))

  r <- logReturns(c(1, 2, 4, 8, 16, 32), base = 2)
  expect_equal(r, c(NA, 0, 0, 0, 0, 0))

  r <- logReturns(c(1, 2, 4, 8, 16, 32), base = 2, na_padding = FALSE)
  expect_equal(r, c( 0, 0, 0, 0, 0))
})
