context("Regression")

test_that("makeFormula", {
  f <- makeFormula("y", "x")
  expect_is(f, "formula")
  expect_equal(deparse(f), "y ~ x")

  f <- makeFormula("y", c("x1", "x2", "x3"))
  expect_is(f, "formula")
  expect_equal(deparse(f), "y ~ x1 + x2 + x3")

  f <- makeFormula("y", c("x1", "x2", "x3"), "dummies")
  expect_is(f, "formula")
  expect_equal(deparse(f), "y ~ x1 + x2 + x3 + dummies")
})
