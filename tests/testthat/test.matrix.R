context("Matrix")

test_that("NA detection is correct", {
  m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)], ncol = 2, byrow = FALSE)
  expect_equal(findRowsNA(m), c(3, 5))
  expect_equal(showRowsNA(m), matrix(c(NA, "f", NA, "h"), nrow = 2, byrow = TRUE))

  d <- data.frame(x = 1:10)
  expect_equal(findRowsNA(d), integer(0))
  expect_null(showRowsNA(d))
})
