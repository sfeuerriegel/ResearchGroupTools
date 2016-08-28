context("Matrix")

test_that("row-wise NA detection is correct", {
  m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)], ncol = 2, byrow = FALSE)
  expect_equal(findRowsNA(m), c(3, 5))
  expect_equal(showRowsNA(m), matrix(c(NA, "f", NA, "h"), nrow = 2, byrow = TRUE))

  d <- data.frame(x = 1:10)
  expect_equal(findRowsNA(d), integer(0))
  expect_null(showRowsNA(d))

  m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)], ncol = 2, byrow = FALSE)
  colnames(m) <- c("x", "y")
  expect_equal(findRowsNA(m), c(3, 5))
  expect_equal(showRowsNA(m), matrix(c(NA, "f", NA, "h"), nrow = 2, byrow = TRUE,
                                     dimnames = list(NULL, c("x", "y"))))

  d <- matrix(1:10)
  colnames(d) <- "x"
  expect_equal(findRowsNA(d), integer(0))
  expect_null(showRowsNA(d))
})

test_that("column-wise NA detection is correct", {
  m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)], ncol = 2, byrow = FALSE)
  colnames(m) <- c("x", "y")
  expect_equal(findColsNA(m), "x")
  expect_equal(showColsNA(m), c("a", "b", NA, "c", NA))

  d <- data.frame(x = 1:10)
  expect_equal(findColsNA(d), character(0))
  expect_null(showColsNA(d))

  m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)], ncol = 2, byrow = FALSE)
  colnames(m) <- c("x", "y")
  m <- as.data.frame(m, stringsAsFactors=FALSE)
  expect_equal(findColsNA(m), "x")
  expect_equal(showColsNA(m), c("a", "b", NA, "c", NA))

  d <- matrix(1:10)
  colnames(d) <- "x"
  expect_equal(findColsNA(d), character(0))
  expect_null(showColsNA(d))
})
