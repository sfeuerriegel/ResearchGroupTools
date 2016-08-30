context("strings")

test_that("string concatenation operator", {
  s <- "a" %+% "b"
  expect_equal(s, "ab")

  s <- 3 %+% 4
  expect_equal(s, "34")

  s <- `%+%`(letters)
  expect_equal(letters, paste0(letters))
})
