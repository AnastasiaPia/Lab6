context("knapsack_dynamic")



test_that("Test with a simple data frame", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, 30))
  result <- knapsack_dynamic(x, 5)
  expect_equal(result[[1]], 30)
  expect_equal(result[[2]], c(3))
})

test_that("Test with an empty data frame", {
  x <- data.frame(w = numeric(0), v = numeric(0))
  result <- knapsack_dynamic(x, 5)
  expect_equal(result[[1]], 0)
  expect_equal(result[[2]], numeric(0))
})



test_that("Test with non-numeric data frame", {
  x <- data.frame(w = c(2, 3, 4), v = c("a", "b", "c"))
  expect_error(knapsack_dynamic(x, 5), "The x must be a data frame.")
})


test_that("Test with no 'w' or 'v' columns", {
  x <- data.frame(x = c(2, 3, 4), y = c(10, 20, 30))
  expect_error(knapsack_dynamic(x, 5), "The x must be a data frame with column names 'w' and 'v'.")
})


test_that("Test with negative values in 'x'", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, -30))
  expect_error(knapsack_dynamic(x, 5), "You must provide positive values for x.")
})


test_that("Test with non-integer weights", {
  x <- data.frame(w = c(2.5, 3, 4), v = c(10, 20, 30))
  expect_error(knapsack_dynamic(x, 5), "weights must be positive integers")
})




