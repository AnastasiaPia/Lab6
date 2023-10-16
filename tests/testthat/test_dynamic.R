library(testthat)

# Load the knapsack_dynamic function
source("dynamic.R")

# Test 1: Test with a simple data frame
test_that("Test with a simple data frame", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, 30))
  result <- knapsack_dynamic(x, 5)
  expect_equal(result[[1]], 50)
  expect_equal(result[[2]], c(1, 2, 3))
})

# Test 2: Test with empty data frame
test_that("Test with an empty data frame", {
  x <- data.frame(w = numeric(0), v = numeric(0))
  result <- knapsack_dynamic(x, 5)
  expect_equal(result[[1]], 0)
  expect_equal(result[[2]], numeric(0))
})

# Test 3: Test with negative weight
test_that("Test with negative weight", {
  x <- data.frame(w = c(2, 3, -4), v = c(10, 20, 30))
  expect_error(knapsack_dynamic(x, 5), "W must be positive.")
})

# Test 4: Test with non-numeric data frame
test_that("Test with non-numeric data frame", {
  x <- data.frame(w = c(2, 3, 4), v = c("a", "b", "c"))
  expect_error(knapsack_dynamic(x, 5), "The x must be a data frame.")
})

# Test 5: Test with no 'w' or 'v' columns
test_that("Test with no 'w' or 'v' columns", {
  x <- data.frame(x = c(2, 3, 4), y = c(10, 20, 30))
  expect_error(knapsack_dynamic(x, 5), "The x must be a data frame with column names 'w' and 'v'.")
})

# Test 6: Test with negative values in 'x'
test_that("Test with negative values in 'x'", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, -30))
  expect_error(knapsack_dynamic(x, 5), "You must provide positive values for x.")
})

# Test 7: Test with non-integer weights
test_that("Test with non-integer weights", {
  x <- data.frame(w = c(2.5, 3, 4), v = c(10, 20, 30))
  expect_error(knapsack_dynamic(x, 5), "weights must be positive integers")
})

# Test 8: Test with a large data frame
test_that("Test with a large data frame", {
  x <- data.frame(w = 1:50, v = 51:100)
  result <- knapsack_dynamic(x, 200)
  expect_equal(result[[1]], 2550)
  expect_equal(result[[2]], 1:50)
})

# Test 9: Test with a large weight limit
test_that("Test with a large weight limit", {
  x <- data.frame(w = 1:10, v = 11:20)
  result <- knapsack_dynamic(x, 1000)
  expect_equal(result[[1]], 155)
  expect_equal(result[[2]], 1:10)
})

# Test 10: Test with the smallest possible weight limit
test_that("Test with the smallest possible weight limit", {
  x <- data.frame(w = 1:10, v = 11:20)
  result <- knapsack_dynamic(x, 1)
  expect_equal(result[[1]], 0)
  expect_equal(result[[2]], numeric(0))
})
