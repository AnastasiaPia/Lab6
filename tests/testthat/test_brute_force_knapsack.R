


library(testthat)
library(Lab6)

test_check("Lab6")




# Load the brute_force_knapsack function
source("bruteForce.R")

# Test 1: Test with a simple data frame
test_that("Test with a simple data frame", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, 30))
  result <- brute_force_knapsack(x, 5)
  expect_equal(result$value, 40)
  expect_equal(result$elements, c(2, 3))
})

# Test 2: Test with empty data frame
test_that("Test with an empty data frame", {
  x <- data.frame(w = numeric(0), v = numeric(0))
  result <- brute_force_knapsack(x, 5)
  expect_equal(result$value, 0)
  expect_equal(result$elements, numeric(0))
})

# Test 3: Test with negative weight
test_that("Test with negative weight", {
  x <- data.frame(w = c(2, 3, -4), v = c(10, 20, 30))
  expect_error(brute_force_knapsack(x, 5), "W must be a positive numeric value.")
})

# Test 4: Test with non-numeric data frame
test_that("Test with non-numeric data frame", {
  x <- data.frame(w = c(2, 3, 4), v = c("a", "b", "c"))
  expect_error(brute_force_knapsack(x, 5), "The x must be a data frame.")
})

# Test 5: Test with no 'w' or 'v' columns
test_that("Test with no 'w' or 'v' columns", {
  x <- data.frame(x = c(2, 3, 4), y = c(10, 20, 30))
  expect_error(brute_force_knapsack(x, 5), "The x must be a data frame with column names 'w' and 'v'.")
})

# Test 6: Test with negative values in 'x'
test_that("Test with negative values in 'x'", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, -30))
  expect_error(brute_force_knapsack(x, 5), "The x must be a data frame with only positive values.")
})

# Test 7: Test with parallel computation
test_that("Test with parallel computation", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, 30))
  result <- brute_force_knapsack(x, 5, parallel = TRUE)
  expect_equal(result$value, 40)
  expect_equal(result$elements, c(2, 3))
})

# Test 8: Test with parallel computation and negative weight
test_that("Test with parallel computation and negative weight", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, 30))
  expect_error(brute_force_knapsack(x, -5, parallel = TRUE), "W must be a positive numeric value.")
})

# Test 9: Test with parallel computation and non-numeric data frame
test_that("Test with parallel computation and non-numeric data frame", {
  x <- data.frame(w = c(2, 3, 4), v = c("a", "b", "c"))
  expect_error(brute_force_knapsack(x, 5, parallel = TRUE), "The x must be a data frame.")
})

# Test 10: Test with parallel computation and negative values in 'x'
test_that("Test with parallel computation and negative values in 'x'", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, -30))
  expect_error(brute_force_knapsack(x, 5, parallel = TRUE), "The x must be a data frame with only positive values.")
})
