library(testthat)

# Load the greedy_knapsack function
source("greedy.R")

# Test 1: Test with a simple data frame
test_that("Test with a simple data frame", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, 30))
  result <- greedy_knapsack(x, 5)
  expect_equal(result$value, 50)
  expect_equal(result$elements, c(3, 2, 1))
})

# Test 2: Test with empty data frame
test_that("Test with an empty data frame", {
  x <- data.frame(w = numeric(0), v = numeric(0))
  result <- greedy_knapsack(x, 5)
  expect_equal(result$value, 0)
  expect_equal(result$elements, numeric(0))
})

# Test 3: Test with negative weight
test_that("Test with negative weight", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, 30))
  expect_error(greedy_knapsack(x, -5), "W must be a positive number.")
})

# Test 4: Test with non-numeric data frame
test_that("Test with non-numeric data frame", {
  x <- data.frame(w = c(2, 3, 4), v = c("a", "b", "c"))
  expect_error(greedy_knapsack(x, 5), "You must provide a data frame.")
})

# Test 5: Test with no 'w' or 'v' columns
test_that("Test with no 'w' or 'v' columns", {
  x <- data.frame(x = c(2, 3, 4), y = c(10, 20, 30))
  expect_error(greedy_knapsack(x, 5), "The data frame must have columns 'w' and 'v'.")
})

# Test 6: Test with negative values in 'x'
test_that("Test with negative values in 'x'", {
  x <- data.frame(w = c(2, 3, 4), v = c(10, 20, -30))
  expect_error(greedy_knapsack(x, 5), "All values in x must be non-negative.")
})

# Test 7: Test with a large data frame
test_that("Test with a large data frame", {
  x <- data.frame(w = 1:50, v = 51:100)
  result <- greedy_knapsack(x, 200)
  expect_equal(result$value, 2550)
  expect_equal(result$elements, 1:50)
})

# Test 8: Test with a large weight limit
test_that("Test with a large weight limit", {
  x <- data.frame(w = 1:10, v = 11:20)
  result <- greedy_knapsack(x, 1000)
  expect_equal(result$value, 155)
  expect_equal(result$elements, 1:10)
})

# Test 9: Test with the smallest possible weight limit
test_that("Test with the smallest possible weight limit", {
  x <- data.frame(w = 1:10, v = 11:20)
  result <- greedy_knapsack(x, 1)
  expect_equal(result$value, 0)
  expect_equal(result$elements, numeric(0))
})

# Test 10: Test with no items fit in the knapsack
test_that("Test with no items fit in the knapsack", {
  x <- data.frame(w = c(6, 7, 8), v = c(10, 20, 30))
  result <- greedy_knapsack(x, 5)
  expect_equal(result$value, 0)
  expect_equal(result$elements, numeric(0))
})
