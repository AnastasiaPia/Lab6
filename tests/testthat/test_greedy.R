context("greedy_knapsack")


suppressWarnings(RNGversion(min(as.character(getRversion(
)), "3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
                               v = runif(n = n, 0, 10000))


test_that("functions rejects errounous input.", {
  expect_error(greedy_knapsack("hej", 3500))
  expect_error(greedy_knapsack(x = knapsack_objects[1:8, ], W = -3500))
})

test_that("Function return correct results.", {
  st <-
    system.time(gk <-
                  greedy_knapsack(x = knapsack_objects[1:16, ], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)

  gk <- greedy_knapsack(x = knapsack_objects[1:800, ], W = 3500)
  expect_equal(round(gk$value), 42430)

  gk <- greedy_knapsack(x = knapsack_objects[1:1200, ], W = 3500)
  expect_equal(round(gk$value), 55392)
})
