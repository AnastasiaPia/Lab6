greedy_knapsack <- function(x, W) {
  n <- nrow(x)
  x <- x[order(x$Value / x$Weight, decreasing = TRUE), ]

  total_value <- 0
  total_weight <- 0
  selected_items <- c()

  for (i in 1:n) {
    if (total_weight + x$Weight[i] <= W) {
      total_weight <- total_weight + x$Weight[i]
      total_value <- total_value + x$Value[i]
      selected_items <- c(selected_items, x$ID[i])
    }
    if (total_weight >= W) {
      break
    }
  }

  result <- list(value = total_value, elements = selected_items)
  return(result)
}

# Example usage
set.seed(123) # For reproducibility
knapsack_objects <- data.frame(
  ID = 1:1200,
  Value = sample(1:100, 1200, replace = TRUE),
  Weight = sample(1:50, 1200, replace = TRUE)
)

#result <- greedy_knapsack(x = knapsack_objects[1:1200, ], W = 2000)
result<-greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
cat("$value\n")
cat(result$value, "\n")
cat("$elements\n")
cat(result$elements, "\n")
