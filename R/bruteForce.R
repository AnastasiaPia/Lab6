brute_force_knapsack <- function(cx, W) {
  # Check input validity
  if (!is.data.frame(cx) || !all(c("v", "w") %in% colnames(cx))) {
    stop("Input must be a data.frame with columns 'v' and 'w'.")
  }

  if (any(cx$v <= 0) || any(cx$w <= 0) || W <= 0) {
    stop("All values in the data.frame and W must be positive.")
  }

  n <- nrow(cx)
  max_value <- 0
  best_combination <- integer(n)

  # Generate all binary combinations for 2^n possibilities
  for (i in 1:(2^n - 1)) {
    combination <- as.integer(rev(intToBits(i))[1:n])  # Reverse and trim to n bits
    total_weight <- sum(combination * cx$w)

    if (total_weight <= W) {
      total_value <- sum(combination * cx$v)
      if (total_value > max_value) {
        max_value <- total_value
        best_combination <- combination
      }
    }
  }

  # Get the indices of the selected elements using best_combination
  selected_indices <- which(best_combination == 1)

  return(list(value = max_value, elements = selected_indices))
}

# Example usage:
# Assuming that knapsack_objects is your data.frame with 'v' and 'w' columns
result <- brute_force_knapsack(knapsack_objects[1:8,], W = 3500)

# Print the maximum value and selected elements
cat("Value:", result$value, "\n")
cat("Elements:", result$elements, "\n")
