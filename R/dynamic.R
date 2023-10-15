knapsack_dynamic <- function(cx, W) {
  # Check input validity
  if (!is.data.frame(cx) || !all(c("v", "w") %in% colnames(cx))) {
    stop("Input must be a data.frame with columns 'v' and 'w'.")
  }

  if (any(cx$v <= 0) || any(cx$w <= 0) || W <= 0) {
    stop("All values in the data.frame and W must be positive.")
  }

  n <- nrow(cx)
  max_values <- matrix(0, nrow = n + 1, ncol = W + 1)

  for (i in 1:(n + 1)) {
    for (w in 0:(W + 1)) {
      if (i == 0 || w == 0) {
        max_values[i, w] <- 0
      } else if (i <= n && w >= cx$w[i]) {
        max_values[i, w] <- max(max_values[i - 1, w], max_values[i - 1, w - cx$w[i]] + cx$v[i])
      } else {
        max_values[i, w] <- max_values[i - 1, w]
      }
    }
  }



  max_value <- max_values[n + 1, W + 1]
  selected_indices <- integer(0)
  i <- n
  w <- W
  while (i > 0 && w > 0) {
    if (max_values[i, w] != max_values[i - 1, w]) {
      selected_indices <- c(selected_indices, i)
      w <- w - cx$w[i]
    }
    i <- i - 1
  }

  return(list(value = max_value, elements = rev(selected_indices)))
}


