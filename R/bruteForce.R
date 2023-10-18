#'Brute force knapsack
#'
#'@description This function implements the brute force knapsack.
#'
#'@param x It represents the input data frame with 2 columns w (weights) and v (values).
#'@param W It represents the knapsack size.
#'@param parallel It determines whether parallel computation is used and it is logical.
#'
#'@importFrom parallel detectCores makeCluster
#'@importFrom methods is.data.frame is.numeric
#'@importFrom utils intToBits
#'
#'@return list It contains the maximum value and the elements selected to maximize the knapsack value.
#'
#'@export brute_force_knapsack

#generator<- function (){RNGversion(min(as.character(getRversion()), "3.5.3"))
#set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#n <- 2000
#knapsack_objects <-
#  data.frame(w = sample(1:4000, size = n, replace = TRUE),
#             v = runif(n = n, 0, 10000))}


brute_force_knapsack <- function(x, W, parallel = FALSE) {
  if (!is.data.frame(x)) {
    stop("The 'x' must be a data frame.")
  }
  if (!is.numeric(W) || W <= 0) {
    stop("W must be a positive numeric value.")
  }
  if (!all(c("w", "v") %in% colnames(x))) {
    stop("The 'x' must be a data frame with column names 'w' and 'v'.")
  }
  if (!all(x$w > 0) || !all(x$v > 0)) {
    stop("The 'x' must be a data frame with only positive values.")
  }

  n <- nrow(x)
  counter <- 1:2^n
  combinations <- lapply(counter - 1, function(i) as.integer(intToBits(i)[1:n]))
  values <- weights <- rep(0, length(counter))

  if (parallel) {
    temp_dfs <- parallel::mclapply(combinations, function(y) {
      selected_items <- x[as.logical(y), ]
      list(value = sum(selected_items$v), weight = sum(selected_items$w))
    })
    values <- sapply(temp_dfs, function(x) x$value)
    weights <- sapply(temp_dfs, function(x) x$weight)
  } else {
    for (i in counter) {
      selected_items <- x[as.logical(combinations[[i]]), ]
      values[i] <- sum(selected_items$v)
      weights[i] <- sum(selected_items$w)
    }
  }

  relevant_indices <- which(weights <= W)
  max_value <- max(values[relevant_indices])
  result_index <- which(values == max_value)
  elements <- which(as.logical(combinations[[result_index]]))

  return(list(value = round(max_value), elements = elements))
}

#brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, TRUE)
#brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#system.time(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
#system.time(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE))

