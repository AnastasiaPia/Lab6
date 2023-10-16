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


brute_force_knapsack<-function(x, W, parallel = FALSE) {  #parallel is to show the parallel computation
  if (!is.data.frame(x)) {
    stop("The x must be a data frame.")
  }
  if (!is.numeric(W) || W <= 0) {
    stop("W must be a positive numeric value.")
  }
  if (!all(c("w", "v") %in% colnames(x))) {
    stop("The x must be a data frame with column names 'w' and 'v'.")
  }
  if (!all(x > 0, na.rm = TRUE)) {
    stop("The x must be a data frame with only positive values.")
  }
  x <- x[x$w <= W, ]
  n <- nrow(x)
  big_o<- 2 ^ n - 1    #the number of combinations of items 2^n-1 in the knapsack problem
  if (parallel) {
    cores <- parallel::detectCores()
    cl <- parallel::makeCluster(cores)
    combinations <- parLapply(cl, 1:big_o, function(x) {
      as.integer(head(intToBits(x), n))
    })
    weight <- unlist(lapply(combinations, function(x) {
      sum(x$w[as.logical(x)])
    }))

    n <- length(combinations)
    values <- numeric(n)

    for (i in 1:n) {
      subset_values <- x$v[as.logical(combinations[[i]])]
      values[i] <- sum(subset_values)
    }

    i <- which.max(value)
    value <- round(value[i])
    elements <- as.integer(rownames(x[as.logical(combinations[[i]]), ]))
    stopCluster(cl)
    return(list(value = value, elements = elements))
  } else {
    combinations <- matrix(nrow = big_o, ncol = n)
    for (i in 1:big_o) {
      combinations[i, ] <- as.integer(head(intToBits(i), n))
    }
    weight <- sapply(1:big_o, function(i) sum(x$w[as.logical(combinations[i, ])])
    )
    combinations <- combinations[weight <= W, ]
    value <- sapply(1:nrow(combinations), function(i) sum(x$v[as.logical(combinations[i, ])])
    )
    i <- which.max(value)
    value <- round(value[i])
    elements <- as.integer(rownames(x[as.logical(combinations[i, ]), ]))
    return(list(value = value, elements = elements))
  }
}


#brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
#brute_force_knapsack(x = knapsack_objects[1:12, ], W = 3500)
#brute_force_knapsack(x = knapsack_objects[1:8, ], W = 2000)
#brute_force_knapsack(x = knapsack_objects[1:12, ], W = 2000)
