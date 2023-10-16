#'Dynamic Knapsack
#'
#'@description
#'This function solves the 0/1 knapsack problem using dynamic programming.
#'
#'@param x A data frame with two columns, 'v' for values and 'w' for weights.
#'@param W The capacity of the knapsack.
#'
#'@return A list with the maximum value and a vector of selected item indices.
#'
#' @export knapsack_dynamic





#generator<- function (){RNGversion(min(as.character(getRversion()), "3.5.3"))
#set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#n <- 2000
#knapsack_objects <-
#  data.frame(w = sample(1:4000, size = n, replace = TRUE),
#             v = runif(n = n, 0, 10000))
#}




knapsack_dynamic <- function(x, W) {
  if (!is.data.frame(x) || !is.numeric(x$w) || !is.numeric(x$v)) {
    stop("The x must be a data frame with column names 'w' and 'v'.")
  }

  if (!is.numeric(W)) {
    stop("W must be a number.")
  }
  if (W <= 0) {
    stop("W must be positive.")
  }
  if (!all(colnames(x) %in% c("w", "v"))) {
    stop("The x must be a data frame with column names 'w' and 'v'.")
  }
  if (!all(x > 0, na.rm = TRUE)) {
    stop("You must provide positive values for x.")
  }
  if (nrow(x) == 0) {
    return(list(0, numeric(0)))
  }
  for (i in x$w) {
    if (i %% 1 != 0)
      stop("weights must be positive integers")
  }
  for(i in x$w) {
    if(i %% 1 != 0)
      stop("weights must be positive integers")
  }
  n <- nrow(x)
  max_value <- matrix(0, nrow = n+1, ncol = W+1)
  elem_list <- list()
  for(i in 1:(n+1)*(W+1))
    elem_list[i] <- list(NULL)
    dim(elem_list) <- c(n+1, W+1)
  for(i in 2:n+1) {
    item_value <- x$v[i-1]
    item_weight <- x$w[i-1]
    for(j in 2:W+1) {
      if(item_weight > j-1) {
        max_value[i,j] <- max_value[i-1,j]
        if(is.null(elem_list[[i-1,j]]))
          elem_list[[i,j]] <- list(NULL)
        else
          elem_list[[i,j]] <- elem_list[[i-1,j]]
      }
      else {
        max_value[i,j] <- max(max_value[i-1, j], item_value + max_value[i-1, j-item_weight])
        if(max_value[i-1, j] < item_value + max_value[i-1, j-item_weight])
          elem_list[[i, j]] <- c(elem_list[[i-1, j-item_weight]], i-1)
        else {
          if(is.null(elem_list[[i-1,j]]))
            elem_list[[i,j]] <- list(NULL)
          else
            elem_list[[i,j]] <- elem_list[[i-1,j]]
        }
      }
    }
  }
  elem_list <- unlist(elem_list[[n+1, W+1]])
  result <- list(round(max_value[n+1, W+1]), elem_list)
  return(result)
}


#knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
#knapsack_dynamic(x = knapsack_objects[1:12, ], W = 3500)
#knapsack_dynamic(x = knapsack_objects[1:8, ], W = 2000)
#knapsack_dynamic(x = knapsack_objects[1:12, ], W = 2000)
