#'Greedy Knapsack Solver
#'
#'This function solves the knapsack problem using a greedy approach.
#'
#'@param x A data frame with two columns, 'v' for values and 'w' for weights.
#'@param W The capacity of the knapsack.
#'
#'@return A list with the maximum value and a vector of selected item indices.
#'
#'@export greedy_knapsack




#generator<- function (){RNGversion(min(as.character(getRversion()), "3.5.3"))
#set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#n <- 2000
#knapsack_objects <-
#  data.frame(w = sample(1:4000, size = n, replace = TRUE),
#             v = runif(n = n, 0, 10000))
#}



greedy_knapsack <- function(x, W) {
  if (!is.data.frame(x)) {
    stop("You must provide a data frame.")
  }
  if (!is.numeric(W)) {
    stop("W must be a numeric value.")
  }
  if (W <= 0) {
    stop("W must be a positive number.")
  }
  if (!all(colnames(x) %in% c("w", "v"))) {
    stop("The data frame must have columns 'w' and 'v'.")
  }
  if (!all(x >= 0, na.rm = TRUE)) {
    stop("All values in x must be non-negative.")
  }
  x <- subset(x, v <= W)
  x <- x[order(x$v/x$w , decreasing = TRUE), ]

  total_weight <- 0
  n<- nrow(x)
  for (i in 1:n) {
    if (total_weight <= W) {
      total_weight <- total_weight + x$w[i]
      i<-i+1
    }else{
       break
     }
  }
  i <- if (i > n && total_weight <= W) i - 1 else i - 2
  value <- round(sum(x$v[1:i]))
  result <- list(value = value, elements = as.integer(rownames(x[1:i,])))
  return(result)
}



#result <- greedy_knapsack(x = knapsack_objects[1:1200, ], W = 2000)
#result<-greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#cat("$value\n")
#cat(result$value, "\n")
#cat("$elements\n")
#cat(result$elements, "\n")



# Generate random data for 1,000,000 objects (you can adjust values and weights as needed)
#set.seed(123)
#knapsack_objects_large <- data.frame(
#  ID = 1:1000000,
#  Value = sample(1:100, 1000000, replace = TRUE),
#  Weight = sample(1:50, 1000000, replace = TRUE)
#)

#greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)


# Set the knapsack capacity
#W <- 3500  # Adjust as needed

# Measure the execution time for the greedy algorithm
#timing <- system.time(greedy_knapsack(x = knapsack_objects_large, W = W))
#print(timing)
