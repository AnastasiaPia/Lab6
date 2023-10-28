#'Greedy Knapsack Solver
#'
#'This function solves the knapsack problem using a greedy approach. In this approach, items are sorted by their value-to-weight ratio in descending order and then selected one by one until the knapsack's weight limit is reached.
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

#set.seed(123)
#knapsack_objects_large <- data.frame(
#  ID = 1:1000000,
#  v = sample(1:100, 1000000, replace = TRUE),
#  w = sample(1:50, 1000000, replace = TRUE)
#)


greedy_knapsack <- function(x, W) {                                     # function that takes two arguments: x (a data frame with item information) and W (the weight capacity of the knapsack).
  if (!is.data.frame(x)) {                                              #checks if x is a data frame. If it's not, the function raises an error with the message "You must provide a data frame."
    stop("You must provide a data frame.")
  }
  if (!is.numeric(W)) {                                                 #checks if W is a numeric value. If it's not, the function raises an error with the message "W must be a numeric value."
    stop("W must be a numeric value.")
  }
  if (W <= 0) {                                                         #checks if W is a positive number. If W is not greater than 0, the function raises an error with the message "W must be a positive number."
    stop("W must be a positive number.")
  }
  if (!all(colnames(x) %in% c("w", "v"))) {                             #checks if the data frame x has the required columns 'w' and 'v'. If not, the function raises an error with the message "The data frame must have columns 'w' and 'v'."
    stop("The data frame must have columns 'w' and 'v'.")
  }
  if (!all(x >= 0, na.rm = TRUE)) {                                     #It checks if all values in the x data frame are non-negative, including both the weights ('w') and values ('v'). If any value is negative (including missing values, NA), the function raises an error with the message "All values in x must be non-negative."
    stop("All values in x must be non-negative.")
  }
  #x <- subset(x, v <= W)
  x <- x[order(x[,"v"]/x[,"w"], decreasing = TRUE), ]                   #The x data frame is sorted by the value-to-weight ratio in descending order. This is achieved by dividing the 'v' (value) column by the 'w' (weight) column and using order with the decreasing = TRUE argument. Items with the highest value-to-weight ratios are placed at the beginning of the data frame.

  total_weight <- 0                                                     #total_weight is initialized to 0, representing the total weight of items in the knapsack.
  n<- nrow(x)                                                           #n is assigned the number of rows in the data frame x, which represents the number of available items.
  for (i in 1:n) {                                                      #A for loop is used to iterate through the items in descending order of value-to-weight ratio.
    if ((total_weight + x$w[i]) <= W) {                                 #Within the loop, it checks if adding the weight of the current item to the total_weight would still keep the total weight below or equal to the knapsack's weight limit W.
      total_weight <- total_weight + x$w[i]                             #If the condition is met, the current item is added to the knapsack by increasing total_weight and incrementing the loop counter i.
      i<-i+1
    }else{
       break
     }
  }
  i <- if (i > n && total_weight <= W) i - 1 else i - 2                     #After the loop, i is adjusted to ensure that it points to the last item added to the knapsack. If i is greater than n and the total_weight is still less than or equal to W, i is reduced by 1 to ensure it points to the last item added. Otherwise, it is reduced by 2 to exclude the last item that exceeded the weight limit.
  value <- round(sum(x$v[1:i]))                                             #The value is calculated as the rounded sum of the values of the items from the beginning of the sorted data frame up to the ith item. This represents the total value of the items selected in the knapsack.
  result <- list(value = value, elements = as.integer(rownames(x[1:i,])))   #The result is stored in a list containing the calculated value and the indices (as integers) of the selected elements in the original data frame x
  return(result)                                                            #The function returns the result.
}


#W <- 50000
#timing <- system.time(result <- greedy_knapsack(x = knapsack_objects_large, W = W))
#print(timing)


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

#system.time(greedy_knapsack(x = knapsack_objects[1:1000000,], W =3500))
