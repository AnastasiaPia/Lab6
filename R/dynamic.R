#'Dynamic Knapsack
#'
#'@description
#'This function solves the 0/1 knapsack problem using dynamic programming. The knapsack problem is a classic optimization problem where you must choose a subset of items with given weights and values to maximize the total value while not exceeding a given weight limit.
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




knapsack_dynamic <- function(x, W) {                                            #R function that takes two arguments: x (a data frame with item information) and W (the weight capacity of the knapsack).
  if (!is.data.frame(x) || !is.numeric(x$w) || !is.numeric(x$v)) {              #It checks if x is a data frame and if it has the required columns 'w' and 'v'. If not, the function raises an error with the message "The x must be a data frame with column names 'w' and 'v'."
    stop("The x must be a data frame with column names 'w' and 'v'.")
  }

  if (!is.numeric(W)) {                                                         #It checks if W is a numeric value. If not, the function raises an error with the message "W must be a number."
    stop("W must be a number.")
  }
  if (W <= 0) {                                                                 #checks if W is positive. If W is not greater than 0, the function raises an error with the message "W must be positive."
    stop("W must be positive.")
  }
  if (!all(colnames(x) %in% c("w", "v"))) {                                     #checks if all column names in the x data frame are either 'w' or 'v'. If not, the function raises an error with the message "The x must be a data frame with column names 'w' and 'v'."
    stop("The x must be a data frame with column names 'w' and 'v'.")
  }
  if (!all(x > 0, na.rm = TRUE)) {                                              #It checks if all values in the x data frame are positive, including the weights and values. If any value is non-positive (including missing values, NA), the function raises an error with the message "You must provide positive values for x."
    stop("You must provide positive values for x.")
  }
  if (nrow(x) == 0) {                                                           #If there are no rows in the x data frame (i.e., nrow(x) is 0), the function returns a list containing 0 as the maximum value and an empty numeric vector as the list of selected elements, since there are no elements to select.
    return(list(0, numeric(0)))
  }
  for (i in x$w) {                                                              #Two for loops are used to check that all weights (w) in the data frame are positive integers. It uses the modulo operator (%%) to check if the weights are integers. If any weight is not an integer, the function raises an error.
    if (i %% 1 != 0)
      stop("weights must be positive integers")
  }
  for(i in x$w) {
    if(i %% 1 != 0)
      stop("weights must be positive integers")
  }
  n <- nrow(x)                                                                  #The n variable is assigned the number of rows in the data frame x, which represents the number of available items
  max_value <- matrix(0, nrow = n+1, ncol = W+1)                                #A max_value matrix is initialized with all elements set to 0. This matrix will be used to store the maximum values that can be achieved for different combinations of items and weight limits.
  elem_list <- list()                                                           #An empty list, elem_list, is initialized. This list will be used to keep track of the selected elements in the knapsack.
  for(i in 1:(n+1)*(W+1))                                                       #Nested for loops iterate through item indices (i) and weight limits (j) to calculate the maximum value that can be achieved for various combinations of items and weight limits.
    elem_list[i] <- list(NULL)
    dim(elem_list) <- c(n+1, W+1)
  for(i in 2:n+1) {
    item_value <- x$v[i-1]                                                      #item_value is assigned the value of the current item (indexed by i-1) from the 'v' column.
    item_weight <- x$w[i-1]                                                     #item_weight is assigned the weight of the current item (indexed by i-1) from the 'w' column
    for(j in 2:W+1) {
      if(item_weight > j-1) {                                                   #checks whether the weight of the current item exceeds the current weight limit (j-1). If it does, it means that the item cannot be included in the knapsack, so the maximum value is the same as the maximum value achieved without this item.
        max_value[i,j] <- max_value[i-1,j]
        if(is.null(elem_list[[i-1,j]]))
          elem_list[[i,j]] <- list(NULL)
        else
          elem_list[[i,j]] <- elem_list[[i-1,j]]
      }
      else {
        max_value[i,j] <- max(max_value[i-1, j], item_value + max_value[i-1, j-item_weight])   #If the item's weight is less than or equal to the current weight limit, the algorithm compares the maximum value achieved without the item  max_value[i-1, j]and the maximum value achieved by including the item item_value + max_value[i-1, j-item_weight]
        if(max_value[i-1, j] < item_value + max_value[i-1, j-item_weight])
          elem_list[[i, j]] <- c(elem_list[[i-1, j-item_weight]], i-1)                    #Depending on which value is larger, the corresponding elements (selected items) are updated in elem_list.
        else {
          if(is.null(elem_list[[i-1,j]]))
            elem_list[[i,j]] <- list(NULL)
          else
            elem_list[[i,j]] <- elem_list[[i-1,j]]
        }
      }
    }
  }
  elem_list <- unlist(elem_list[[n+1, W+1]])                                    #The elem_list at the end of the process contains the indices of the selected items that maximize the knapsack's value.
  result <- list(round(max_value[n+1, W+1]), elem_list)                         #The result is stored in a list containing the rounded maximum value and the list of selected elements (items).
  return(result)                                                                #The function returns this result.
}


#knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
#knapsack_dynamic(x = knapsack_objects[1:12, ], W = 3500)
#knapsack_dynamic(x = knapsack_objects[1:8, ], W = 2000)
#knapsack_dynamic(x = knapsack_objects[1:12, ], W = 2000)
#system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W =3500))
