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
    stop("The x must be a data frame.")                   #checks if the argument is a data frame
  }
  if (!is.numeric(W) || W <= 0) {
    stop("W must be a positive numeric value.")           #checks if the W is a numerical value
  }
  if (!all(c("w", "v") %in% colnames(x))) {               #checks if the data frame has columns w,v
    stop("The x must be a data frame with column names 'w' and 'v'.")
  }
  if (!all(x > 0, na.rm = TRUE)) {                        #checks if the values in the data frame are positive
    stop("The x must be a data frame with only positive values.")
  }
  x <- x[x$w <= W, ]                                                       #filters the rows of the x data frame, retaining only those with weight ('w') less than or equal to the knapsack capacity W.
  n <- nrow(x)                                                             #n is assigned the number of rows in the filtered x data frame, which represents the number of available items.
  big_o<- 2 ^ n - 1                                                        #the number of possible combinations of items 2^n-1 that can be included in the knapsack problem
  if (parallel) {                                                          #If parallel is set to TRUE, it enters the parallel computation section.
    cores <- parallel::detectCores()                                       #It detects the number of CPU cores available for parallel processing
    cl <- parallel::makeCluster(cores)                                     #A cluster of workers for parallel computation is created
    combinations <- parallel::parLapply(cl, 1:big_o, function(x) {         #The combinations variable is populated with all possible combinations of items. It uses parallel::parLapply to distribute the combinations across the worker nodes
      as.integer(head(intToBits(x), n))
    })
    weight <- unlist(lapply(combinations, function(combin) {               #weight is calculated for each combination of items, and a list of these weights is obtained.
      sum(x$w[as.logical(combin)])
    }))

    n <- length(combinations)
    values <- numeric(n)                                                   #values is initialized as an empty numeric vector.

    for (i in 1:n) {                                                       #It iterates through the combinations, calculates the total value for each combination, and stores it in the values vector.
      subset_values <- x$v[as.logical(combinations[[i]])]
      values[i] <- sum(subset_values)
    }

    i <- which.max(value)                                                  #The index i of the combination with the maximum value is determined using which.max.
    value <- round(value[i])                                               #The maximum value is rounded and assigned to the value variable
    elements <- as.integer(rownames(x[as.logical(combinations[[i]]), ]))   #The elements (items) included in the knapsack are extracted based on the chosen combination and stored in the elements variable.
    parallel::stopCluster(cl)                                              #The cluster of workers is stopped using parallel::stopCluster
    return(list(value = value, elements = elements))
  } else {                                                                 #If parallel is set to FALSE, it enters the non-parallel computation section.
    combinations <- matrix(nrow = big_o, ncol = n)                         #The combinations matrix is initialized to store all possible combinations.
    for (i in 1:big_o) {
      combinations[i, ] <- as.integer(head(intToBits(i), n))               #It iterates through the combinations and calculates each combination using bitwise operations (intToBits).
    }
    weight <- sapply(1:big_o, function(i) sum(x$w[as.logical(combinations[i, ])])  #weight is calculated for each combination of items, and a list of these weights is obtained.
    )
    combinations <- combinations[weight <= W, ]                                    #The combinations matrix is filtered to keep only combinations where the total weight does not exceed the knapsack capacity.
    value <- sapply(1:nrow(combinations), function(i) sum(x$v[as.logical(combinations[i, ])])   #The value variable is initialized as an empty numeric vector.
    )                                                                                           #Thw value variable iterates through the filtered combinations and calculates the total value for each combination, storing it in the value vector.
    i <- which.max(value)                                                  #The index i of the combination with the maximum value is determined using which.max.
    value <- round(value[i])                                               #The maximum value is rounded and assigned to the value variable.
    elements <- as.integer(rownames(x[as.logical(combinations[i, ]), ]))   #The elements (items) included in the knapsack are extracted based on the chosen combination and stored in the elements variable.
    return(list(value = value, elements = elements))                       #the function returns a list containing the maximum value and the indices of the selected elements in the knapsack.
  }
}


#brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
#brute_force_knapsack(x = knapsack_objects[1:12, ], W = 3500)
#brute_force_knapsack(x = knapsack_objects[1:8, ], W = 2000)
#brute_force_knapsack(x = knapsack_objects[1:12, ], W = 2000)
