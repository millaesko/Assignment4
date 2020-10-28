#' brute_force:knapsack
#'
#'@description
#'A function that solves the knapsack problem:
#'we have a knapsack that can take a limited weight W, which we want to fill with a number of items i = 1,...,n, each with
#'a weight w_i and a value v_i. The goal is to find the knapsack with the largest value of the elements added to the knapsack.
#'
#' @param x a data frame with two variables, v (value) and w (weight).
#' @param W the knapsack size
#'
#' @return
#' The function returns a list with two elements: the value of the knapsack and the items it consists of.
#' @export
#'
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects<-data.frame(w =sample(1: 4000,size= n,replace= TRUE),
#'                             v =runif(n = n,0, 10000))
#'
#' brute_force_knapsack(x = knapsack_objects [1: 12,],W = 3500)
#' brute_force_knapsack(x = knapsack_objects [1:12,],W = 2000)

brute_force_knapsack<-function(x, W){
  stopifnot(is.data.frame(x) |
              all(variable.names(x) %in% c("v","w"))|
              all(x)>=0)
  x <- cbind(i=c(1:nrow(x)),x) # keeps track of item number
  y <- subset(x,w<=W) # filters out items that weigh more than W
  nr_of_items<-nrow(y) # code from binary_combinations.pdf
  n = 2^nr_of_items
  all_combn_decimal <- 0:(n-1)
  all_combn_matrix <-
    sapply(all_combn_decimal, function(x){
    result <- intToBits(x)
    result <- as.numeric(result)
    return(result[1:nr_of_items]) # returns matrix with columns representing each combination of items
    })
  results<-data.frame(i=c(1:n),
                      w = colSums(y$w*all_combn_matrix), # total weights for each combinations
                      v = colSums(y$v*all_combn_matrix)) # total value for each combination
  results = subset(results,w<=W) # filter out combinations that exceed max weight
  max_value_index <- results$i[which.max(results$v)]
  value_max_comb <- all_combn_matrix[,max_value_index]
  max_value_elements <- y$i[which(value_max_comb==1)]
  return(list(value = round(max(results$v)),
              elements = max_value_elements))
}


