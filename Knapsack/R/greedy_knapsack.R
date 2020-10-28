#' greedy_knapsack
#'@description
#'The function gives an approximation for the problem by using George Dantzigs greedy approximation algorithm.
#' @param x a data frame with two variables, v (value) and w (weight).
#' @param W the knapsack size
#' @return Returns a list with two elements: the value of the knapsack and the items it consists of.
#' @export
#' @examples
#'n = 2000
#'knapsack_objects<-data.frame(w =sample(1: 4000,size= n,replace= TRUE),v =runif(n = n,0, 10000))
#'brute_force_knapsack(x = knapsack_objects [1: 12,],W = 3500)
#'brute_force_knapsack(x = knapsack_objects [1:12,],W = 2000)

greedy_knapsack <- function(x, W){
  x$item <- c(1:nrow(x)) #keeps track of the original item number
  x$ratio <- x$v/x$w
  newdata <- x[order(x$ratio, decreasing = TRUE),]
  cum_sum<-cumsum(newdata$w)
  S_2 = which(cum_sum>W)[1]
  S_1_items = c(1:(S_2-1))
  S_1_value = sum(newdata$v[S_1_items])
  S_2_value = newdata$v[S_2]
  if(S_1_value < S_2_value){
    value = S_2_value
    elements = newdata$item[S_2]

  }
  else{
    value = S_1_value
    elements = newdata$item[S_1_items]
  }
  result = list(value = value, elements = elements)
  return(result)
}
