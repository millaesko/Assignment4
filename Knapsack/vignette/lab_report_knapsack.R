## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Knapsack)

## -----------------------------------------------------------------------------
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(w = sample(1: 4000, size = n, replace = TRUE),v = runif(n = n, 0, 10000))

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
system.time(brute_force_knapsack(x = knapsack_objects [1: 16,],W = 2000))

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
system.time(brute_force_knapsack(x = knapsack_objects [1: 16,],W = 4000))

## -----------------------------------------------------------------------------
set.seed(42)
n <- 1000000
knapsack_objects <- data.frame(w = sample(1: 4000, size = n, replace = TRUE),v = runif(n = n, 0, 10000))
system.time(greedy_knapsack(x = knapsack_objects,W = 2000))

