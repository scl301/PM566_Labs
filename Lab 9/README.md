Lab 9
================
Stephanie Lee
2022-10-26

# Problem 1

Three examples of problems that may be solved using parallel
computing 1. Can run multiple tests at once - multtest package 2.
Running multiple simulations at once

# Problem 2

The following functions can be written to be more efficient without
using parallel:  

### 1. This function generate a `n x k` dataset with all its entries distributed poisson with mean `lambda`

``` r
set.seed(1235)

# This function creates 100 vectors of length 4
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}

# Create one matrix instead of individual vectors
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  
  x <- matrix(rpois(n*k,lambda), ncol=4)
  
  return(x)
}

# Benchmarking
microbenchmark::microbenchmark(
  fun1(),
  fun1alt()
)
```

    ## Unit: microseconds
    ##       expr   min     lq    mean median     uq     max neval
    ##     fun1() 351.7 513.30 892.264 586.90 631.60 29243.1   100
    ##  fun1alt()  21.4  24.25  62.712  27.65  33.85  3211.0   100

We can see that generating the matrix `fun1alt` is way faster than
generating by four numbers at a time `fun1()`
