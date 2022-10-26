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
    ##     fun1() 350.6 542.10 901.820 589.95 741.00 20414.2   100
    ##  fun1alt()  21.2  24.45  57.067  27.40  33.75  2567.6   100

We can see that generating the matrix `fun1alt` is way faster than
generating by four numbers at a time `fun1()`

  
\### Find the column max with `max.col()`

``` r
# Data Generating Process
set.seed(1234)
M <- matrix(runif(12), ncol=4)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}
fun2(x=M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
fun2alt <- function(x) {
  idx <- max.col(t(x))
  x[cbind(idx, 1:4)]
}
fun2alt(x=M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
x <- matrix(rnorm(1e4), nrow=10)

# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x)
)
```

    ## Unit: microseconds
    ##        expr    min     lq     mean  median     uq    max neval
    ##     fun2(x) 1286.4 1370.3 1879.231 1641.40 2121.5 5340.6   100
    ##  fun2alt(x)  124.4  140.2  233.720  179.65  222.2 3192.9   100

We see that using `max.col()` is way faster than individually applying a
max function.  
