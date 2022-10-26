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
    ##       expr   min    lq    mean median     uq     max neval
    ##     fun1() 352.0 526.4 909.827 588.45 757.55 26928.3   100
    ##  fun1alt()  21.5  25.1  61.477  28.30  35.25  2827.0   100

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
    ##        expr    min      lq     mean  median      uq    max neval
    ##     fun2(x) 1266.7 1386.15 1810.626 1547.05 1944.65 4525.3   100
    ##  fun2alt(x)  122.6  132.95  203.119  145.20  186.20 3615.6   100

We see that using `max.col()` is way faster than individually applying a
max function.  

# Problem 3: Parallelize everything

Resampling from the data set with bootstrapping (resampling with
replacement)  
This is the setup for the bootstrapping function:

``` r
my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  
  # STEP 1: 
  cl <- makePSOCKcluster(4)
  clusterSetRNGStream(cl, 123) # same as 'set.seed(123)'
  
  # STEP 2:
  clusterExport(cl, c("stat", "dat", "idx"), envir = environment())
  
  # STEP 3: THIS FUNCTION NEEDS TO BE REPLACED WITH parLapply
  ans <- parLapply(cl,seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  
  ans
  
}
```

  
\### 1. Use the bootstrapping code to make it work in parallel

``` r
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))

# DATA SIM
set.seed(1)
n <- 500; R <- 1e4

x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)

# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)

# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1386903 0.04856752
    ## x            4.8685162 5.04351239

``` r
ans0
```

    ##                  2.5 %     97.5 %
    ## (Intercept) -0.1379033 0.04797344
    ## x            4.8650100 5.04883353

### 2. Check whether this version is faster than the non-parallel version:

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))
```

    ##    user  system elapsed 
    ##    0.26    0.18    4.98

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))
```

    ##    user  system elapsed 
    ##    0.28    0.10    5.23
