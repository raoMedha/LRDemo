
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LRDemo

<!-- badges: start -->

<!-- badges: end -->

The goal of LRDemo is to implement a binary or multinomial logistic
regression via gradient descent. Helper functions are carried out in
C++/ Rcpp for optimized computation.

## Installation

You can install the development version of LRDemo like so:

``` r
# install.packages("devtools")
devtools::install_github("YOUR_GITHUB_USERNAME/LRDemo")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LRDemo)

set.seed(1234567)
x <- matrix(rnorm(200), ncol = 2)
p <- 1 / (1 + exp(-(x[, 1] - x[, 2])))
y <- rbinom(nrow(x), size = 1, prob = p)

fit  <- logRegModel(x, y, family = "binomial", nIter = 500)
phat <- predict(fit, x, type = "response")
mean((phat > 0.5) == y)
#> [1] 0.72
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
