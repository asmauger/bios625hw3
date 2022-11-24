
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pca

<!-- badges: start -->

[![R-CMD-check](https://github.com/asmauger/bios625hw3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asmauger/bios625hw3/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/asmauger/bios625hw3/branch/main/graph/badge.svg)](https://app.codecov.io/gh/asmauger/bios625hw3?branch=main)
<!-- badges: end -->

The goal of pca is to provide a function for carrying out a principle
components analysis and two plotting functions to visualize the results
of the analysis.

## Installation

You can install the development version of pca from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asmauger/bios625hw3")
```

For detailed information about how to use this package, please see the
vignette and help pages. Below is a brief example that showcases the
three function of this package: `pca`, `elbowplot`, and `scoreplot`.

## Example analyzing mtcars data with PCA

The `mtcars` dataset contains observations on 11 variables for 32 cars.
We can use `pca` to find principal components and the variance explained
by each component.

``` r
library(pca)
data(mtcars)
cars_pca = pca(~mpg + disp + hp + drat + wt + qsec, data=mtcars)
summary(cars_pca)
#>                           PC1    PC2    PC3    PC4    PC5    PC6
#> Standard deviation     2.0463 1.0715 0.5774 0.3929 0.3533 0.2280
#> Proportion of Variance 0.6979 0.1914 0.0556 0.0257 0.0208 0.0087
#> Cumulative Proportion  0.6979 0.8893 0.9448 0.9705 0.9913 1.0000
```

An elbowplot (or screeplot) can help us visualize the information
provided in the summary from `pca`.

``` r
elbowplot(cars_pca)
```

<img src="man/figures/README-example-elbowplot-1.png" width="100%" />

This plot shows us that the first 3 components capture most of the
variation in the mtcars data.

A scoreplot allows us to visualize the mtcars data in just two
dimensions using the first two principal components.

``` r
library(ggplot2)
library(patchwork)
plot1 = scoreplot(cars_pca, grouping=mtcars$am) + labs(col='transmission')
plot2 = scoreplot(cars_pca, grouping=mtcars$cyl) + labs(col='cylinders')
plot1 + plot2
```

<img src="man/figures/README-example-scoreplot-1.png" width="100%" />

After coloring by the transmission type (automatic or manual), we see
that the second PC (y axis) separates the cars based on transmission
type. The plot coloring by number of cylinders shows us that the first
component separates the cars by number of cylinders.
