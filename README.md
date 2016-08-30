
-   [ResearchGroupTools](#researchgrouptools)
    -   [Installation](#installation)
    -   [Usage](#usage)
-   [Functionality](#functionality)
    -   [Library handling](#library-handling)
    -   [Strings](#strings)
    -   [Numerical functions](#numerical-functions)
    -   [Data handling](#data-handling)
    -   [Time series](#time-series)
    -   [Matrix functions (or data.frame)](#matrix-functions-or-data.frame)
    -   [Descriptive statistics](#descriptive-statistics)
    -   [Visualization](#visualization)
    -   [Regressions](#regressions)
    -   [Package development](#package-development)

<!-- README.md is generated from README.Rmd. Please edit that file -->
ResearchGroupTools
==================

[![Build Status](https://travis-ci.org/sfeuerriegel/ResearchGroupTools.svg?branch=master)](https://travis-ci.org/sfeuerriegel/ResearchGroupTools) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ResearchGroupTools)](https://cran.r-project.org/package=ResearchGroupTools) [![Coverage Status](https://img.shields.io/codecov/c/github/sfeuerriegel/ResearchGroupTools/master.svg)](https://codecov.io/github/sfeuerriegel/ResearchGroupTools?branch=master)

**ResearchGroupTools** provides a collection of utilitiy function for rapid prototyping. These functions facilitate implemenation works related to advanced analytics. As such, it specifically supports data handling, preprocessing, visualization and analytics.

Installation
------------

Using the **devtools** package, you can easily install the latest development version of **SentimentAnalysis** with

``` r
install.packages("devtools")

# Option 1: download and install latest version from "GitHub"
devtools::install_github("sfeuerriegel/ResearchGroupTools")

# Option 2: install directly from bundled archive
# devtoos::install_local("ResearchGroupTools_0.2.0.tar.gz")
```

Notes:

-   In the case of option 2, you have to specify the path either to the directory of **ResearchGroupTools** or to the bundled archive **ResearchGroupTools\_0.2.0.tar.gz**

-   The package will only be shipped via GitHub; CRAN support is not intended.

Usage
-----

This section shows the basic functionality of how to perform a sentiment analysis. First, load the corresponding package **ResearchGroupTools**.

``` r
library(ResearchGroupTools)
```

By default, the seed for the random number generator is initialized to 0.

Functionality
=============

Library handling
----------------

-   `Library()` (note the capital "L") loads packages. If not available, these are automatically installed.

``` r
Library("ggplot2", "dplyr")
#> ggplot2
#> dplyr
```

-   `loadRegressionLibraries()` loads and installs common libraries for econometric purposes.

``` r
loadRegressionLibraries()
```

Strings
-------

-   `%+%` concatenates strings (as an alterantive to `paste()`).

``` r
"a" %+% "b"
3 %+% 4
`%+%`(letters)
```

Numerical functions
-------------------

-   `ceil()` computes the largest integer less or equal given a numerical value. It is a wrapper for `ceiling` with a more consistent naming.

``` r
ceil(3.4)
#> [1] 4
```

Data handling
-------------

-   `pull()`, `pull_string()` and `pull_ith()` extract single columns from a **dplyr** `tbl` object and return them as a vector.

``` r
library(dplyr)

d <- data_frame(x = 1:10,
               y = rnorm(10))
d %>% pull(x)
#>  [1]  1  2  3  4  5  6  7  8  9 10
d %>% pull("x")
#>  [1]  1  2  3  4  5  6  7  8  9 10

v <- "x"
d %>% pull_string(v)
#>  [1]  1  2  3  4  5  6  7  8  9 10

d %>% pull_ith(1)
#>  [1]  1  2  3  4  5  6  7  8  9 10
```

Time series
-----------

-   `differences()` calculates lagged differences of a given order. It is more convenient thant `diff()` as it adds leading `NA` values.

``` r
differences(1:10)
#>  [1] NA  1  1  1  1  1  1  1  1  1
differences(c(1, 2, 4, 8, 16, 32))
#> [1] NA  1  2  4  8 16
differences(c(1, 2, 4, 8, 16, 32), order = 2)
#> [1] NA NA  1  2  4  8
differences(c(1, 2, 4, 8, 16, 32), na_padding = FALSE)
#> [1]  1  2  4  8 16
```

-   `returns()` calculates returns of a time series (similar to `diff()` for differenes).

``` r
returns(1:10)
#>  [1]        NA 1.0000000 0.5000000 0.3333333 0.2500000 0.2000000 0.1666667
#>  [8] 0.1428571 0.1250000 0.1111111
returns(c(1, 2, 4, 8, 16, 32))
#> [1] NA  1  1  1  1  1
returns(c(1, 2, 4, 8, 16, 32), na_padding = FALSE) # remove trailing NA's
#> [1] 1 1 1 1 1
```

-   `logReturns()` computes log-returns (by default, with base `exp(1)`).

``` r
logReturns(c(1, 2, 4, 8, 16, 32), base = 2)
#> [1] NA  1  1  1  1  1
```

Matrix functions (or data.frame)
--------------------------------

-   `findRowsNA()` and `showRowsNA()`, as well as `findColsNA()` and `showColsNA()`, help find `NA` values within a dataset.

``` r
m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)], ncol = 2, byrow = FALSE)
colnames(m) <- c("x", "y")
m
#>      x   y  
#> [1,] "a" "d"
#> [2,] "b" "e"
#> [3,] NA  "f"
#> [4,] "c" "g"
#> [5,] NA  "h"

anyNA(m)      # use built-in routine to test for NA values
#> [1] TRUE

findRowsNA(m) # returns indices of that rows
#> [1] 3 5
showRowsNA(m) # prints rows with NA values
#>      x  y  
#> [1,] NA "f"
#> [2,] NA "h"

findColsNA(m) # returns name of that columns
#> [1] "x"
showColsNA(m) # print columns with NA values
#> [1] "a" "b" NA  "c" NA
```

Descriptive statistics
----------------------

-   `descriptiveStatistics()` produces **pretty** summary statistics. By default, it exports the statistics into a LaTeX file. An optional parameter `filename` can be used to change the filename for the export.

``` r
data(USArrests)
descriptiveStatistics(USArrests)
#>             mean median  min   max     sd   skew kurtosis
#> Murder     7.788   7.25  0.8  17.4  4.356  0.371   -0.949
#> Assault  170.760 159.00 45.0 337.0 83.338  0.221   -1.145
#> UrbanPop  65.540  66.00 32.0  91.0 14.475 -0.213   -0.872
#> Rape      21.232  20.10  7.3  46.0  9.366  0.754    0.075
unlink("table_descriptives.tex")
```

-   `correlationMatrix()` computes a **pretty** correlation matrix. An optional parameter `filename` can be used to specify a LaTeX file to which the result is exported with significance stars.

``` r
correlationMatrix(USArrests)
#>            Murder  Assault UrbanPop Rape
#> Murder                                  
#> Assault  0.802***                       
#> UrbanPop    0.070    0.259              
#> Rape     0.564*** 0.665***  0.411**
correlationMatrix(USArrests, filename = "table_cor.tex") # stores output in LaTeX file
#>            Murder  Assault UrbanPop Rape
#> Murder                                  
#> Assault  0.802***                       
#> UrbanPop    0.070    0.259              
#> Rape     0.564*** 0.665***  0.411**
unlink("table_cor.tex")
```

This requires a few changes to your LaTeX document in order to get it running. The steps are documented in the help of ; below is a minimal working example:

``` r
\documentclass{article}
\usepackage{SIunitx}
  \newcommand{\sym}[1]{\rlap{$^{#1}$}}
  \siunitx{input-symbols={()*}}
\begin{document}

\begin{tabular}{l SSS}
\toprule
\include{table_cor}
\end{tabular}
\end{document}
```

Above, we included `SIunitx`, introduced a command `\sym`, changed the `input-symbols` and used custom column alignments (`S`).

Visualization
-------------

-   `linePlot()` is a simple wrapper to **ggplot2**.

``` r
linePlot(1:10)
```

![](README-linePlot-1.png)

``` r

x <- seq(0, 4, length.out = 100)
linePlot(x, sin(x))
```

![](README-linePlot-2.png)

-   `scientificLabels()` enables a nice exponential notation in **ggplot2** plots.

``` r
library(ggplot2)
df <- data.frame(x=rnorm(100), y=rnorm(100))
ggplot(df, aes(x=x, y=y)) +
  geom_point() +
  scale_x_continuous(labels=scientificLabels) +
  scale_y_continuous(labels=scientificLabels)
```

![](README-scientific_labels-1.png)

Regressions
-----------

-   `makeFormula()` lets one build formulae based on strings to identify the individual variables.

``` r
makeFormula("y", "x")
#> y ~ x
makeFormula("y", c("x1", "x2", "x3"))
#> y ~ x1 + x2 + x3
makeFormula("y", c("x1", "x2", "x3"), "dummies")
#> y ~ x1 + x2 + x3 + dummies
```

-   `regression()` is a customized, all-in-one routine for ordinary least squares with optional dummy variables. It can filter for a subset of observations, remove outliers at a certain cutoff and remove dummies that are `NA`.

``` r
x <- 1:100
clusters <- rep(c(1, 2), 50)
dummies <- model.matrix(~ clusters)
y <- x + clusters + rnorm(100)
d <- data.frame(x = x, y = y)

m <- regression(formula("y ~ x + dummies"), data = d, subset = 1:90,
                dummies = "dummies", cutoff = 0.5)
#> Removing 2 observations; i.e. 0.02222222 percent.
#> Dropping 1 coefficients: dummies(Intercept)
summary(m)
#> 
#> Call:
#> lm(formula = formula, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.35458 -0.47758  0.03748  0.67710  1.89330 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -0.265295   0.348944  -0.760    0.449    
#> x            1.003313   0.003756 267.141  < 2e-16 ***
#> dummies      1.061065   0.195939   5.415 5.59e-07 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.9186 on 85 degrees of freedom
#> Multiple R-squared:  0.9988, Adjusted R-squared:  0.9988 
#> F-statistic: 3.575e+04 on 2 and 85 DF,  p-value: < 2.2e-16
```

-   `showCoeftest()` shows coefficient tests, but hides (dummy) variables starting with a certain string.

``` r
x1 <- 1:100
x2 <- rep(c(1, 2), 50)
y <- x1 + x2 + rnorm(100)

m <- lm(y ~ x1 + x2)

showCoeftest(m, hide = "x") # leaves only the intercept
#>               Estimate Std..Error    t.value  Pr...t.. Stars
#> (Intercept) -0.2788929  0.3515728 -0.7932721 0.4295555
```

-   `extractRegressionStatistics()` extracts key statistics of regression and returns them as a `data.frame` (so that it can later be stacked via row-wise binding).

``` r
x <- 1:10
y <- 1 + x + rnorm(10)
m <- lm(y ~ x)
 
extractRegressionStatistics(m)
#>   Observations DegreesFreedom ResidualError  Rsquared AdjRsquared      AIC
#> 1           10              8      1.043377 0.9083956    0.896945 32.99659
#>        BIC Fstatistic Fsignficance Fstars
#> 1 33.90435   79.33203 2.000286e-05    ***
```

-   `getRowsOutlierRemoval()` helps to remove outliers at the 0.5% level at both ends (or any other threshold defined by the argument `cutoff`).

``` r
 d <- data.frame(x = 1:200, y = 1:200 + rnorm(200))
m <- lm(y ~ x, d)                  # fit original model

idx_rm <- getRowsOutlierRemoval(m) # identify row indices of outliers
m <- lm(y ~ x, d[-idx_rm, ])       # refit model with outliers removed
```

Package development
-------------------

-   `rebuildPackage()` builds, loads and checks package during the development process all at once. In particular, the manual is updated.

``` r
rebuildPackage()
rebuildPackage(TRUE) # also runs README.Rmd
```

### License

**ResearchGroupTools** is released under the [MIT License](https://opensource.org/licenses/MIT)

Copyright (c) 2016 Stefan Feuerriegel
