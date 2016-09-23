
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
    -   [Time series analysis](#time-series-analysis)
    -   [Hooks to other packages](#hooks-to-other-packages)
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
#> Warning: package 'texreg' was built under R version 3.3.1
#> Warning: changing locked binding for 'coeftostring' in 'texreg' whilst
#> loading 'ResearchGroupTools'
#> Warning: changing locked binding for 'sanitize.numbers' in 'xtable' whilst
#> loading 'ResearchGroupTools'
```

By default, the seed for the random number generator is initialized to 0.

Functionality
=============

Library handling
----------------

-   `Library()` (note the capital "L") loads packages. If not available, these are automatically installed.

``` r
Library("texreg")
#> texreg
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
#> [1] "ab"
3 %+% 4
#> [1] "34"
do.call(`%+%`, as.list(letters))
#> [1] "abcdefghijklmnopqrstuvwxyz"
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

-   `completeLowResolutionData()` takes data in low resolution (e.g. monthly) and copies its values to match a high resolution (e.g. daily).

``` r
ts <- data.frame(Date = seq(from = as.Date("2000-01-01"), to = as.Date("2000-03-31"), by = "1 day"))
df_monthly <- data.frame(Month = c(as.Date("2000-01-31"), as.Date("2000-02-29"), as.Date("2000-03-31")),
                         Values = 1:3)

df_daily <- completeLowResolutionData(ts$Date, df_monthly, "Month")

# example of how to bind things together
ts <- ts %>%
  left_join(df_daily, by = c("Date" = "Month"))
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
#> Column names: \textbf{Mean} & \textbf{Median} & \textbf{Min.} & \textbf{Max} & \textbf{Std. dev.} & \textbf{Skewness} & \textbf{Excess kurtosis} \\
#>             mean median  min   max     sd   skew excess_kurtosis
#> Murder     7.788   7.25  0.8  17.4  4.356  0.371          -0.949
#> Assault  170.760 159.00 45.0 337.0 83.338  0.221          -1.145
#> UrbanPop  65.540  66.00 32.0  91.0 14.475 -0.213          -0.872
#> Rape      21.232  20.10  7.3  46.0  9.366  0.754           0.075
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
#> Column names: \textbf{Murder} & \textbf{Assault} & \textbf{UrbanPop} & \textbf{Rape} \\
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
df <- data.frame(x=rnorm(100)/1000, y=rnorm(100)/1000)
ggplot(df, aes(x=x, y=y)) +
  geom_point() +
  scale_x_continuous(labels=scientificLabels) +
  scale_y_continuous(labels=scientificLabels)
```

![](README-scientificLabels-1.png)

-   `allDigitsLabels()` enforces that all digits are displayed in **ggplot2** plots.

``` r
ggplot(df, aes(x=x, y=y)) +
  geom_point() +
  scale_x_continuous(labels=allDigitsLabels) +
  scale_y_continuous(labels=allDigitsLabels)
```

![](README-allDigitsLabels-1.png)

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

m_dummies <- regression(formula("y ~ x + dummies"), data = d, subset = 1:90,
                        dummies = "dummies", cutoff = 0.5)
#> Removing 2 observations; i.e. 0.02222222 percent.
#> Dropping 1 coefficients: dummies(Intercept)
summary(m_dummies)
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

-   `standardizeCoefficients()` extracts standardized coefficients and hides (dummy) variables if needed.

``` r
library(vars)
#> Warning: package 'vars' was built under R version 3.3.1
#> Loading required package: MASS
#> Warning: package 'MASS' was built under R version 3.3.1
#> 
#> Attaching package: 'MASS'
#> The following object is masked from 'package:dplyr':
#> 
#>     select
#> Loading required package: strucchange
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: sandwich
#> Loading required package: urca
#> Warning: package 'urca' was built under R version 3.3.1
#> Loading required package: lmtest
data(Canada)

prod <- differences(as.numeric(Canada[, 2]))
production <- data.frame(Prod = prod, Lag1 = dplyr::lag(prod), Lag2 = dplyr::lag(prod, 2))

m <- lm(Prod ~ Lag1, data = production)
standardizeCoefficients(m)
#>           Coef  SdChange StandardizedCoef
#> Lag1 0.2969171 0.2143436        0.2968569
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

-   `texreg_tvalues()` converts a the result of an ordinary least squares regression into in LaTeX. Instead of reporting standard errors, it gives t-values as a common alternative in finance. An optional parameter `dummies` can be specified which removes certain coefficients in the output.

``` r
texreg_tvalues(m_dummies)
#> 
#> \begin{table}
#> \begin{center}
#> \begin{tabular}{l c }
#> \hline
#>  & Model 1 \\
#> \hline
#> (Intercept) & $-0.27$      \\
#>             & $(-0.76)$    \\
#> x           & $1.00^{***}$ \\
#>             & $(267.14)$   \\
#> dummies     & $1.06^{***}$ \\
#>             & $(5.42)$     \\
#> \hline
#> R$^2$       & 1.00         \\
#> Adj. R$^2$  & 1.00         \\
#> Num. obs.   & 88           \\
#> RMSE        & 0.92         \\
#> \hline
#> \multicolumn{2}{l}{\scriptsize{$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$}}
#> \end{tabular}
#> \caption{Statistical models}
#> \label{table:coefficients}
#> \end{center}
#> \end{table}
texreg_tvalues(m_dummies, hide = "dummies")
#> 
#> \begin{table}
#> \begin{center}
#> \begin{tabular}{l c }
#> \hline
#>  & Model 1 \\
#> \hline
#> (Intercept) & $-0.27$      \\
#>             & $(0.35)$     \\
#> x           & $1.00^{***}$ \\
#>             & $(0.00)$     \\
#> dummies     & $1.06^{***}$ \\
#>             & $(0.20)$     \\
#> \hline
#> R$^2$       & 1.00         \\
#> Adj. R$^2$  & 1.00         \\
#> Num. obs.   & 88           \\
#> RMSE        & 0.92         \\
#> \hline
#> \multicolumn{2}{l}{\scriptsize{$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$}}
#> \end{tabular}
#> \caption{Statistical models}
#> \label{table:coefficients}
#> \end{center}
#> \end{table}
```

Time series analysis
--------------------

-   `standardizeCoefficients()` returns standardized coefficients.

``` r
var.2c <- VAR(Canada, p = 2, type = "none")

standardizeCoefficients(var.2c$varresult$e)
#>                Coef   SdChange StandardizedCoef
#> e.l1     1.62046761 14.4485612       1.60957876
#> prod.l1  0.17973134  0.7418013       0.08263712
#> rw.l1   -0.04425592 -0.9971380      -0.11108180
#> U.l1     0.11310425  0.1799192       0.02004311
#> e.l2    -0.64815156 -5.7426482      -0.63973460
#> prod.l2 -0.11683270 -0.4640806      -0.05169887
#> rw.l2    0.04475537  1.0337409       0.11515939
#> U.l2    -0.06581206 -0.1040299      -0.01158900

std <- standardizeCoefficients(var.2c)
std$e
#>                Coef   SdChange StandardizedCoef
#> e.l1     1.62046761 14.4485612       1.60957876
#> prod.l1  0.17973134  0.7418013       0.08263712
#> rw.l1   -0.04425592 -0.9971380      -0.11108180
#> U.l1     0.11310425  0.1799192       0.02004311
#> e.l2    -0.64815156 -5.7426482      -0.63973460
#> prod.l2 -0.11683270 -0.4640806      -0.05169887
#> rw.l2    0.04475537  1.0337409       0.11515939
#> U.l2    -0.06581206 -0.1040299      -0.01158900
```

-   `adf()` checks a time series for stationarity using the Augmented Dickey-Fuller (ADF) test. It returns the result in a pretty format and, if an optional argument `filename` is specified, it also exports it as LaTeX.

``` r
adf(USArrests, verbose = FALSE)
#> The following `from` values were not present in `x`: drift, trend
#> The following time series appear stationary, as P-values > 0.05:  Murder, Assault, UrbanPop, Rape
#>   Variable Type Lags   TestStat CriticalValue10 CriticalValue5
#> 1   Murder none    1 -1.7630314           -1.61          -1.95
#> 2  Assault none    1 -1.5869393           -1.61          -1.95
#> 3 UrbanPop none    1 -0.4833016           -1.61          -1.95
#> 4     Rape none    1 -1.7395692           -1.61          -1.95
#>   CriticalValue1     Pvalue
#> 1          -2.62 0.07789519
#> 2          -2.62 0.11252638
#> 3          -2.62 0.62888163
#> 4          -2.62 0.08193470
adf(USArrests, vars = c("Murder", "Rape"), type = "drift",
   filename = "adf.tex", verbose = FALSE)
#> The following `from` values were not present in `x`: trend
#> 
#> 
#> \begin{tabular}{ll SSSSS} 
#> \toprule 
#> \multicolumn{1}{l}{Variable} & \multicolumn{1}{l}{Deterministic trend} & \multicolumn{1}{c}{Lags}& \multicolumn{1}{c}{Test value} & \multicolumn{3}{c}{\textbf{Critical values}}\\ 
#> \cline{5-7} 
#> &&&& $10\,\%$ & $5\,\%$ & $1\,\%$ \\ 
#> 
#> 
#> All time series appear stationary, since all P-values < 0.05.
#>   Variable     Type Lags  TestStat CriticalValue10 CriticalValue5
#> 1   Murder Constant    1 -5.653178            -2.6          -2.93
#> 2     Rape Constant    1 -4.762830            -2.6          -2.93
#>   CriticalValue1       Pvalue
#> 1          -3.58 1.575081e-08
#> 2          -3.58 1.908966e-06
unlink("adf.tex")
```

-   `cointegrationTable()` performs a cointegration test following the Johansen procedure. The output is written as LaTeX into a file named `filename`.

``` r
cointegrationTable(USArrests, vars = c("Murder", "Rape"), K = 2, filename = "cointegration_eigen.tex")
#> Test statistic in the top row is larger than the 1% values:  All time-series variables are stationary, i.e. I(0), to start with. Cointegration is not relevant here. 
#> 
#> 
#> \begin{tabular}{l SSSS} 
#> \toprule 
#> \textbf{$H_{0}$} 
#> & \textbf{Test statistic} & \multicolumn{3}{c}{\textbf{Critical Values}}\\ 
#> \crule{3-5} 
#> & {$n = 2$}& {$10\,\%$} & {$5\,\%$} & {$1\,\%$} \\
#>     H0 TestStatistic CriticalValue10 CriticalValue5 CriticalValue1
#> 1  r=0         28.63           12.91           14.9          19.19
#> 2 r<=1        21.702             6.5           8.18          11.65
unlink("cointegration_eigen.tex")
```

-   `plotIrf()` returns a `ggplot` with a nice impulse response function in black/white.

``` r
plotIrf(irf, ylab = "Production")
```

![](README-plotIrf-1.png)

-   `impulseResponsePlot()` combines computation and plot, thereby returning a `ggplot` with a nice impulse response function in black/white.

``` r
impulseResponsePlot(var.2c, impulse = "e", response = "prod", ylab = "Production", n.ahead = 5)
```

![](README-impulseResponsePlot-1.png)

-   `testSpecification()` checks if non-autocorrelation, normally distributed residuals and homoskedasticity is present.

``` r
testSpecification(var.2c)
#> All specification tests seem fine. No autocorrelation, normally distributed residuals, homoskedasticity.
#>                           BG                PT               JB
#> Statistic   97.8554991662177  193.690038336454 4.37397926053634
#> Pvalue    0.0853032132233613 0.753481554600346 0.82190467653541
#> Stars                                                          
#>                        MARCH
#> Statistic   541.888980939688
#> Pvalue    0.0950466601495369
#> Stars
```

Hooks to other packages
-----------------------

-   `coeftostring()` from the `texreg` package is overwritten. This also fixes the behavior of `texreg()` itself.

``` r
coeftostring(-0.000001, digits = 4) # the original function would return "-.0000"
#> [1] ".0000"

d <- data.frame(y = 1:1000 - 0.0000001, x = 1:1000)
m <- lm(y ~ x, data = d)
texreg(m) # intercept would otherwise be "-0.00"
#> Warning in summary.lm(model, ...): essentially perfect fit: summary may be
#> unreliable
#> 
#> \begin{table}
#> \begin{center}
#> \begin{tabular}{l c }
#> \hline
#>  & Model 1 \\
#> \hline
#> (Intercept) & $0.00^{***}$ \\
#>             & $(0.00)$     \\
#> x           & $1.00^{***}$ \\
#>             & $(0.00)$     \\
#> \hline
#> R$^2$       & 1.00         \\
#> Adj. R$^2$  & 1.00         \\
#> Num. obs.   & 1000         \\
#> RMSE        & 0.00         \\
#> \hline
#> \multicolumn{2}{l}{\scriptsize{$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$}}
#> \end{tabular}
#> \caption{Statistical models}
#> \label{table:coefficients}
#> \end{center}
#> \end{table}
```

-   `sanitize.numbers()` inside `xtable` is overwritten.

``` r
xtable(matrix(1:4, nrow = 2) * -0.000001) # would otherwise return "-0.00"
#> % latex table generated in R 3.3.0 by xtable 1.8-2 package
#> % Fri Sep 23 12:06:54 2016
#> \begin{table}[ht]
#> \centering
#> \begin{tabular}{rrr}
#>   \hline
#>  & 1 & 2 \\ 
#>   \hline
#> 1 & 0.00 & 0.00 \\ 
#>   2 & 0.00 & 0.00 \\ 
#>    \hline
#> \end{tabular}
#> \end{table}
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
