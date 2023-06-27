
<!-- README.md is generated from README.Rmd. Please edit that file -->

# daltoolbox

<!-- badges: start -->

![GitHub Repo
stars](https://img.shields.io/github/stars/cefet-rj-dal/daltoolbox?logo=Github)
<!-- badges: end -->

The goal of DAL Toolbox is to provide a series data analytics functions
organized as a framework. It supports data preprocessing,
classification, regression, clustering, and time series prediction
functions.

## Installation

You can install the development version of dal from
[GitHub](https://github.com/) with:

``` r
library(devtools)
devtools::install_github("cefet-rj-dal/daltoolbox", force=TRUE, dependencies=FALSE, upgrade="never")
```

## Examples

The DAL Toolbox examples are made available at:
<https://nbviewer.org/github/cefet-rj-dal/daltoolbox-examples/tree/main/>

A demo video is provided at:
<https://eic.cefet-rj.br/~dal/leveraging-experiment-lines-to-data-analytics/>

The examples are organized according to general (data preprocessing),
clustering, classification, regression, and time series functions.

``` r
library(daltoolbox)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#> 
#> Attaching package: 'daltoolbox'
#> The following object is masked from 'package:base':
#> 
#>     transform
## loading DAL Toolbox
```
