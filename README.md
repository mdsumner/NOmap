
[![Travis build
status](https://travis-ci.org/mdsumner/NOmap.svg?branch=master)](https://travis-ci.org/mdsumner/NOmap)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mdsumner/NOmap?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/NOmap)
[![Coverage
status](https://codecov.io/gh/mdsumner/NOmap/branch/master/graph/badge.svg)](https://codecov.io/github/mdsumner/NOmap?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/NOmap)](https://cran.r-project.org/package=NOmap)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# NOmap

The goal of NOmap is to make publication quality round Northern Ocean
maps in polar projections with little effort. This package is COMPLETELY
WIP AND WILL MAKE YOUR LIFE VERY BAD, DO NOT USE.

``` r
## see, you'll regret this
library(NOmap)
#> Loading required package: raster
#> Loading required package: sp
#> Warning: S3 method 'print.SOmap' was declared in NAMESPACE but not found
nmap <- NOmap(land = F)
#> Loading required namespace: rgeos
data("wrld_simpl", package = "maptools")
wrld_simpl <- subset(wrld_simpl, !NAME %in% "Antarctica")
plot(nmap)
NOmap::SOplot(wrld_simpl, col = grey(0.6, alpha = 0.3))
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

Please note that the NOmap project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.
