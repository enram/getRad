
<!-- README.md is generated from README.Rmd. Please edit that file -->

# getRad

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/getRad)](https://CRAN.R-project.org/package=getRad)
[![R-CMD-check](https://github.com/aloftdata/getRad/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aloftdata/getRad/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/aloftdata/getRad/branch/main/graph/badge.svg)](https://app.codecov.io/gh/aloftdata/getRad/)
[![repo
status](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The goal of `getRad` is to provide a unified interface to radar data for
biological research. This is done by downloading data from repositories
and loading it directly into R. Currently the functionality if focused
around volume data from weather radars. However in the longer term it
might also support vertical profile information, vertically integrated
profile information and possibly data from other radars.

## Installation

You can install the development version of `getRad` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aloftdata/getRad")
```

For the time being the package is not yet released on CRAN.

## Usage

Here are some examples of volume data with biological information that
can be downloaded

``` r
library(getRad)
library(bioRad)
#> Welcome to bioRad version 0.8.1
#> Attempting to load MistNet from: /home/bart/R/x86_64-pc-linux-gnu-library/4.4/vol2birdR/lib 
#> MistNet successfully initialized.
#> using vol2birdR version 1.0.2 (MistNet installed)
# Plot insect movements in Finland (Mäkinen et al. 2022)
pvol<-get_pvol("fianj", as.POSIXct("2012-5-17 14:15", tz="UTC"))
plot(project_as_ppi(get_scan(pvol,0), range_max = 75000))
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

# Spring migration in Estonia
pvol<-get_pvol("nlhrw", as.POSIXct("2023-3-19 22:15", tz="UTC+1"))
plot(calculate_vp(pvol, h_layer = 50, n_layer = 40))
#> Running vol2birdSetUp
#> Warning: radial velocities will be dealiased...
```

<img src="man/figures/README-example-2.png" width="100%" />

## Meta

- We welcome
  [contributions](https://aloftdata.github.io/getRad/CONTRIBUTING.html)
  including bug reports.
- License: MIT
- Get [citation
  information](https://aloftdata.github.io/getRad/authors.html#citation) for
  getRad in R doing `citation("getRad")`.
- Please note that this project is released with a [Contributor Code of
  Conduct](https://aloftdata.github.io/getRad/CODE_OF_CONDUCT.html). By
  participating in this project you agree to abide by its terms.
