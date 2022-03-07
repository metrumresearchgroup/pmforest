
<!-- README.md is generated from README.Rmd. Please edit that file -->

<br>

# pmforest

<!-- # pmforest <a href='https:/metrumresearchgroup.github.io/pmforest'><img src='man/figures/logo.png' align="right" height="120" /></a> -->
<!-- badges: start -->

[![Build
Status](https://github-drone.metrumrg.com/api/badges/metrumresearchgroup/pmforest/status.svg)](https://github-drone.metrumrg.com/metrumresearchgroup/pmforest)
[![codecov](https://codecov.io/gh/metrumresearchgroup/pmforest/branch/main/graph/badge.svg)](https://codecov.io/gh/metrumresearchgroup/pmforest)
<!-- badges: end -->

## Installation

You can install the latest released version of `pmforest` via [MPN
snapshots](https://mpn.metworx.com/docs/snapshots/) from any snapshot
date in March 2022 or later.

You can install the latest development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("metrumresearchgroup/pmforest", ref = "main")
```

## Documentation

You can find documentation and a “Getting Started” vignette that shows
users how to set up and summarize their data, as well as demonstrates
the basic plotting methods. Additional formatting attributes are
highlighted as well.

### Featured Vignettes

-   [Getting Started with
    bbr](https://metrumresearchgroup.github.io/pmforest/articles/getting-started.html)
    – Data specifications, summarization, and basic plotting methods.
-   [Multiple
    Simulations](https://metrumresearchgroup.github.io/pmforest/articles/multiple-simulations.html)
    – Plotting additional confidence intervals over the ‘replicate’ or
    simulation run.

## Development

`pmforest` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to
manage development dependencies and
[renv](https://rstudio.github.io/renv/) to provide isolation. To
replicate this environment,

1.  clone the repo

2.  install pkgr

3.  open package in an R session and run `renv::init()`

    -   install `renv` &gt; 0.8.3-4 into default `.libPaths()` if not
        already installed

4.  run `pkgr install` in terminal within package directory

5.  restart session

Then, launch R with the repo as the working directory (open the project
in RStudio). renv will activate and find the project library.
