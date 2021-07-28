
<!-- README.md is generated from README.Rmd. Please edit that file -->

# djprlabourdash

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/djpr-data/djprlabourdash/workflows/R-CMD-check/badge.svg)](https://github.com/djpr-data/djprlabourdash/actions)

<!-- badges: end -->

## Overview

The `djprlabourdash` repository contains the code for the DJPR Jobs
Dashboard. This dashboard is made in R with the [Shiny
package](https://shiny.rstudio.com).

The dashboard provides an overview of the Victorian labour market using
publicly-available data. It is created and maintained by the Data +
Analytics team in DJPR Strategy and Priority Projects. **Note that all
data currently displayed on the dashboard is publicly-available data.**

The [live dashboard is hosted at
Shinyapps.io](https://djpr-spp.shinyapps.io/djprlabourdash/).

`djprlabourdash` is structured as an R package. This facilitates easier
unit testing and code structure.

## Related repos

`djprlabourdash` depends on several related R packages:

-   [`djprdashdata`](https://github.com/djpr-data/djprdashdata) contains
    the data used in this app, as well as the code to update that data;

-   [`djprshiny`](https://github.com/djpr-data/djprshiny) contains
    common code used across DJPR Data + Analytics Shiny apps;

-   [`djprtheme`](https://github.com/djpr-data/djprtheme) is a ggplot2
    theme package that implements the DJPR visualisation style guide.

## Installation

Install `djprlabourdash` and its dependencies with:

``` r
# install.packages("devtools")
devtools::install_github("djpr-data/djprlabourdash", dependencies = TRUE)
```

## Git branch structure and deployment

There are three types of branches in this repository:

1.  The `main` branch contains the code corresponding to the current
    live dashboard. When any changes are made in `main`, an automatic
    process is triggered via GitHub Actions that re-deploys the Shiny
    web app - ie. it updates the live dashboard to reflect any changes
    made in `main`. Changes to `main` are almost always made via pull
    requests from `dev`. The only exception to this is urgent ‘hotfix’
    changes made by the project administrator.

2.  The `dev` branch is the main working branch for the repository. Code
    from project contributors is added in ‘feature branches’ which are
    then merged into `dev`, tested, then merged into `main`.

3.  Feature branches are created by project contributors to add or
    modify code. For example, a feature branch might be created to add a
    new function, or a range of new functions, to the code base.
    Branches are created off `dev` and are merged back into `dev`
    through a pull request when work is completed.

![](images/djprlabourdash%20git%20branches.png)
