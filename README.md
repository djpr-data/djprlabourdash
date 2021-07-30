
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
    live dashboard.

2.  The `dev` branch is the main working branch for the repository. This
    is where contributions by different people are integrated and tested
    before being deployed to the live dashboard.

3.  Feature branches are created by project contributors to add or
    modify the dashboard’s functionality.

![](images/djprlabourdash%20git%20branches.svg)

When any changes are made in `main`, an automatic process is triggered
via GitHub Actions that re-deploys the Shiny web app - ie. the live
dashboard is updated to reflect any changes made in `main`. Changes to
`main` are almost always made via pull requests from `dev`. The only
exception to this are urgent ‘hotfix’ changes made by the project
administrator.

Code from project contributors is always added in ‘feature branches’
which are then merged into `dev`, tested, then merged into `main`.

## Folder structure and key files

Because `djprlabourdash` is structured as an R package, the folder
structure reflects R package conventions. These include:

-   All functions in the package live in files ending in `.R` in the
    `/R` subfolder of the package.

-   Documentation for individual functions lives in `.Rd` files in the
    `/man` subfolder of the package. These are created automatically
    based on [special comments in .R
    files](https://r-pkgs.org/man.html?q=roxy#roxygen-comments) and
    should not be edited directly.

-   Code to create data used by the package (‘internal data’) is saved
    in `/data-raw`. This folder also contains some code (‘scratchpads’)
    that is used to test certain functions and can ultimately be
    discarded.

-   Tests that ensure functions do what they are expected to do are
    saved in `/tests`.

There are some other files and folders of note:

-   [DESCRIPTION](https://r-pkgs.org/description.html#pkg-description)
    is a file included in all R packages that descriptions what the
    package does, who created it, and what other packages the package
    depends on (‘imports’). Package contributors should generally not
    edit this file directly - if an additional package needs to be
    added, use `usethis::use_package("packagename")`.

-   [NAMESPACE](https://r-pkgs.org/namespace.html?q=NAMESPACE#namespace)
    is another file included in all R packages. It tells R which
    functions from this package should be made available to users
    (“exported”) and which functions from other packages should be
    available to this package (“imported”). It shouldn’t be directly
    edited.

-   `/.github` contains code for GitHub to execute using [Github
    Actions](https://docs.github.com/en/actions). This includes
    instructions to check the package (using [R CMD
    check](https://r-pkgs.org/r-cmd-check.html)) to make sure that it
    passes all the built-in tests and does what an R package is expected
    to do - this is done whenever anyone initiates a pull request to the
    `main` or `dev` branches. It also includes instructions to deploy
    the dashboard to shinyapps.io whenever anything is changed in the
    `main` branch. Package contributors should not edit these files.

-   `/rsconnect` contains information used as part of the deployment of
    the dashboard to shinyapps.io. These files are created automatically
    and should not be directly edited.

## Code structure

As noted above, all functions live in the `/R` folder of the package.
The key files for contributors to be aware of are:

-   Files starting with `viz_` contain functions to make the graphs on
    the dashboard.

-   Files starting with `page_` define the user interface for individual
    pages of the dashboard.

-   The `labour_app.R` file contains the server-side code for the Shiny
    app.

There are some other files related to tables and utility functions.

## Dashboard data

All data in the dashboard is publicly available. The [`djprdashdata`
repository](https://github.com/djpr-data/djprdashdata) extracts this
data from the ABS, minimally transforms it, and stores it. See
[`djprdashdata`](https://github.com/djpr-data/djprdashdata) for further
information about data extraction and storage.

### Loading dashboard data

Data is loaded into the dashboard using the function
`djprlabourdash::load_dash_data()`. This function is executed once at
the start of each dashboard session, ensuring any upstream change in the
data is (nearly) immediately reflected on the dashboard itself.

In interactive sessions where a contributor is working with the
dashboard data, the following command should be run at the beginning of
the session:

``` r
# First, run `load_all()` to ensure that this package's functions are available
# devtools::load_all()
library(djprshiny)
dash_data <- load_dash_data()
```

Note that `dash_data` is a data frame (tibble) with a nested structure.
Each row of the tibble corresponds to an ABS series ID. The `data`
column of the tibble contains a list of tibbles that must be unnested
prior to use.

``` r
dash_data
#> # A tibble: 784 × 25
#>    series_id        data age   gcc_restofstate indicator sex   industry duration
#>    <chr>       <list<ti> <chr> <chr>           <chr>     <chr> <chr>    <chr>   
#>  1 15-24_empl… [273 × 8] 15-24 Australian Cap… Employed  ""    ""       ""      
#>  2 15-24_empl… [273 × 8] 15-24 Greater Adelai… Employed  ""    ""       ""      
#>  3 15-24_empl… [273 × 8] 15-24 Greater Brisba… Employed  ""    ""       ""      
#>  4 15-24_empl… [273 × 8] 15-24 Greater Darwin  Employed  ""    ""       ""      
#>  5 15-24_empl… [273 × 8] 15-24 Greater Hobart  Employed  ""    ""       ""      
#>  6 15-24_empl… [273 × 8] 15-24 Greater Melbou… Employed  ""    ""       ""      
#>  7 15-24_empl… [273 × 8] 15-24 Greater Perth   Employed  ""    ""       ""      
#>  8 15-24_empl… [273 × 8] 15-24 Greater Sydney  Employed  ""    ""       ""      
#>  9 15-24_empl… [273 × 8] 15-24 Rest of NSW     Employed  ""    ""       ""      
#> 10 15-24_empl… [273 × 8] 15-24 Rest of NT      Employed  ""    ""       ""      
#> # … with 774 more rows, and 17 more variables: state <chr>, current_lfs <chr>,
#> #   dependents <chr>, duration_of_unemp <chr>, duration_with_employer <chr>,
#> #   education_attendance <chr>, exp_future_emp <chr>, highest_ed <chr>,
#> #   hours <chr>, industry_subdiv <chr>, marital_status <chr>,
#> #   market_nonmarket <chr>, occupation <chr>, relship_in_hhold <chr>,
#> #   sa4 <chr>, sector <chr>, status_in_emp <chr>
```

### Filtering dashboard data

Users generally work with the `dash_data` object using the
`filter_dash_data()` function. This function takes a vector of ABS time
series ID(s) and returned a tibble containing the corresponding data.
This output is unnested for the user, meaning that project contributors
can generally ignore the nested nature of `dash_data`.

A lookup table of series IDs is available from the `djprdashdata`
package:

``` r
djprdashdata::lfs_lookup
#> # A tibble: 22,171 × 28
#>    age   gcc_restofstate  indicator series_id series series_type table_no cat_no
#>    <chr> <chr>            <chr>     <chr>     <chr>  <chr>       <chr>    <chr> 
#>  1 15-24 Australian Capi… Employed  15-24_em… 15-24… Original    RM1      6291.…
#>  2 15-24 Australian Capi… NILF      15-24_ni… 15-24… Original    RM1      6291.…
#>  3 15-24 Australian Capi… Unemploy… 15-24_un… 15-24… Original    RM1      6291.…
#>  4 15-24 Greater Adelaide Employed  15-24_em… 15-24… Original    RM1      6291.…
#>  5 15-24 Greater Adelaide NILF      15-24_ni… 15-24… Original    RM1      6291.…
#>  6 15-24 Greater Adelaide Unemploy… 15-24_un… 15-24… Original    RM1      6291.…
#>  7 15-24 Greater Brisbane Employed  15-24_em… 15-24… Original    RM1      6291.…
#>  8 15-24 Greater Brisbane NILF      15-24_ni… 15-24… Original    RM1      6291.…
#>  9 15-24 Greater Brisbane Unemploy… 15-24_un… 15-24… Original    RM1      6291.…
#> 10 15-24 Greater Darwin   Employed  15-24_em… 15-24… Original    RM1      6291.…
#> # … with 22,161 more rows, and 20 more variables: sex <chr>, industry <chr>,
#> #   duration <chr>, state <chr>, current_lfs <chr>, dependents <chr>,
#> #   duration_of_unemp <chr>, duration_with_employer <chr>,
#> #   education_attendance <chr>, exp_future_emp <chr>, highest_ed <chr>,
#> #   hours <chr>, industry_subdiv <chr>, marital_status <chr>,
#> #   market_nonmarket <chr>, occupation <chr>, relship_in_hhold <chr>,
#> #   sa4 <chr>, sector <chr>, status_in_emp <chr>
```

If a user wanted to get a data frame with the number of unemployed
people in Bendigo each month from 1998 to present, they would use
`filter_dash_data()` as follows:

``` r
bendigo_unemp <- filter_dash_data("A84600028X")
bendigo_unemp
#> # A tibble: 273 × 32
#>    series_id  date       value series   series_type table_no data_type frequency
#>    <chr>      <date>     <dbl> <chr>    <chr>       <chr>    <chr>     <chr>    
#>  1 A84600028X 1998-10-01  5.01 >>> Ben… Original    6291016  STOCK     Month    
#>  2 A84600028X 1998-11-01  6.66 >>> Ben… Original    6291016  STOCK     Month    
#>  3 A84600028X 1998-12-01  6.36 >>> Ben… Original    6291016  STOCK     Month    
#>  4 A84600028X 1999-01-01  6.56 >>> Ben… Original    6291016  STOCK     Month    
#>  5 A84600028X 1999-02-01  6.76 >>> Ben… Original    6291016  STOCK     Month    
#>  6 A84600028X 1999-03-01  5.47 >>> Ben… Original    6291016  STOCK     Month    
#>  7 A84600028X 1999-04-01  5.15 >>> Ben… Original    6291016  STOCK     Month    
#>  8 A84600028X 1999-05-01  6.20 >>> Ben… Original    6291016  STOCK     Month    
#>  9 A84600028X 1999-06-01  3.73 >>> Ben… Original    6291016  STOCK     Month    
#> 10 A84600028X 1999-07-01  2.74 >>> Ben… Original    6291016  STOCK     Month    
#> # … with 263 more rows, and 24 more variables: unit <chr>, age <chr>,
#> #   gcc_restofstate <chr>, indicator <chr>, sex <chr>, industry <chr>,
#> #   duration <chr>, state <chr>, current_lfs <chr>, dependents <chr>,
#> #   duration_of_unemp <chr>, duration_with_employer <chr>,
#> #   education_attendance <chr>, exp_future_emp <chr>, highest_ed <chr>,
#> #   hours <chr>, industry_subdiv <chr>, marital_status <chr>,
#> #   market_nonmarket <chr>, occupation <chr>, relship_in_hhold <chr>,
#> #   sa4 <chr>, sector <chr>, status_in_emp <chr>
```

It is then straightforward to visualise this. We will use the
`djprshiny::djpr_ts_linechart()` convenience function, which creates a
ggplot2 object that generally meets our style guide:

``` r
bendigo_unemp %>%
  djpr_ts_linechart()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## `viz_` files

Most contributions to the dashboard will come in the `viz_` files. These
contain the code that creates the graphs on the dashboard.

Generally, each graph has its own function – the only exception are some
functions that produce multiple related graphs that are combined with
`{patchwork}`.

### Naming graph functions

Graph functions all **must start with `viz_`**. The general naming
convention is:

`viz_page_content_charttype()`

where:

-   `page` is an abbreviated version of the page name - like `ind` for
    the Indicators page;

-   `content` provides some indication of the subject of the chart, like
    `partrate` for a chart of the participation rate; and

-   `charttype` indicates the type of chart, such as `bar` for a bar
    chart.

So the `viz_ind_partrate_bar()` function, for example, produces this
chart:

``` r
djprlabourdash:::viz_ind_partrate_bar()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

### Graph functions’ arguments

**Each `viz_` function must have a first argument called `data`**. It
can optionally have other arguments. **All arguments to `viz_` functions
must have defaults supplied.**

Generally, the default to the `data` argument will involve a call to
`filter_dash_data()`. This function takes a vector of ABS time series
ID(s) and returns a dataframe containing the data corresponding to those
IDs, if it is present in the data loaded into the dashboard.

These are examples of **good `viz_` functions** that satisfy these
requirements:

``` r
# This is good
viz_fake_function <- function(data = filter_dash_data("A84600028X")) {
  data %>%
    djpr_ts_linechart()
}

# This is also good
viz_fake_function <- function(data = filter_dash_data("A84600028X"),
                              min_date = as.Date("2010-01-01")) {
  data %>%
    dplyr::filter(.data$date >= min_date) %>%
    djpr_ts_linechart()
}
```

These are examples of **bad `viz_` functions** that do not satisfy the
requirements:

``` r
# THIS IS NOT OK - no data argument
viz_fake_function <- function() {
  filter_dash_data("A84600028X") %>%
    djpr_ts_linechart()
}

# THIS IS NOT OK - no default supplied to data argument 
viz_fake_function <- function(data) {
  data %>%
    djpr_ts_linechart()
}

# THIS IS NOT OK - no default supplied to min_date argument 
viz_fake_function <- function(data = filter_dash_data("A84600028X"),
                              min_date) {
  data %>%
    dplyr::filter(.data$date >= min_date) %>%
    djpr_ts_linechart()
}
```

### Graph functions’ output

All `viz_` functions must return a single object, which must be a
ggplot2 plot. The automated tests that are run on the package test that
each `viz_` function returns a ggplot2 plot – ie. a single object that
inherits class `gg` - and these tests will fail where this is not true.

If multiple plots are returned, they should be combined using the
`{patchwork}` package, which returns objects of class `gg`.

These are examples of **good `viz_` functions** that return a `gg`
object:

``` r
# This is good - returns a single ggplot2 object
viz_fake_function <- function(data = filter_dash_data("A84600028X")) {
  data %>%
    djpr_ts_linechart()
}

# This is also good - returns a single ggplot2 object
viz_fake_function <- function(data = filter_dash_data("A84600028X")) {
  my_plot <- data %>%
    djpr_ts_linechart()
  
  my_plot
}

# This is also good - returns a single ggplot2 object
viz_fake_function <- function(data = filter_dash_data("A84600028X")) {
  data %>%
    ggplot(aes(x = date, y = value)) +
    geom_line() +
    djprtheme::theme_djpr()
}

# This is also good - returns a {patchwork} object containing subplots
viz_fake_function <- function(data = filter_dash_data(c("A84599665J",
                                                        "A84600028X"))) {
  
  plot_1 <- data %>%
    dplyr::filter(.data$series_id == "A84599665J") %>%
    djpr_ts_linechart() +
    labs(subtitle = "Number of unemployed people in Ballarat (000s)")
  
  plot_2 <- data %>%
    dplyr::filter(.data$series_id == "A84600028X") %>%
    djpr_ts_linechart() +
    labs(subtitle = "Number of unemployed people in Bendigo (000s)")
  
  patchwork::wrap_plots(plot_1, plot_2, ncol = 1)
}
```

These are examples of **bad `viz_` functions** that do not return a
single `gg` object:

``` r
# THIS IS NOT OK - doesn't return anything
viz_fake_function <- function(data = filter_dash_data("A84600028X")) {
  my_plot <- data %>%
    djpr_ts_linechart()
}

# THIS IS NOT OK - returns something other than a `gg` object
viz_fake_function <- function(data = filter_dash_data("A84600028X")) {
  my_plot <- data %>%
    djpr_ts_linechart()
  
  data
}

# THIS IS NOT OK - the function makes two plots, but only one is returned
viz_fake_function <- function(data = filter_dash_data(c("A84599665J",
                                                        "A84600028X"))) {
  
  plot_1 <- data %>%
    dplyr::filter(.data$series_id == "A84599665J") %>%
    djpr_ts_linechart() +
    labs(subtitle = "Number of unemployed people in Ballarat (000s)")
  
  data %>%
    dplyr::filter(.data$series_id == "A84600028X") %>%
    djpr_ts_linechart() +
    labs(subtitle = "Number of unemployed people in Bendigo (000s)")
}

# THIS IS NOT OK - the function returns a list of `gg` objects
viz_fake_function <- function(data = filter_dash_data(c("A84599665J",
                                                        "A84600028X"))) {
  
  plot_1 <- data %>%
    dplyr::filter(.data$series_id == "A84599665J") %>%
    djpr_ts_linechart() +
    labs(subtitle = "Number of unemployed people in Ballarat (000s)")
  
  plot_2 <- data %>%
    dplyr::filter(.data$series_id == "A84600028X") %>%
    djpr_ts_linechart() +
    labs(subtitle = "Number of unemployed people in Bendigo (000s)")
  
  list(plot_1, plot_2)
}
```

## Shiny conventions

Each `viz_` function produces a chart. These charts must be used
somewhere on the Shiny dashboard. Like in any Shiny app, it is required
that each chart has a unique identifier, which is used to connect the UI
and server side of the app.

**We use the name of the relevant function without the `viz_` prefix as
the unique identifier for Shiny.** For example, the unique identified
for the `viz_ind_emppop_state_slope()` function is
`"ind_emppop_state_slope"`.

Most charts are added to the dashboard using the `djpr_plot_server()`
and `djpr_plot_ui()` functions. These live in the `{djprshiny}` package.
These are the two components of a Shiny module that creates a chart
environment with some standard features, including a download button,
and an optional date slider and checkbox. This Shiny module turns a
ggplot2 object into a responsive htmlwidget using the
`ggiraph::girafe()` function.

UI code lives in files that start with `page_`. For example, the UI of
the Indicators page of the dashboard lives in `page_indicators.R`.
Within this file, we add code like this:

``` r
djpr_plot_ui("ind_emppop_state_slope")
```

Server-side code lives in `labour_app.R`. Within this file, we add code
like this:

``` r
djpr_plot_server(id = "ind_emppop_state_slope",
                 plot_function = viz_ind_emppop_state_slope,
                 date_slider = FALSE,
# plt_change informs the Shiny module about changes in the browser size, to assist with correct re-sizing of the graph when the window changes size
                 plt_change = plt_change,
                 data = filter_dash_data(c(
                   "A84423272J",
                   "A84423356T",
                   "A84423286W",
                   "A84423370L",
                   "A84423328J",
                   "A84423300F",
                   "A84423314V",
                   "A84423342C"
                   )))
```
