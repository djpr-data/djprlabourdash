
<!-- README.md is generated from README.Rmd. Please edit that file -->

# djprlabourdash

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/djpr-data/djprlabourdash/workflows/R-CMD-check/badge.svg)](https://github.com/djpr-data/djprlabourdash/actions)[![Codecov
test
coverage](https://codecov.io/gh/djpr-data/djprlabourdash/branch/main/graph/badge.svg)](https://codecov.io/gh/djpr-data/djprlabourdash?branch=main)
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

Git and GitHub are used to manage version control and collaboration
within this project. See [Happy Git and GitHub for the
useR](https://happygitwithr.com) for an introduction to Git and GitHub.

Work in the repository happens in different
[branches](https://happygitwithr.com/git-branches.html). There are three
types of branches in this repository:

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

The `djprlabourdash` package has an internal data object called
`dash_data`. This is created/updated by sourcing the file
`data-raw/internal_data.R`.

The function `djprlabourdash::load_dash_data()` can be used to obtain
the latest version of the data from the `djprdashdata` repository. This
function is executed once at the start of each dashboard session,
ensuring any upstream change in the data is (nearly) immediately
reflected on the dashboard itself.

In interactive sessions where a contributor is working with the
dashboard data, the following command should be run at the beginning of
the session:

``` r
# First, run `load_all()` to ensure that this package's functions are available
# devtools::load_all()
dash_data <- load_dash_data()
```

`dash_data` is a tibble (data frame):

``` r
dash_data
#> # A tibble: 221,284 × 32
#>    series_id   date       value series  series_type table_no data_type frequency
#>    <chr>       <date>     <dbl> <chr>   <chr>       <chr>    <chr>     <chr>    
#>  1 15-24_empl… 1998-10-01  30.8 15-24 … Original    RM1      STOCK     Month    
#>  2 15-24_empl… 1998-11-01  31.9 15-24 … Original    RM1      STOCK     Month    
#>  3 15-24_empl… 1998-12-01  32.3 15-24 … Original    RM1      STOCK     Month    
#>  4 15-24_empl… 1999-01-01  30.6 15-24 … Original    RM1      STOCK     Month    
#>  5 15-24_empl… 1999-02-01  32.0 15-24 … Original    RM1      STOCK     Month    
#>  6 15-24_empl… 1999-03-01  31.3 15-24 … Original    RM1      STOCK     Month    
#>  7 15-24_empl… 1999-04-01  31.5 15-24 … Original    RM1      STOCK     Month    
#>  8 15-24_empl… 1999-05-01  31.2 15-24 … Original    RM1      STOCK     Month    
#>  9 15-24_empl… 1999-06-01  31.7 15-24 … Original    RM1      STOCK     Month    
#> 10 15-24_empl… 1999-07-01  32.1 15-24 … Original    RM1      STOCK     Month    
#> # … with 221,274 more rows, and 24 more variables: unit <chr>, age <chr>,
#> #   gcc_restofstate <chr>, indicator <chr>, sex <chr>, industry <chr>,
#> #   duration <chr>, state <chr>, sa4 <chr>, current_lfs <chr>,
#> #   dependents <chr>, duration_of_unemp <chr>, duration_with_employer <chr>,
#> #   education_attendance <chr>, exp_future_emp <chr>, highest_ed <chr>,
#> #   hours <chr>, industry_subdiv <chr>, marital_status <chr>,
#> #   market_nonmarket <chr>, occupation <chr>, relship_in_hhold <chr>, …
```

### Filtering dashboard data

Users generally work with the `dash_data` object using the
`filter_dash_data()` function. This function takes a vector of ABS time
series ID(s) and returns a tibble containing the corresponding data.

A lookup table of series IDs is available from the `djprdashdata`
package:

``` r
djprdashdata::lfs_lookup
#> # A tibble: 22,246 × 28
#>    age   gcc_restofstate indicator series_id  series  sex   series_type table_no
#>    <chr> <chr>           <chr>     <chr>      <chr>   <chr> <chr>       <chr>   
#>  1 15-24 ""              Employed  15-24_emp… 15-24 … ""    Original    RM1     
#>  2 15-24 ""              Employed  15-24_emp… 15-24 … ""    Original    RM1     
#>  3 15-24 ""              Employed  15-24_emp… 15-24 … ""    Original    RM1     
#>  4 15-24 ""              Employed  15-24_emp… 15-24 … ""    Original    RM1     
#>  5 15-24 ""              Employed  15-24_emp… 15-24 … ""    Original    RM1     
#>  6 15-24 ""              Employed  15-24_emp… 15-24 … ""    Original    RM1     
#>  7 15-24 ""              Employed  15-24_emp… 15-24 … ""    Original    RM1     
#>  8 15-24 ""              Employed  15-24_emp… 15-24 … ""    Original    RM1     
#>  9 15-24 ""              Employed  15-24_emp… 15-24 … ""    Original    RM1     
#> 10 15-24 ""              Employed  15-24_emp… 15-24 … ""    Original    RM1     
#> # … with 22,236 more rows, and 20 more variables: cat_no <chr>, industry <chr>,
#> #   duration <chr>, state <chr>, sa4 <chr>, current_lfs <chr>,
#> #   dependents <chr>, duration_of_unemp <chr>, duration_with_employer <chr>,
#> #   education_attendance <chr>, exp_future_emp <chr>, highest_ed <chr>,
#> #   hours <chr>, industry_subdiv <chr>, marital_status <chr>,
#> #   market_nonmarket <chr>, occupation <chr>, relship_in_hhold <chr>,
#> #   sector <chr>, status_in_emp <chr>
```

If a user wanted to get a data frame with the number of unemployed
people in Bendigo each month from 1998 to present, they would use
`filter_dash_data()` as follows:

``` r
bendigo_unemp <- filter_dash_data("A84600028X")
bendigo_unemp
#> # A tibble: 276 × 32
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
#> # … with 266 more rows, and 24 more variables: unit <chr>, age <chr>,
#> #   gcc_restofstate <chr>, indicator <chr>, sex <chr>, industry <chr>,
#> #   duration <chr>, state <chr>, sa4 <chr>, current_lfs <chr>,
#> #   dependents <chr>, duration_of_unemp <chr>, duration_with_employer <chr>,
#> #   education_attendance <chr>, exp_future_emp <chr>, highest_ed <chr>,
#> #   hours <chr>, industry_subdiv <chr>, marital_status <chr>,
#> #   market_nonmarket <chr>, occupation <chr>, relship_in_hhold <chr>, …
```

It is then straightforward to visualise this. We will use the
`djprshiny::djpr_ts_linechart()` convenience function, which creates a
ggplot2 object that generally meets our style guide and has some
standard elements, such as an annotation on the final point.

``` r
library(djprshiny)

bendigo_unemp %>%
  djpr_ts_linechart()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## `viz_` files

Most contributions to the dashboard will come in the `viz_` files. These
contain the code that creates the graphs on the dashboard.

Generally, **each graph has its own function** – the only exception are
some functions that produce multiple related graphs that are combined
with `{patchwork}`.

### Graph functions’ names

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

The different options for the `page` part of the `viz_` function names
are:

-   `overview` for graphs on the Overview page;
-   `ind` for graphs on the Indicators page;
-   `gr` for graphs on the Inclusion page (formerly known as Groups);
-   `reg` for graphs on the Regions page;
-   `industries` for graphs on the Industries page.

### Graph functions’ arguments

**Each `viz_` function must have a first argument called `data`**. It
can have other arguments, but this is optional. **All arguments to
`viz_` functions must have defaults supplied.**

Generally, the default to the `data` argument will involve a call to
`filter_dash_data()`. This function takes a vector of ABS time series
ID(s) and returns a dataframe containing the data corresponding to those
IDs, if the data is present in the data loaded into the dashboard.

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

# This is also good - you don't have to use `djpr_ts_linechart()`
viz_fake_function <- function(data = filter_dash_data("A84600028X")) {
  data %>%
    ggplot(aes(x = date, y = value)) +
    geom_line() +
    djprtheme::theme_djpr()
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

## Code convention: `{tidyverse}`-first

The code in this package is generally written using the `{tidyverse}`
packages, such as `{dplyr}`, `{tidyr}` and `{ggplot2}`. Contributions to
the package should do the same.

## Code convention: using functions from packages

All packages you wish to use must be included in the `DESCRIPTION` file.
If you want to use a package, open `DESCRIPTION` and check if the
package is there. If it is not, add your package using
`usethis::use_package("package_name")`. Before doing this, consider
whether the functionality you need is present in a package already
present in `DESCRIPTION`.

Functions other than those from base R should generally be called using
`package_name::function_name()`, as in `stringr::str_wrap()`. Omitting
this - such as calling `str_wrap()` will generally cause a warning to be
thrown by `R CMD check`.

Avoiding importing functions using the Roxygen
`@importFrom package_name function_name` pattern.

The `djprlabourdash` package imports several packages in their entirety,
including `ggplot2` and `shiny`. Functions from these packages do not
technically need to be called with `package_name::`, but they still
*can* be called this way. If in doubt, use `::`.

## Code convention: using `.data$` for non-standard evaluation

Writing our code using the `{tidyverse}` packages means our code tends
to be readable and expressive. However, part of what makes tidyverse
code readable is that it blurs the line between variables in your
environment, and variables (columns) in the data frame you are operating
on. This can cause problems.

For example, let’s look at the `mpg` data frame that is supplied with
the `{ggplot2}` package:

``` r
head(ggplot2::mpg)
#> # A tibble: 6 × 11
#>   manufacturer model displ  year   cyl trans      drv     cty   hwy fl    class 
#>   <chr>        <chr> <dbl> <int> <int> <chr>      <chr> <int> <int> <chr> <chr> 
#> 1 audi         a4      1.8  1999     4 auto(l5)   f        18    29 p     compa…
#> 2 audi         a4      1.8  1999     4 manual(m5) f        21    29 p     compa…
#> 3 audi         a4      2    2008     4 manual(m6) f        20    31 p     compa…
#> 4 audi         a4      2    2008     4 auto(av)   f        21    30 p     compa…
#> 5 audi         a4      2.8  1999     6 auto(l5)   f        16    26 p     compa…
#> 6 audi         a4      2.8  1999     6 manual(m5) f        18    26 p     compa…
```

Each row of this data frame corresponds to a different type of car. Now,
let’s say we want to calculate for each car the ratio of highway fuel
efficiency (`hwy`) to city fuel efficiency (`cty`). We could do this
with `dplyr::mutate()` as follows:

``` r
ggplot2::mpg %>%
  dplyr::mutate(ratio = hwy / cty)
#> # A tibble: 234 × 12
#>    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
#>    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
#>  1 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
#>  2 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
#>  3 audi         a4           2    2008     4 manu… f        20    31 p     comp…
#>  4 audi         a4           2    2008     4 auto… f        21    30 p     comp…
#>  5 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
#>  6 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
#>  7 audi         a4           3.1  2008     6 auto… f        18    27 p     comp…
#>  8 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
#>  9 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
#> 10 audi         a4 quattro   2    2008     4 manu… 4        20    28 p     comp…
#> # … with 224 more rows, and 1 more variable: ratio <dbl>
```

Here, `dplyr::mutate()` correctly assumes that we are referring to the
`hwy` and `cty` variables within the `mpg` data frame. But what happens
if we already have a variable called `cty` that is separate from this
data frame?

``` r
cty <- 15

ggplot2::mpg %>%
  dplyr::mutate(ratio = hwy / cty)
#> # A tibble: 234 × 12
#>    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
#>    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
#>  1 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
#>  2 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
#>  3 audi         a4           2    2008     4 manu… f        20    31 p     comp…
#>  4 audi         a4           2    2008     4 auto… f        21    30 p     comp…
#>  5 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
#>  6 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
#>  7 audi         a4           3.1  2008     6 auto… f        18    27 p     comp…
#>  8 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
#>  9 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
#> 10 audi         a4 quattro   2    2008     4 manu… 4        20    28 p     comp…
#> # … with 224 more rows, and 1 more variable: ratio <dbl>
```

`dplyr::mutate()` again assumes that when we refer to `cty`, we are
referring to the variable in `mpg`. This is correct in this case, but it
is dangerously ambiguous. In a package like `djprlabourdash` we want to
be explicit about whether we are referring to a variable within our
dataframe, or a variable outside the data frame (an environment
variable). To do this we use the `.data$` and `.env$` prefixes from the
`{rlang}` package. So the code chunk above would instead be written like
this:

``` r
cty <- 15

ggplot2::mpg %>%
  dplyr::mutate(ratio = .data$hwy / .data$cty)
#> # A tibble: 234 × 12
#>    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
#>    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
#>  1 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
#>  2 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
#>  3 audi         a4           2    2008     4 manu… f        20    31 p     comp…
#>  4 audi         a4           2    2008     4 auto… f        21    30 p     comp…
#>  5 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
#>  6 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
#>  7 audi         a4           3.1  2008     6 auto… f        18    27 p     comp…
#>  8 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
#>  9 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
#> 10 audi         a4 quattro   2    2008     4 manu… 4        20    28 p     comp…
#> # … with 224 more rows, and 1 more variable: ratio <dbl>
```

Now we have explicitly told `dplyr::mutate()` that we are referring to
the `hwy` and `cty` variables within our data, so there can be no
confusion. If, instead, we wanted to divide each car’s highway
efficiency by a constant number, we could do:

``` r
cty <- 15

ggplot2::mpg %>%
  dplyr::mutate(ratio = .data$hwy / .env$cty)
#> # A tibble: 234 × 12
#>    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
#>    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
#>  1 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
#>  2 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
#>  3 audi         a4           2    2008     4 manu… f        20    31 p     comp…
#>  4 audi         a4           2    2008     4 auto… f        21    30 p     comp…
#>  5 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
#>  6 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
#>  7 audi         a4           3.1  2008     6 auto… f        18    27 p     comp…
#>  8 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
#>  9 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
#> 10 audi         a4 quattro   2    2008     4 manu… 4        20    28 p     comp…
#> # … with 224 more rows, and 1 more variable: ratio <dbl>
```

Again, we are being clear and unambiguous in our `mutate()` call. This
pattern - prefixing variables within our data frame with `.data$` -
should be used in your code.

See the [programming with
`dplyr`](https://dplyr.tidyverse.org/articles/programming.html) vignette
for more on non-standard evaluation.

## Code style

We follow the [tidyverse style guide](https://style.tidyverse.org). To
style your code consistent with this guide, run `styler::style_pkg()`.

Note that all functions, arguments, and objects should be named using
`snake_case` - all letters are lower case, words separated with
underscores.

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

## Adding a chart to the dashboard: a step-by-step guide

### Creating a `viz_` function

1.  If you do not already have a GitHub account, [create
    one](https://github.com/join). Then ask Matt Cowgill or another
    Data + Analytics administrator to add you to the `djpr-data` GitHub
    team.
2.  As a once-off step, you will need to create a local version of the
    `djprlabourdash` repository on your machine. In RStudio, click
    `File` -> `New Project` -> `Version Control` -> `Git.` Then paste
    the GitHub repository URL for `djprdashdata` and choose a location
    for the project on your disk. Note that this copy of the repository
    is just for you, so put it in a location that only you have access
    to.
3.  Create a new branch on GitHub in `djprlabourdash` from the `dev`
    branch.
4.  In RStudio, click the `Git` pane (by default this is in the
    upper-right hand quadrant of the RStudio window). Click `Pull` then
    switch to your new branch. **Ensure that you are working in your
    branch** - the name of the active Git branch is shown in the
    upper-right hand corner of the `Git` pane in RStudio - confirm this
    is your branch.
5.  Open the relevant `viz_*.R` file from the `R` folder of the
    repository. For example, if you want to contribute a chart to the
    Indicators page, open `viz_indicators.R`.
6.  Ensure you have nothing in your environment (if you click the
    `Environment` pane in RStudio, there should be nothing there. Click
    the broom icon to clear your environment.)
7.  If you do not have the `devtools` package installed, install it
    using: `install.packages("devtools")`. This only needs to be done
    once.
8.  You will need to install the packages that `djprlabourdash` depends
    on. Do this using `devtools::install_deps()`.
9.  Run `devtools::load_all()` to make the functions of `djprlabourdash`
    available to you.
10. To help you in the process of writing your graph function, download
    the dashboard’s data and assign it to an object called `dash_data`,
    by running this line:

``` r
dash_data <- load_dash_data()
```

1.  Add a new function to the `viz_*.R` file that is called `viz_`
    something - eg. `viz_ind_emppop_state_slope` - and satisfies the
    rules set out above. In short, it should have a `data` argument, it
    can optionally have other arguments, all arguments must have default
    values specified, and it must return a ggplot2 object. 12 Check that
    your function works by running it in the console. A ggplot2 chart
    should be displayed in RStudio.
2.  Check that the dashboard (including your new function) passes the
    automated test by clicking `Build` -> `Check` in RStudio. It should
    go through the process of checking the passage and conclude by
    saying `0 ERRORS | 0 WARNINGS | 0 NOTES`. If you encounter errors
    here, you will need to fix the error, with help from teammates where
    required.

At this stage, you have successfully added a `viz_` function. The
remaining steps are to add your graph to the dashboard, and to initiate
a pull request to merge your code with the existing dashboard code base.

### Adding your graph to the dashboard

The DJPR Jobs Dashboard is a web app made with the Shiny package. All
Shiny apps have two key components - the UI and the server-side code. We
need to edit each of them to add your chart to the dashboard.

#### The UI side

1.  The UI code for each page of the dashboard lives in its own file.
    For example, the Indicators page code lives in `page_indicators.R`.
    Open the relevant `page_` file.
2.  Identify the location on the page where you want your graph to live.
    The elements in the UI page directly correspond to the content of
    the dashboard, and are in the same order. Each element is separated
    from the next with a comma.
3.  Add a `djprshiny::djpr_plot_ui()` call. The first argument must be a
    unique identifier for your graph. Our convention is to use the name
    of your `viz_` function (without the `viz_` part) as this
    identifier. For example, the identifier for
    `viz_ind_emppop_state_slope()` is `"ind_emppop_state_slope"`. Your
    code will look like

``` r
djpr_plot_ui("ind_emppop_state_slope")
```

#### The server side

1.  The server-side code lives in `labour_app.R`. Open this file.
2.  Find the relevant location in the file. Note that the order of the
    code within the server file does not matter, but for ease of
    maintenance we try to group server code from each page together, and
    order in (roughly) the same order as on the dashboard.
3.  Add a call to `djprshiny::djpr_plot_server()`. This calls your
    `viz_` function and sets up the plot environment that will be
    displayed to the user. Your code will look something like:

``` r
djpr_plot_server(id = "ind_emppop_state_slope",
                 plot_function = viz_ind_emppop_state_slope,
                 date_slider = FALSE,
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

Note that for some charts you may want a `date_slider`, in which case
you will set that argument to `TRUE`. There are other arguments to
`djpr_plot_server()` that allow you to customise the plot environment -
see `?djpr_plot_server()` for the help file.

#### Manually inspecting your work

1.  Clear your environment and start a new R session (`Session` ->
    `Restart R`). This ensures that any objects you’ve created in your
    development process are not available.
2.  Run `devtools::load_all()`
3.  Run `app()`
4.  Navigate to the place on the dashboard where you expect to see your
    graph. Confirm it is there. Check that any interactivity works as
    expected, such as tooltips, date sliders, and plot download buttons.

### Merging your code with the main dashboard

1.  Ensure your work is saved.
2.  In the `Git` pane in RStudio, select all files. Click one of the
    checkboxes in the `Staged` column.
3.  Click `Commit`
4.  Write an informative message to describe what you have done, such as
    “Add `viz_ind_emppop_state_slope()` function to the indicators
    page”. Click `Commit` again. Click `Close` on the window that pops
    up to confirm your commit has worked.
5.  Click `Push`. Close the window that pops up, and close the Review
    Changes window. Your `Git` pane should now have no files listed in
    it.
6.  Go to the
    [`djprlabourdash`](https://github.com/djpr-data/djprlabourdash)
    repository on GitHub. Navigate to your branch. Initiate a pull
    request by clicking the button marked `Compare & pull request`.
7.  Ensure that your pull request is going from your branch to `dev`.
8.  Write an informative pull request title and comment. The comment
    should describe what has been done in this batch of code, and any
    outstanding issues that must be fixed by others.
9.  Click `Create pull request`.

A project admin will review your code, and GitHub will conduct automated
test. Once the pull request has been concluded, your code will be merged
into `dev` branch. At this stage the live dashboard on the web does not
yet reflect your changes - this will be updated when `dev` is merged
into `main`, which the project administrator does periodically.
