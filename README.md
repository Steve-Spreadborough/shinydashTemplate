
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{shinyTemplate}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Details

This is a project used to develop functionality for Shiny Apps in a
single place which can then be used as a template for creating actual
Apps from. The idea therefore being that everything in this project
should be reproducible and easy to copy/replicate to other Shiny Apps to
support creating and rolling out ‘production’ apps with similar
layouts/functionality..

This project is very much developmental and reflects authors development
of code and knowledge around Shiny. Contributions and suggestions on
improving code, following the code of conduct, are welcome.

The layout of code follows the use of modules & R6 Class object for
sharing data between modules within a `{golem}` framework. See
[here](https://engineering-shiny.org/index.html) re creating production
grade shiny apps with `{golem}` and
[here](https://engineering-shiny.org/common-app-caveats.html?q=R6#using-r6-as-data-storage)
re using R6 class objects for data storage.

**Important**: This app uses data from the `stats19` package,
[here](https://github.com/ropensci/stats19), containing crash data. The
app uses this data purely as example data and the **outputs included in
this App are for demonstration/exploration of code purposes only and are
not official analysis of the data**.

**NOTE:** A more developed version using `bslibs` can be found
[here](https://github.com/Steve-Spreadborough/shinyTemplate)

As a basic, this application includes the following set up:

- Code structure following `{golem}`
- Shiny dashboard layout using `shinydashboardPlus`
- Modules with UIs & servers
- R6 class objects for data storage and access across modules
- Reactivity of R6 objects using `gargoyle`
- Util functions used across the App
- Unit testing

See NEWS for further developments.

## Run

You can launch the application by running:

``` r
devtools::load_all()
shinyTemplate::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-01-29 09:25:47 GMT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading shinyTemplate
#> ── R CMD check results ─────────────────────────── shinyTemplate 0.0.0.9000 ────
#> Duration: 1m 34.3s
#> 
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

``` r
covr::package_coverage()
#> shinyTemplate Coverage: 62.94%
#> R/run_app.R: 0.00%
#> R/server_utils.R: 0.00%
#> R/mod_date_filter.R: 48.39%
#> R/mod_explore_data.R: 57.08%
#> R/app_config.R: 100.00%
#> R/app_r6.R: 100.00%
#> R/app_server.R: 100.00%
#> R/app_ui.R: 100.00%
#> R/mod_intro.R: 100.00%
#> R/mod_pres_select.R: 100.00%
```
