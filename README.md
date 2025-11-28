
# `{ieaexplorer}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install and run the development version of `{ieaexplorer}` like
so:

``` r
remotes::install_github("TanguyGen/IEAExplorer")
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'ieaexplorer' from a github remote, the SHA1 (c271be85) has not changed since last install.
#>   Use `force = TRUE` to force installation
ieaexplorer::run_app()
#> Warning: remplacement de l'importation précédente 'jsonlite::validate' par
#> 'shiny::validate' lors du chargement de 'ieaexplorer'
#> Warning: remplacement de l'importation précédente 'shiny::renderDataTable' par
#> 'DT::renderDataTable' lors du chargement de 'ieaexplorer'
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>

or install this repository and run:

``` r
library(shiny)
runApp()
```

## About

IEAExplorer is an app made to visualise and download time series, using
the ATAC analysis (Analysis of Trend And recent Changes) for different
marine socio-ecosystems. The time series include data on ocean climate,
ecology, human pressure and sectors.
