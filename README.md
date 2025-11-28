
# `IEAexplorer`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install and run the development version of `{ieaexplorer}` like
so:

``` r
remotes::install_github("TanguyGen/IEAExplorer")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo TanguyGen/IEAExplorer@HEAD
#> Warning in untar2(tarfile, files, list, exdir, restore_times): skipping pax
#> global extended headers
#> Warning in untar2(tarfile, files, list, exdir, restore_times): skipping pax
#> global extended headers
#> digest   (0.6.38 -> 0.6.39) [CRAN]
#> promises (1.3.3  -> 1.5.0 ) [CRAN]
#> yaml     (2.3.10 -> 2.3.11) [CRAN]
#> purrr    (1.1.0  -> 1.2.0 ) [CRAN]
#> S7       (0.2.0  -> 0.2.1 ) [CRAN]
#> Installing 5 packages: digest, promises, yaml, purrr, S7
#> Installation des packages dans 'C:/Users/tgenthon/AppData/Local/R/cache/R/renv/library/IEAExplorer-aa1d099f/windows/R-4.5/x86_64-w64-mingw32'
#> (car 'lib' n'est pas spécifié)
#> 
#>   Des versions binaires sont disponibles mais les versions des sources
#>   sont plus récentes:
#>        binary source needs_compilation
#> digest 0.6.38 0.6.39              TRUE
#> yaml   2.3.10 2.3.11              TRUE
#> 
#> le package 'promises' a été décompressé et les sommes MD5 ont été vérifiées avec succés
#> Warning: impossible de supprimer l'installation précédente du package
#> 'promises'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problème lors de la
#> copie de
#> C:\Users\tgenthon\AppData\Local\R\cache\R\renv\library\IEAExplorer-aa1d099f\windows\R-4.5\x86_64-w64-mingw32\00LOCK\promises\libs\x64\promises.dll
#> vers
#> C:\Users\tgenthon\AppData\Local\R\cache\R\renv\library\IEAExplorer-aa1d099f\windows\R-4.5\x86_64-w64-mingw32\promises\libs\x64\promises.dll :
#> Permission denied
#> Warning: 'promises' restauré
#> le package 'purrr' a été décompressé et les sommes MD5 ont été vérifiées avec succés
#> Warning: impossible de supprimer l'installation précédente du package 'purrr'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problème lors de la
#> copie de
#> C:\Users\tgenthon\AppData\Local\R\cache\R\renv\library\IEAExplorer-aa1d099f\windows\R-4.5\x86_64-w64-mingw32\00LOCK\purrr\libs\x64\purrr.dll
#> vers
#> C:\Users\tgenthon\AppData\Local\R\cache\R\renv\library\IEAExplorer-aa1d099f\windows\R-4.5\x86_64-w64-mingw32\purrr\libs\x64\purrr.dll :
#> Permission denied
#> Warning: 'purrr' restauré
#> le package 'S7' a été décompressé et les sommes MD5 ont été vérifiées avec succés
#> Warning: impossible de supprimer l'installation précédente du package 'S7'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problème lors de la
#> copie de
#> C:\Users\tgenthon\AppData\Local\R\cache\R\renv\library\IEAExplorer-aa1d099f\windows\R-4.5\x86_64-w64-mingw32\00LOCK\S7\libs\x64\S7.dll
#> vers
#> C:\Users\tgenthon\AppData\Local\R\cache\R\renv\library\IEAExplorer-aa1d099f\windows\R-4.5\x86_64-w64-mingw32\S7\libs\x64\S7.dll :
#> Permission denied
#> Warning: 'S7' restauré
#> 
#> Les packages binaires téléchargés sont dans
#>  C:\Users\tgenthon\AppData\Local\Temp\Rtmp6ZrffH\downloaded_packages
#> installation des packages sources 'digest', 'yaml'
#> Warning in i.p(...): l'installation du package 'digest' a eu un statut de
#> sortie non nul
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\tgenthon\AppData\Local\Temp\Rtmp6ZrffH\remotes69e054e32159\TanguyGen-IEAExplorer-2f157d9/DESCRIPTION' ...     checking for file 'C:\Users\tgenthon\AppData\Local\Temp\Rtmp6ZrffH\remotes69e054e32159\TanguyGen-IEAExplorer-2f157d9/DESCRIPTION' ...   ✔  checking for file 'C:\Users\tgenthon\AppData\Local\Temp\Rtmp6ZrffH\remotes69e054e32159\TanguyGen-IEAExplorer-2f157d9/DESCRIPTION' (1.6s)
#>       ─  preparing 'ieaexplorer': (1.8s)
#>      checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts (1.7s)
#>   ─  checking for empty or unneeded directories
#>      Omitted 'LazyData' from DESCRIPTION
#>      NB: this package now depends on R (>=        NB: this package now depends on R (>= 3.5.0)
#>        WARNING: Added dependency on R >= 3.5.0 because serialized objects in
#>      serialize/load version 3 cannot be read in older versions of R.
#>      File(s) containing such objects:
#>        'ieaexplorer/inst/app/www/Iceland/tables.Rdata'
#>       ─  building 'ieaexplorer_0.0.0.9000.tar.gz'
#>      
#> 
#> Installation du package dans 'C:/Users/tgenthon/AppData/Local/R/cache/R/renv/library/IEAExplorer-aa1d099f/windows/R-4.5/x86_64-w64-mingw32'
#> (car 'lib' n'est pas spécifié)
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
