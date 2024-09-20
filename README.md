<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Release](https://img.shields.io/github/release/inbo/forrescalc.svg)](https://github.com/inbo/forrescalc/releases)
[![R build
status](https://github.com/inbo/forrescalc/workflows/check%20package%20on%20main/badge.svg)](https://github.com/inbo/forrescalc/actions)
![r-universe
name](https://inbo.r-universe.dev/badges/:name?color=c04384)
[![r-universe package](https://inbo.r-universe.dev/badges/forrescalc)](https://inbo.r-universe.dev/forrescalc)
[![Codecov test
coverage](https://codecov.io/gh/inbo/forrescalc/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/forrescalc?branch=main)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/inbo/forrescalc.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/inbo/forrescalc.svg)
<!--[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4028303.svg)](https://doi.org/10.5281/zenodo.4028303)-->
<!-- badges: end -->

# forrescalc

Calculation of aggregated values on dendrometry, regeneration and vegetation of forests, starting from individual tree measures from Vandekerkhove et al. (2021) that are saved in a `Fieldmap` database.

Vandekerkhove K., Van de Kerckhove P., Leyman A., De Keersmaeker L., Lommelen E., Esprit M. and Goessens S., 2021. Monitoring programme on strict forest reserves in flanders (Belgium): Methods and operational protocols: With an overview of the intensive monitoring sites. (Rapporten van het Instituut voor Natuur- en Bosonderzoek; Nr. 28). Instituut voor Natuur- en Bosonderzoek. https://doi.org/10.21436/inbor.38677490

# Installation

To install `forrescalc` from the [INBO universe](https://inbo.r-universe.dev/builds),
start a new R session and run this code (before loading any packages):

```r
# Enable the INBO universe (not needed for INBO employees, as this is the default setting)
options(
  repos = c(
    inbo = "https://inbo.r-universe.dev", CRAN = "https://cloud.r-project.org"
  )
)
# Install the package
install.packages("forrescalc")
```

To install `forrescalc` from GitHub, start a new R session and run this code (before loading any packages):

```r
#install.packages("remotes")
remotes::install_github("inbo/forrescalc", build_vignettes = TRUE)
```

Some functions require local access to the GitHub repository [`forresdat`](https://github.com/inbo/forresdat).
