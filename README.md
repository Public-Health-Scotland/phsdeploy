
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phsdeploy

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Public-Health-Scotland/phsdeploy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Public-Health-Scotland/phsdeploy/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of phsdeploy is to provide a consistent pipeline for
publishing information products, including Shiny apps, R Markdown and
Quarto reports.

## Installation

You can install the development version of phsdeploy like so:

``` r
remotes::install_git(url = "git@github.com:public-health-scotland/phsdeploy")
```

Where network security settings require, it is possible to download a
zip of the repository and install like so:

``` r
# Install from local zip
remotes::install_local("<FILEPATH OF ZIPPED FILE>/phsdeploy-main.zip",
  upgrade = "never"
)
```

## Usage

Once installed, you can load phsdeploy like so:

``` r
library(phsdeploy)
```

## Licence

This package is available under the [MIT Licence](LICENSE). This covers
both the codebase and any sample code in the documentation.

© Public Health Scotland, 2026. The documentation in this package is
licensed under the [Open Government Licence
3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3),
except where otherwise stated.
