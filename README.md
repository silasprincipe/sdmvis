
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sdm**vis** <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**sdmvis** is a helper package: it enables to analyze SDM/ENM or any
other spatial-based output similar to an SDM in an interactive map.
sdmvis is mainly based on the `leaflet` package, and is similar to the
great `mapview` package, but more focused on the SDM routine.

In addition to providing simple ways to plot SDM/ENM maps during the
modeling, it is also a good way to produce maps to be used in
interactive reports or pages.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("silasprincipe/sdmvis")
```

## Usage

The main function of the **sdmvis** package is the `sdm_leaflet`. You
can use it as:

``` r
sdm_leaflet(your_sdm_raster)
```

You can also include your occurrence data as a map layer:

``` r
sdm_leaflet(your_sdm_raster,
            pts = your_occurrence_data)
```

[See the package vignette for a complete use case within the SDM
routine.](https://silasprincipe.github.io/sdmvis/articles/linking.html)

## Next steps

This work is still experimental, so I hope to add some other functions
in the future. Please, let me know if you find any bug or if you have
suggestions for improvement.
