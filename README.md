
<!-- README.md is generated from README.Rmd. Please edit that file -->

# appendornot

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/appendornot)](https://CRAN.R-project.org/package=appendornot)
<!-- badges: end -->

The goal of `appendornot` is to provide easy access to writing and
appending csv or text files.

## Installation

You can install the development version of `appendornot` like so:

``` r
## install.packages("remotes")
remotes::install_github("favstats/appendornot")
```

## Load Library

``` r
library(appendornot)
```

## Motivation for `appendornot`

I often find myself in the situation where I am scraping data and I want
to keep appending some data to a file. However, I also want to make it
so that it creates the file in the first place if it doesn’t exist yet,
then appends to it in the next iteration.

## Example `save_csv()`

Write csv file with `save_csv`. The function will automatically create a
new csv or append the file if it already does exist.

``` r
save_csv(cars, "cars.csv")
```

You can also create a folder (if it doesn’t exist yet) before it saves
the csv.

This example creates the “`data`” folder first, then saves the file:

``` r
save_csv(cars, "data/cars.csv")
```

## Example `save_lines()`

Write text file with `save_lines`. The function will automatically
create a new text file or append the file if it already does exist.

``` r
save_lines(names(cars), "lines.txt")
```

You can also create a folder (if it doesn’t exist yet) before it saves
the text file.

This example creates the “`txt`” folder first, then saves the file:

``` r
save_lines(names(cars),  "txt/lines.txt")
```
