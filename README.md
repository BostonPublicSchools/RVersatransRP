
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RVersatransRP

<!-- badges: start -->
<!-- badges: end -->

RVersatransRP provides easy to use functions for querying and formatting
data from a Versatrans Routing & Planning database.

## Installation

The development verions of RVersatransRP is currently available from
GitHub:

``` r
# install.packages("devtools") 
devtools::install_github("BostonPublicSchools/RVersatransRP")
```

## Example

This is a basic example which shows you how to retrieve the student
monitor file from the Versatrans RP database:

``` r
library(RVersatransRP) ## or source("RVersatransRP/R/monitors_student_file.R")

student_monitor <- rp_report_student_monitor(database = "Sandbox", format = "long")

## example pickup data:
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
dplyr::glimpse(student_monitor[0,])
#> Rows: 0
#> Columns: 13
#> $ RecordID               <int> 
#> $ `Student ID`           <chr> 
#> $ `Last, First`          <chr> 
#> $ `Program Description`  <chr> 
#> $ `INFO WC`              <chr> 
#> $ `INFO NEED MONITOR`    <chr> 
#> $ `School Abbrev`        <chr> 
#> $ `School Anchor Abbrev` <chr> 
#> $ `School Name`          <chr> 
#> $ Day                    <chr> 
#> $ Shift                  <chr> 
#> $ Route                  <chr> 
#> $ Bus                    <chr>
```
