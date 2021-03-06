
<!-- README.md is generated from README.Rmd. Please edit that file -->

# erf-numerical-results

<!-- badges: start -->

<!-- badges: end -->

The goal of `erf-numerical-results` is to reproduce the results of the
paper.

## Dependencies

Make sure you install the packages listed in `main/dependencies.R`. In
particular, you will need the `erf` package. To install it, run:

``` r
# install.packages("devtools")
devtools::install_github("nicolagnecco/erf")
```

## Reproduce figures

To create the figures in the paper, you can run
`main/plots_for_manuscript.R`.

``` r
source("main/plots_for_manuscript.R")
```

## Run code

To run the code that produced the figures, you can run one of the
individual files `main/sec_*.R`. Make sure to set the parameter
`RUN_SIMULATION <- TRUE` at the top of the file you want to run.

**Note**: some simulations are computationally heavy, and run in
parallel with 8–12 workers. To change the number of workers, specify
`n_workers` in the file or its corresponding `configs/` file.
