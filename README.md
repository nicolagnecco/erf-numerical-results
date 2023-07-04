
<!-- README.md is generated from README.Rmd. Please edit that file -->

# erf-numerical-results

<!-- badges: start -->
<!-- badges: end -->

The goal of `erf-numerical-results` is to reproduce the results of the
paper. For further details see Gnecco, Terefe, and Engelke (2023,
https://arxiv.org/abs/2201.12865)

## Dependencies

Make sure you install the packages listed in `main/dependencies.R`. In
particular, you will need the `erf` package. To install it, run:

``` r
# install.packages("devtools")
devtools::install_github("nicolagnecco/erf")
```

## Clone repository

The repository works with `git lfs` \[<https://git-lfs.com/>\]. After
you install `git lfs` on your system, you can clone the GitHub directory
by typing the following in your command line.

``` bash
git lfs clone https://github.com/nicolagnecco/erf-numerical-results.git
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

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-merg2020" class="csl-entry">

Gnecco, Nicola, Edossa Merga Terefe, and Sebastian Engelke. 2023.
“Extremal Random Forests.” <https://arxiv.org/abs/2201.12865>.

</div>

</div>
