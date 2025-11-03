# Evidence for unconscious priming using a Bayesian single-subject approach

This repository contains the data and analysis script of the manuscript "Evidence for unconscious priming using a Bayesian single-subject approach"

## Data

The file `raw_control.csv` contains the first session (before training) of all participants, and the file `raw_train.csv` contains sessions 2-5 and 6 of all participants.

## R and package versions

This project uses [**renv**](https://rstudio.github.io/renv/) to ensure a fully reproducible R environment.

The exact R version (4.3.3 (2024-02-29)) version and package versions used for all analyses are recorded in the `renv.lock` file.

To reproduce the same environment:

```r
install.packages("renv")
renv::restore()
```

Note that `renv` will install the appropriate version of the packages but it won't change your R version. You may need to install R (4.3.3 (2024-02-29)) yourself.

## Getting Started

Analysis scripts are numbered in the order they should be run:

```
01_filter_data.R
02_test_control_group.R
03_plot_control_group.R
04_test_train_group.R
05_plot_train_group.R
06_test_train_subjects.R
07_plot_train_subjects.R
08_recode_mask_data.R
09_test_subjects_direct_comparison.R
10_plot_subjects_direct_comparison.R
11_test_group_direct_comparison.R
12_plot_group_direct_comparison.R
13_recreate_vorberg_plots.R
```

Compiled models are provided to prevent the need to rerun the models. Alternatively, the compiled models (.rds files) can be deleted to re-run the models.

