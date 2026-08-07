# Evidence for unconscious priming using a Bayesian single-subject approach

This repository contains the data and analysis script of the manuscript "Evidence for unconscious priming using a Bayesian single-subject approach"

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

`00_make_raw_data.R` is only used to create the public raw-data files included in this repository. It depends on source files that are not distributed in the GitHub or OSF versions of the repository, so it is not expected to run for external users.

Analysis scripts are numbered in the order they should be run:

```
01_filter_data.R                                # Filters trials and prepares data for analysis
02_test_control_group.R                         # Bayesian tests for the control session (prime & mask tasks)
03_plot_control_group.R                         # Plots for the control session
04_test_train_group.R                           # Bayesian tests for the trained sessions (group level)
05_plot_train_group.R                           # Plots for the trained sessions (group level)
06_test_train_subjects.R                        # Bayesian tests for the trained sessions (single-subject level)
07_plot_train_subjects.R                        # Plots for the trained sessions (single-subject level)
08_recode_mask_data.R                           # Recodes mask discrimination data
09_check_mask_data_recoding.R                   # Checks and validates the mask data recoding
10_split_half_rel_train_subjects.R              # Split-half reliability analysis for the trained sessions
11_test_subjects_direct_comparison.R            # Bayesian tests: direct comparison prime/mask (single-subject level)
12_plot_subjects_direct_comparison.R            # Plots for the direct comparison prime/mask (single-subject level)
13_test_group_direct_comparison.R               # Bayesian tests: direct comparison prime/mask (group level)
14_plot_group_direct_comparison.R               # Plots for the direct comparison prime/mask (group level)
15_recreate_vorberg_plots.R                     # Recreates Vorberg-style priming visualisations
16_test_group_direct_comparison_intersubject_variance.R  # Tests intersubject variance in the direct comparison
17_rt_trim_check_control_group.R                # RT trimming robustness check – control session
18_rt_trim_check_train_group.R                  # RT trimming robustness check – trained sessions (group level)
19_rt_trim_check_train_subject.R                # RT trimming robustness check – trained sessions (single-subject level)
20_rt_trim_check_train_direct_subject.R         # RT trimming robustness check – direct comparison (single-subject level)
21_rt_trim_check_train_direct_group.R           # RT trimming robustness check – direct comparison (group level)
22_criterion_300ms.R                            # Test decision criterion difference with/without 300 ms RT criterion
functions.R                                     # Shared helper functions (sourced by other scripts)
```

Model-fit `.rds` files are not stored in the GitHub version of this repository because GitHub doesn't like large files. If you are viewing this project on GitHub and need the model fits, use the OSF archive instead: https://doi.org/10.17605/OSF.IO/3M6FU

The full repository on OSF includes the `model_fits/` `.rds` files. If you are working from the GitHub copy, you can either obtain the model fits from OSF or rerun the analysis scripts locally to regenerate them.

## Experiment code

The folder `exp_code` contains two folders. The code of the first session (`prime_control`) and the code to run sessions 2 onwards (`prime_trained`). To run the experiment go inside any of the folders and execute the `main.py` script (`python main.py`). 

## Data Overview

In session 1 (`raw_control.csv`), in the first half of the experiment, participants performed a mask discrimination task followed by a prime detection task. Then, in the second part of the experiment they performed a prime discrimination task. In sessions 2 onwards (`raw_train.csv`), participants performed a prime discrimination or mask discrimination task in different alternating blocks.

Two raw datasets are provided:

* **`raw_control.csv`** — mask discrimination + prime detection task, and prime discrimination task.
* **`raw_train.csv`** — prime and mask discrimination tasks.

Each row corresponds to a single trial.

The `data/processed/` folder contains filtered and reshaped versions of the raw data, produced by `01_filter_data.R`:

| File                    | Contents                                              |
| ----------------------- | ----------------------------------------------------- |
| `control_mask.csv`      | Control session — mask discrimination trials          |
| `control_prime_det.csv` | Control session — prime detection trials              |
| `control_prime_disc.csv`| Control session — prime discrimination trials         |
| `train_mask.csv`        | Trained sessions — mask discrimination trials         |
| `train_mask_recoded.csv`| Trained sessions — mask trials with recoded responses |
| `train_prime.csv`       | Trained sessions — prime discrimination trials        |

### File: `raw_control.csv`

#### Columns

| Column            | Description                                                                              |
| ----------------- | ---------------------------------------------------------------------------------------- |
| `participant`     | Numeric participant identifier (1–6).                                                    |
| `block_count`     | Sequential block number within the session (1–12).                                       |
| `trial_count`     | Trial index                                                                              |
| `trial_aborted`   | `TRUE` if the trial was aborted (response timeout).                                      |
| `soa`             | Stimulus-onset asynchrony (in seconds) between prime and mask (0.0125, 0.025, 0.0375, 0.05, 0.0625, 0.075).                           |
| `congruent`       | `TRUE` if prime and mask pointed in the same direction.                                  |
| `task`            | Indicates which was performed (`"mask"` or `"prime"`).                      |
| `prime_presence`  | Whether the prime was shown (`"present"` / `"absent"`).                                  |
| `prime_direction` | Direction of the prime stimulus (`"left"` / `"right"`).                                  |
| `mask_direction`  | Direction of the mask stimulus (`"left"` / `"right"`).                                   |
| `stim_position`   | Vertical position of the stimulus (`"top"` / `"bottom"`).                                |
| `mask_answer`     | Participant’s response in the mask task (`"left"`, `"right"`).                           |
| `mask_rt`         | Reaction time (s) for mask response.                                                     |
| `prime_answer`    | Participant’s response in the prime task (if applicable).                                |
| `prime_rt`        | Reaction time (s) for prime response.                                                    |
| `mask_accuracy`   | `TRUE` if the mask response was correct.                                                 |
| `prime_accuracy`  | `TRUE` if the prime response was correct.                                                |

---

### File: `raw_train.csv`

#### Columns

| Column            | Description                                                         |
| ----------------- | ------------------------------------------------------------------- |
| `participant`     | Numeric participant identifier (1–6).                               |
| `session`         | Session number (1–6).                                               |
| `trial_count`     | Trial index.                                                        |
| `trial_aborted`   | `TRUE` if the trial was aborted (response timeout).                 |
| `soa`             | Stimulus-onset asynchrony (in seconds) between prime and mask (0.0125, 0.025, 0.0375, 0.05, 0.0625, 0.075).                           |
| `congruent`       | `TRUE` if prime and mask pointed in the same direction.             |
| `task`            | Indicates which was performed (`"mask"` or `"prime"`).                      |
| `prime_direction` | Direction of the prime stimulus (`"left"` / `"right"`).             |
| `mask_direction`  | Direction of the mask stimulus (`"left"` / `"right"`).              |
| `stim_position`   | Position of the target (`"top"` / `"bottom"`).                      |
| `answer`          | Participant’s response (`"left"` / `"right"`).                      |
| `rt`              | Reaction time (s).                                                  |
| `accuracy`        | `TRUE` if the response was correct.                                 |

---

### Notes

* All reaction times (`mask_rt`, `prime_rt`, `rt`) are measured in **seconds**.
* Trials where `trial_aborted == TRUE` should be excluded from behavioral analyses.

---

## Contact

If you have any questions or run into any issues using this repository, feel free to reach out — I'm happy to help :) You can find my contact email in the correspondence section of the paper.
