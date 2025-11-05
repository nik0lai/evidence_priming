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

## Experiment code

The folder `exp_code` contains two folders. The code of the first session (`prime_control`) and the code to run sessions 2 onwards (`prime_trained`). To run the experiment go inside any of the folders and execute the `main.py` script (`python main.py`). 

## Data Overview

In session 1 (`raw_control.csv`), in the first half of the experiment, participants performed a mask discrimination task followed by a prime detection task. Then, in the second part of the experiment they performed a prime discrimination task. In sessions 2 onwards (`raw_train.csv`), participants performed a prime discrimination or mask discrimination task in different alternating blocks.

Two datasets are provided:

* **`raw_control.csv`** — mask discrimination + prime detection task, and prime discrimination task.
* **`raw_train.csv`** — prime and mask discrimination tasks.

Each row corresponds to a single trial.

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
