# Replication Data

**Paper:** “Which policies increase public support for reintegrating former armed group affiliates? Evidence from a conjoint survey in Nigeria”

**Authors:** Guy Schvitz, Govinda Clayton, Edward Morgan-Jones, Claudia Wiehler,
Ben Szreter, Chloé Chambraud

**Year:** 2026

## Overview

This repository contains the data and R code used to reproduce the analyses, tables, and figures for the paper.

All scripts must be run from the repository root by opening the `conjoint_paper_nigeria.Rproj` file. Do not run the scripts from another working directory or use `setwd()`, because file paths are defined relative to the project root.

## Repository structure

```text
├── README.md
├── R
│   ├── 00_setup.R
│   ├── 01_conjoint_analysis_main.R
│   ├── 02_conjoint_analysis_descriptives.R
│   ├── functions
│   │   └── 00_analysis_functions.R
│   ├── run_all.R
├── conjoint_paper_nigeria.Rproj
├── data
│   ├── conjoint_data_codebook.Rds
│   ├── conjoint_data_prepped.Rds
│   └── conjoint_labels_df.Rds
├── renv
├── renv.lock
└── results
    ├── figures
    └── tables
```

## Software environment

The repository uses [`renv`](https://rstudio.github.io/renv/) to record and restore the R package environment. Package versions are specified in `renv.lock`, while the `renv/` directory contains the project bootstrap files.

Open `conjoint_paper_nigeria.Rproj` in RStudio and start a fresh R session. Then run:

```r
source("R/00_setup.R")
```

This restores the package versions required by the analysis. The initial restoration may take several minutes because packages may need to be downloaded or compiled.

If `renv` is not installed, install it before running the setup script:

```r
install.packages("renv")
```

## Running the complete replication

After restoring the package environment, reproduce all analyses by running:

```r
source("R/run_all.R")
```

The scripts are executed in the following order:

1. Main conjoint analysis.
2. Descriptive analysis.

Generated figures are written to `results/figures/`, and generated tables are written to `results/tables/`. These folders are automatically created in the project's root directory when running the scripts.

## Running individual analyses

Each analysis can also be run separately from a fresh R session after completing the setup step.

### Main conjoint analysis

```r
source("R/01_conjoint_analysis_main.R")
```

This script reproduces the main conjoint estimates, interaction tests, subgroup analyses, and associated figures and tables.

### Descriptive analysis

```r
source("R/02_conjoint_analysis_descriptives.R")
```

This script reproduces the descriptive figures for respondent characteristics, attitudes, survey implementation, and interview duration.

## Code organization

All script files are stored in the `R/` subfolder

Shared project functions are stored in `R/functions/00_analysis_functions.R` and are sourced by the analysis scripts.

All paths are relative to the repository root. The directory structure should therefore be preserved when copying or extracting the replication package.

## Data files

The `data/` directory contains the original survey files and the prepared R data objects used by the analysis.

- `conjoint_data_prepped.Rds` contains the prepared respondent- and conjoint-level analysis data.
- `conjoint_labels_df.Rds` contains the attribute and level labels used in tables and figures.
- `conjoint_data_codebook.Rds` contains variable descriptions and survey-question documentation.

The replication scripts read the prepared `.Rds` files directly. 

## Output files

The analysis scripts write all generated outputs to the `results/` directory:

- `results/figures/` contains the replicated figures.
- `results/tables/` contains statistical results and publication-ready tables.

Existing files with the same names may be overwritten when the scripts are rerun. To verify a clean replication, the contents of `results/figures/` and `results/tables/` can be removed before running `R/run_all.R`.

## Recommended replication procedure

For a complete replication in a clean environment:

1. Extract the repository without changing its internal directory structure.
2. Open `conjoint_paper_nigeria.Rproj` in RStudio.
3. Restart R to begin with a clean session.
4. Run `source("R/00_setup.R")`.
5. Run `source("R/run_all.R")`.
6. Review the generated files under `results/figures/` and `results/tables/`.
