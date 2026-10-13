# drasR

## Drift-Aware Risk-Guaranteed Acceptance Sampling

`drasR` implements Drift-Aware Risk-Guaranteed Acceptance Sampling for adaptive lot disposition under process drift.

The package extends classical acceptance sampling by adding:

- producer-risk and consumer-risk assessment,
- EWMA-based process drift monitoring,
- adaptive inspection tightening,
- Monte Carlo evaluation,
- policy tuning,
- low-cost, balanced, and high-safety policy selection,
- safety-cost frontier visualisation.

## Installation

```r
# Development version
# install.packages("devtools")
# devtools::install_github("YOUR-GITHUB-USERNAME/drasR")
```

## Basic example

```r
library(drasR)

plan <- find_plan(
  aql = 0.01,
  ltpd = 0.05,
  alpha = 0.05,
  beta = 0.10
)

plan
```

## Compare fixed acceptance sampling with DRAS

```r
set.seed(123)

result <- run_dras_experiment(
  n_reps = 100,
  scenario = "sudden_deterioration",
  seed = 123
)

result
```

## Tune DRAS policies

```r
tuned <- tune_dras_policy(
  n_reps = 100,
  scenario = "sudden_deterioration",
  seed = 123
)

selected <- select_dras_policy(tuned)

selected
```

## Plot safety-cost frontier

```r
plot_dras_frontier(tuned, selected)
```

## Interpretation

`drasR` helps users study how adaptive inspection can reduce false acceptance of bad lots under process drift while making the inspection-cost trade-off explicit.

## Status

This package is under active development. The current version focuses on simulation-based evaluation and methodological development.
