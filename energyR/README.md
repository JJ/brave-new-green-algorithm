# energyR

<!-- badges: start -->
<!-- badges: end -->

**energyR** provides reusable R utilities for loading, processing, and
visualising energy-consumption data produced by population-based
metaheuristics measured via Intel/AMD RAPL (Running Average Power Limit)
counters.

The package was developed to support the research papers in the
[Brave New Green Algorithm](https://github.com/JJ/brave-new-green-algorithm)
project, where the Brave New Algorithm (BNA) — a caste-stratified
evolutionary algorithm implemented in Julia — is studied from an energy
consumption perspective. The utilities are general enough to be used with any
experiment that follows the same interleaved-baseline measurement protocol.

## Installation

Until the package is released on CRAN, install directly from the project
repository:

```r
# install.packages("remotes")
remotes::install_github("JJ/brave-new-green-algorithm", subdir = "energyR")
```

## Quick start

```r
library(energyR)

# Load baseline CSV (drops algorithm-parameter columns)
baseline_data <- load_bna_csv("path/to/baseline.csv")

# Summarise baseline
baseline_summary <- summarize_baseline(baseline_data)

# Subtract baseline from workload measurements
workload_data <- load_bna_csv("path/to/workload.csv", drop_baseline_cols = FALSE)
workload_data <- compute_deltas(baseline_summary, workload_data)

# Statistical comparison
combined <- rbind(
  transform(baseline_data, work = "baseline"),
  workload_data
)
wilcoxon_tests(combined, "PKG")

# Visualise
workload_data <- add_pop_dim_label(workload_data)
plot_delta_energy(workload_data, geom = "violin")
```

For interleaved-run experimental designs (where baseline and workload rows
alternate in the same file), use `process_deltas()` instead of
`compute_deltas()`.

## Main functions

| Function | Description |
|---|---|
| `load_bna_csv()` | Load a BNA experiment CSV, optionally dropping baseline-only columns |
| `summarize_baseline()` | Descriptive statistics for baseline data by dimension × pop. size |
| `compute_deltas()` | Subtract summarised baseline from workload measurements |
| `process_deltas()` | Compute deltas from interleaved baseline/workload data |
| `process_covariates()` | Add prev/post baseline covariate columns for regression |
| `create_summary()` | Summarise delta-energy workload results |
| `wilcoxon_tests()` | Wilcoxon rank-sum tests across parameter combinations |
| `process_europar()` | Process and visualise EuroPar-format CSV files |
| `plot_energy_timeline()` | Plot PKG energy over cumulative time |
| `plot_delta_energy()` | Violin / boxplot of delta PKG energy distributions |
| `plot_temperature()` | Plot temperature sensor readings over time |
| `add_pop_dim_label()` | Add a "Pop. size: X, Dim.: Y" label column |
| `format_mean_sd_latex()` | Format mean ± SD strings for LaTeX tables |
| `null_baseline_columns` | Character vector of algorithm-specific column names to drop |

## Vignette

A full worked example, including how to download data from the GitHub
repository, is available in the vignette:

```r
vignette("energyR")
```

## License

GPL (>= 3)
