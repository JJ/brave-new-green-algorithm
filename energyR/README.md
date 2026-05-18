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

### Data loading & preparation

| Function | Description |
|---|---|
| `load_bna_csv()` | Load a BNA experiment CSV, optionally dropping baseline-only columns |
| `add_cumulative_time()` | Add `cum_seconds = cumsum(seconds)` column (needed for every timeline plot) |
| `filter_zero_energy()` | Remove zero-PKG artefact rows (*no0* strategy) |
| `add_log_diff()` | Add `log10(diff_fitness)` column |
| `add_pop_dim_label()` | Add a "Pop. size: X, Dim.: Y" label column |
| `null_baseline_columns` | Character vector of algorithm-specific column names to drop |

### Baseline & delta computation

| Function | Description |
|---|---|
| `summarize_baseline()` | Descriptive statistics for baseline data by dimension × pop. size |
| `compute_deltas()` | Subtract summarised baseline from separated workload measurements |
| `process_deltas()` | Compute deltas from symmetric interleaved (triplet) data |
| `compute_adjacent_deltas()` | Compute deltas from adjacent-pair (mixed/inverted) data |
| `process_covariates()` | Add prev/post baseline covariate columns for regression |
| `process_europar()` | Process and visualise EuroPar-format CSV files |

### Statistical analysis & summaries

| Function | Description |
|---|---|
| `create_summary()` | Summarise delta-energy workload results |
| `wilcoxon_tests()` | Wilcoxon rank-sum tests across parameter combinations |
| `top_anova_terms()` | Extract top-N significant ANOVA terms with % variance explained |
| `energy_effort_by_fitness()` | Energy effort required to reach each fitness level |

### Table formatting

| Function | Description |
|---|---|
| `format_mean_sd()` | Format "mean (sd)" strings for plain-text tables |
| `format_mean_sd_latex()` | Format "$mean \\pm sd$" strings for LaTeX tables |
| `pivot_comparison_table()` | Reshape long summary to wide comparison table (via reshape2::dcast) |

### Visualisation

| Function | Description |
|---|---|
| `plot_energy_timeline()` | Plot PKG energy over cumulative time |
| `plot_delta_energy()` | Violin / boxplot of delta PKG energy distributions |
| `plot_energy_vs_fitness()` | Scatter of energy vs log(diff_fitness) |
| `plot_workload_vs_baseline()` | Workload points with per-combination baseline band |
| `plot_ridgeline_delta()` | Ridgeline density of Δ PKG (requires ggridges) |
| `plot_temperature()` | Plot temperature sensor readings over time |

## Building, testing & CRAN submission

A `Makefile` inside `energyR/` wraps common development tasks:

```bash
# From the energyR/ directory:
make deps          # install all dev dependencies (devtools, roxygen2, …)
make document      # regenerate NAMESPACE + man/ from roxygen2 tags
make test          # run the testthat suite
make check         # R CMD check (standard)
make check-cran    # R CMD check --as-cran (pre-submission gate)
make build         # build source tarball → /tmp/energyR-build/
make install       # install locally
make pkgdown       # build pkgdown website → docs/
make clean         # remove check artefacts and tarballs
```

A GitHub Actions workflow (`.github/workflows/R-CMD-check.yml`) runs
`R CMD check --as-cran` on Ubuntu, macOS, and Windows against both the
current CRAN release and R-devel on every push or pull request that touches
the `energyR/` subtree.

## Vignette

A full worked example, including how to download data from the GitHub
repository, is available in the vignette:

```r
vignette("energyR")
```

## License

GPL (>= 3)
