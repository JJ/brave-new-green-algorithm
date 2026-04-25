# Copilot Cloud Agent Instructions

## Repository Overview

This is an **academic research repository** for a series of papers studying the energy consumption of population-based metaheuristics, specifically the *Brave New Algorithm* (BNA) — a caste-stratified evolutionary algorithm inspired by Aldous Huxley's *Brave New World*. The BNA implementation lives in the external Julia package [`BraveNewAlgorithm.jl`](https://github.com/CeciMerelo/BraveNewAlgorithm.jl); this repository contains only the paper sources, experimental data, and analysis code.

## Tech Stack

| Layer | Technology |
|---|---|
| Algorithm (external) | Julia (`BraveNewAlgorithm.jl`) |
| Data analysis & statistics | R (base R, `ggplot2`, `dplyr`, `kableExtra`, `reshape2`, `ggridges`, `marginaleffects`, `equatiomatic`) |
| Paper typesetting | LaTeX via R/knitr — `.Rnw` files (Sweave/knitr) and `.Rmd` files (R Markdown) |
| Presentation | HTML (`preso/index.html`) |
| CI/CD | GitHub Actions — one workflow (`static.yml`) that renders `.Rmd` files and deploys to GitHub Pages |

## Repository Layout

```
.
├── data/                  # Experimental CSV and RDS datasets (read-only; never overwrite)
├── R/                     # Reusable R helper functions (sourced from scripts)
│   ├── plot_temperature.R
│   ├── process_covariates.R
│   ├── process_deltas.R
│   └── process_europar.R
├── script/                # Per-paper standalone R scripts
│   ├── europar-2026.R
│   └── icsme.R
├── preso/                 # Static HTML presentation (dist/, img/, index.html)
├── *.Rnw                  # Sweave/knitr LaTeX paper sources (one per conference)
├── *.Rmd                  # R Markdown documents (explainer, extended versions)
├── Makefile               # Generic build targets: make all/pdfs/html/deps/<file>.pdf/<file>.html
├── ola-26.R               # Standalone R code extracted from ola-26.Rnw
├── example.jl             # Julia usage example for BraveNewAlgorithm.jl
├── *.bib                  # BibTeX bibliography files (GAs.bib, energy.bib, etc.)
├── llncs.cls / splncs04.bst  # Springer LNCS LaTeX class & bibliography style
├── academic.css           # CSS for the GitHub Pages index
└── .github/workflows/static.yml  # CI: renders Rmd → HTML, deploys to Pages
```

## Paper Sources (`.Rnw` / `.Rmd`)

Each file corresponds to a specific conference submission:

| File | Conference |
|---|---|
| `ola-26.Rnw` | OLA 2026, Chania, Crete |
| `ola-26-explainer.Rmd` | Blog/social-media explainer for OLA paper |
| `ola-26-extended.Rmd` | Extended OLA paper |
| `europar-2026.Rnw` | EuroPar 2026 |
| `evoapps-26.Rnw` | EvoApps 2026 |
| `icsme-2026.Rnw` | ICSME 2026 |
| `ppsn-2026.Rnw` | PPSN 2026 |
| `cec-2026.Rnw` | CEC 2026 |
| `lion-26.Rnw` | LION 2026 |
| `walcom-26.Rnw` | WALCOM 2026 |

## Data Files

- **CSV files** (`data/*.csv`): Raw experimental output from `BraveNewAlgorithm.jl` runs. Column naming convention: `PKG` (package energy in Joules), `seconds`, `population_size`, `dimension`, `alpha`, `max_gens`, `work` (experiment label), `evaluations`, `generations`, `different_seeds`, `diff_fitness`.
- **RDS files** (`data/*.rds`): Pre-processed R data frames for EuroPar, ICSME, PPSN, and other experiments. Load with `readRDS()` / `load()`.
- **Do not modify or delete data files.** They represent completed experimental runs that cannot be trivially re-run.

## Common R Patterns

- All `.Rnw` files embed R code as knitr chunks; extract pure R to `ola-26.R` or `script/` when needed.
- Helper functions in `R/` are sourced with `source("R/process_deltas.R")` etc.
- `compute_deltas()` (defined in `ola-26.R`) and `process_deltas()` (in `R/process_deltas.R`) are the two central energy delta computation patterns — do not conflate them.
- Statistical tests use `wilcox.test()` (Wilcoxon rank-sum); tables are produced with `kableExtra::kable(..., "latex")`.
- Plots use `ggplot2` and are saved with `ggsave("preso/img/<name>.png", width=6, height=4.5)`.

## Building / Rendering

Use `make` (see `Makefile` in the repository root):

| Target | Effect |
|---|---|
| `make all` | Render all `.Rnw` papers to PDF and all `.Rmd` files to HTML |
| `make pdfs` | Render all `.Rnw` papers to PDF |
| `make html` | Render all `.Rmd` files to HTML |
| `make deps` | Install system (`pandoc`, `libuv1-dev`) and R package dependencies |
| `make foo.pdf` | Render a single paper, e.g. `make ola-26.pdf` |
| `make foo.html` | Render a single document, e.g. `make ola-26-explainer.html` |

The `.Rnw` pipeline is: `knitr::knit` → `pdflatex` → `bibtex` → `pdflatex` × 2.

## CI Workflow (`.github/workflows/static.yml`)

- Triggered on push to `main` or `workflow_dispatch`.
- Installs pandoc + R, renders all `*.Rmd` files to HTML, converts `README.md` to `index.html` via pandoc, then deploys the whole repository to GitHub Pages.
- R package cache key is based on the R version and hashes of all `*.Rmd` / `*.R` files.
- `.tex`, `.pdf`, and generated `index.html` are `.gitignore`d; they are produced at CI time.

## Conventions & Gotchas

1. **LaTeX outputs are gitignored**: `.tex`, `.pdf`, `.bbl`, `.aux`, `.log`, `.gz` are all excluded. Do not try to commit compiled PDFs.
2. **LNCS style**: Papers use Springer LNCS (`llncs.cls`, `splncs04.bst`). Keep `\documentclass[runningheads]{llncs}` and the standard LNCS front-matter.
3. **Bibliography files**: Multiple `.bib` files exist for different topic areas (`GAs.bib`, `energy.bib`, `julia.bib`, `metaheuristics.bib`, `ours.bib`, etc.). When adding new citations, place them in the most relevant file.
4. **Energy measurement terminology**: PKG = Intel RAPL package-level energy (Joules); delta PKG (`delta_PKG`) = workload energy minus baseline. Negative deltas can occur and are discussed in the papers as a known artefact.
5. **Hysteresis**: Hardware thermal state (temperature) affects measurements. Mitigation strategies (mixed/interleaved runs, filtering zero-energy rows) are intentional — do not remove them.
6. **`null_baseline_columns`**: A recurring R pattern that drops irrelevant columns from baseline data before merging. Keep it consistent across analyses.
7. **Julia example**: `example.jl` is illustrative only; the actual experiments are run externally via the `BraveNewAlgorithm.jl` package and produce the CSV/RDS files in `data/`.
