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

## Template for annotated versions of papers

Use the conventions in the [`OLA-26-annotated.Rnw`](OLA-26-annotated.Rnw),
including package and templates for anotations included there, as a template for
any other "annotated version of a paper" that's requested.

## Template for divulgative / explainer versions

For short-form explainers (for example `*-explainer.Rmd`), keep the structure simple:

1. Start with 2 short context sentences:
   - what algorithm/problem family is being studied;
   - what is jointly measured (fitness and energy).
2. Define key terms on first use in one line (`baseline`, `delta`, `max_gens`).
3. Use 3-5 visuals. For each visual, include:
   - one question line;
   - one short interpretation tied to the plotted evidence;
   - one bounded takeaway (use wording like `often`, `in this setup`, `for this runtime`).
4. Close with practical implications and a reproducibility note (runtime version + baseline protocol).

Prefer publication-ready text. Avoid process narration (for example "in this explainer we do ...").
Keep claims aligned with the paper source and scoped to measured hardware/runtime context.
Use American spelling in explainer prose.
When a chart shows unexplained visual bands/clusters, explicitly encode the likely driver (for example facet by `dimension`) instead of adding a second competing plot.
Include citations inline at the sentence where each claim appears; avoid "offline" or detached reference stanzas.

### Typographic conventions for all `*.Rmd` documents

- **Author field**: list all paper co-authors using ` & ` as separator, matching the
  corresponding paper's author list (for example, `lion-26-explainer.Rmd` uses the full
  five-author list from `lion-26.Rnw`; `ola-26-explainer.Rmd` uses the two-author list
  from `ola-26.Rnw`).
- **First section heading**: use `## Introduction` or a "Why" question such as
  `## Why this explainer?` or `## Why does this matter?`.  Avoid narrative headings
  such as `## The story so far`.
- **Inline citations**: use `[@key]` or `[@key1; @key2]` at the sentence level where the
  claim appears; do not collect references in a detached paragraph at section end.
- **Bibliography YAML block**: include the four standard `.bib` files
  (`references.bib`, `ours.bib`, `GAs.bib`, `ga-energy.bib`) whenever the document uses
  `[@…]` citations.

## Scientific grounding for LION explainers

When preparing explainers from `lion-26.Rnw`, keep the scientific framing tied to the paper and the exact LION datasets:

1. Use paper-accurate terminology:
   - algorithm: Brave New Algorithm (BNA), a stratified population-based stochastic optimization algorithm;
   - task: BBOB Sphere benchmark;
   - metrics: fitness and package energy (PKG, Joules).
2. Anchor data references to the concrete files used by the explainer:
   - baseline/control measurements: `data/lion-1.11.8-baseline.csv`;
   - workload runs: `data/lion-1.11.8-bna-fix-rand.csv`.
3. Define terms briefly where first used:
   - baseline: energy measured for the control setup used to calibrate deltas;
   - delta energy (`delta_PKG`): workload PKG minus matched baseline PKG.
4. Keep claims traceable to paper evidence (`lion-26.Rnw`/`lion-26.R`) and cite at the sentence where each claim appears.
5. For publication-grade summary comparisons across `max_gens`, mark statistically significant differences with asterisks (`p < 0.05`) and base the claim on Wilcoxon tests between groups with the same `dimension` and `population_size`.

## `energyR` R Package

The `energyR/` subdirectory contains a standalone R package that extracts reusable
analysis patterns from all BNA papers.

- **One function per file**: every exported function lives in `energyR/R/<function-name>.R`.
- **CRAN-compliance**: use `stats::` / `utils::` prefixes for base-R functions; suppress
  ggplot2 NSE globals via `utils::globalVariables()` in `energyR/R/energyR-package.R`.
- **No non-ASCII characters** anywhere in `energyR/R/*.R`, `energyR/tests/`, or
  `energyR/vignettes/`. This includes both literal UTF-8 bytes (e.g. `o` with accent,
  multiplication sign, plus-minus, em-dash) **and** `\uXXXX` escape sequences
  (e.g. `\u2081`, `\u0394`). The vignette rendering engine (knitr/pandoc) and the
  R CMD check example runner both fail when they encounter non-ASCII in strings or
  comments — this is not a CI-environment issue but a fundamental R toolchain
  limitation. Use plain ASCII equivalents: `Lopez` not `Lopez-with-accent`,
  `x` for multiplication, `+/-` not `+-symbol`, `delta` not the Greek letter,
  `log10` not `log\u2081\u2080`.
- **man/ files**: not committed; generated at check time by `roxygen2::roxygenise('.')` in the
  CI step before `R CMD check`.
- **CI**: `.github/workflows/R-CMD-check.yml` runs on ubuntu (release + devel) and macOS only
  (Windows removed). `r-lib/actions/setup-r-dependencies@v2` handles R package caching
  automatically.

## Cross-paper progression documents (`lion-ola-progression.Rmd`)

When writing or editing documents that trace the research progression from one paper to
another:

1. **Scope each "fix" to the progression story**: only include methodology changes that
   address artefacts or observations carried over from the earlier paper (LION-26).
   Changes introduced purely within the later paper (OLA-26) without a root cause in
   LION-26 do not belong in the progression narrative.

2. **Never use "invalidation" language**: do not write that any finding was "invalidated"
   or "disproved."  Instead, frame OLA-26 as providing a **deeper understanding** of how
   everything works — especially how energy is measured and what it depends on.
   Validation is always statistical within the framework of the stated research questions.

3. **Positive framing for methodology evolution**: describe methodology changes as
   revealing more about the measurement process, not as correcting errors or overturning
   conclusions.
