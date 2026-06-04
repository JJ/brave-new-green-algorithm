# Gemini Project Instructions: Brave New Green Algorithm

This repository is an academic research project studying the energy consumption of population-based metaheuristics, specifically the **Brave New Algorithm (BNA)**.

## Project Context
- **External Algorithm**: BNA is implemented in Julia: [`BraveNewAlgorithm.jl`](https://github.com/CeciMerelo/BraveNewAlgorithm.jl).
- **This Repository**: Contains paper sources (.Rnw), experimental data (data/), and analysis code (R/, script/, and the `energyR` package).
- **Core Goal**: Measuring and analyzing energy usage (Intel RAPL PKG) vs. algorithm performance.

## Tech Stack
- **Analysis**: R (base R, `ggplot2`, `dplyr`, `kableExtra`, `reshape2`, `ggridges`, `marginaleffects`, `equatiomatic`).
- **Typesetting**: LaTeX via R/knitr (`.Rnw` files) and R Markdown (`.Rmd` files).
- **Building**: `Makefile` for PDF/HTML generation.
- **Package**: `energyR/` is a standalone R package for reusable analysis logic.

## Repository Layout
- `data/`: Experimental CSV/RDS datasets. **READ-ONLY**. Never overwrite or delete.
- `R/`: Reusable R helper functions sourced by scripts.
- `script/`: Standalone R scripts for specific papers/analyses.
- `energyR/`: Internal R package for analysis patterns.
- `*.Rnw`: LaTeX sources for conference papers.
- `*.Rmd`: R Markdown for explainers and progression docs.
- `preso/`: Static HTML presentations and images.
- `*.bib`: BibTeX bibliography files (organized by topic).

## Core Workflows

### 1. Working with Papers (.Rnw)
- Papers use the **Springer LNCS** style (`llncs.cls`, `splncs04.bst`).
- Code is embedded in knitr chunks. Extract to a standalone `.R` file when needed for debugging or external scripts.
- Use `make <filename>.pdf` to render.

### 2. Creating Annotated Papers (`*-annotated.Rnw`)
Annotated versions highlight core contributions and implementation details for peer review or pedagogical use.
1. **Filename**: Append `-annotated` to the base paper name (e.g., `OLA-26-annotated.Rnw`).
2. **Setup**: Use `\input{lib/annotations.tex}` in the preamble. You must also adjust page margins (e.g., using `geometry`) to provide sufficient `marginparwidth` for the notes.
3. **Usage Guidelines**:
   - `\keyfinding`: Highlight high-level conclusions or empirical results.
   - `\methodstep`: Document specific experimental or methodological shifts.
   - `\codelink`: Provide direct links to code blocks, R chunks, or external repository files.
   - `\important`: Call out critical caveats, reproducibility notes, or warnings.
   - `\reference`: Cite the original publication or venue.
4. **Mandatory Standard**: Do not redefine these commands locally; use the shared `annotations.tex` to maintain a consistent look across the project.

### 3. Creating Explainers (.Rmd)
Follow these structural rules for `*-explainer.Rmd`:
1. Start with 2 context sentences (algorithm/problem + metrics).
2. Define key terms on first use (`baseline`, `delta`, `max_gens`).
3. Use 3-5 visuals. Each visual needs:
   - One question line.
   - One short interpretation tied to evidence.
   - One bounded takeaway (using "often", "in this setup").
4. Close with practical implications and reproducibility notes.
5. Use **American spelling** and **inline citations**.
6. **Typographics**: Author field should join names with ` & `. "Introduction" header must reference the paper title. Include standard bib files in YAML.

### 3. Cross-paper Progression Documents
- Frame changes as **deeper understanding**, never "invalidation".
- Apply new methodologies operationally to earlier data (don't just juxtapose).
- Use **changepoint detection** (not LOESS sign changes) for thermal-regime shifts.
- Use published venue names (e.g., LION), not submission prefixes (e.g., evoapps).

### 4. Developing `energyR` Package
- **One function per file** in `energyR/R/`.
- **ASCII ONLY**: No non-ASCII characters or escape sequences in code, comments, or vignettes.
- Use `stats::` or `utils::` prefixes for base-R functions.
- Man files are auto-generated; do not commit them.

## Conventions & Standards

### Energy Measurement
- **PKG**: Intel RAPL package-level energy (Joules).
- **Delta PKG**: Workload PKG - Baseline PKG.
- **Hysteresis**: Thermal state affects measurements. Mixed/interleaved runs are intentional.

### R Programming
- Source helpers from `R/` using `source()`.
- Central patterns: `compute_deltas()` (script-local) and `process_deltas()` (R/ helper) - **do not conflate**.
- Statistical tests: `wilcox.test()`.
- Tables: `kableExtra::kable(..., "latex")`.
- Plots: `ggplot2`, save with `ggsave()` to `preso/img/`.

### LaTeX & BibTeX
- LaTeX artifacts (`.tex`, `.pdf`, `.aux`, etc.) are `.gitignore`d. Do not commit them.
- Citations: Multiple bib files exist. Use the most relevant one (e.g., `energy.bib` for energy topics).

## Building & Validation
- `make all`: Renders all papers and HTMLs.
- `make deps`: Installs system and R dependencies.
- Validation for papers: Ensure they compile without errors and bibliography is correct.
- Validation for `energyR`: Run `R CMD check` (managed by CI).
