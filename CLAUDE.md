# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## graphify

This project has a knowledge graph at graphify-out/ with god nodes, community structure, and cross-file relationships.

Rules:
- For codebase questions, first run `graphify query "<question>"` when graphify-out/graph.json exists. Use `graphify path "<A>" "<B>"` for relationships and `graphify explain "<concept>"` for focused concepts. These return a scoped subgraph, usually much smaller than GRAPH_REPORT.md or raw grep output.
- If graphify-out/wiki/index.md exists, use it for broad navigation instead of raw source browsing.
- Read graphify-out/GRAPH_REPORT.md only for broad architecture review or when query/path/explain do not surface enough context.
- After modifying code, run `graphify update .` to keep the graph current (AST-only, no API cost).

## Repository overview

Academic research repository for a series of papers studying the energy consumption of
population-based metaheuristics, specifically the **Brave New Algorithm (BNA)** — a
caste-stratified evolutionary algorithm. BNA itself is implemented externally in Julia
([`BraveNewAlgorithm.jl`](https://github.com/CeciMerelo/BraveNewAlgorithm.jl)); this repo
contains only paper sources, experimental data, analysis code, and the `energyR` R package.
`example.jl` is illustrative only — experiments are run externally and their output lands in `data/`.

## Common commands

Root-level paper/document builds (`Makefile`):
```
make all               # render every .Rnw paper to PDF and every .Rmd to HTML
make pdfs              # render all .Rnw -> PDF (knitr::knit -> pdflatex -> bibtex -> pdflatex x2)
make html              # render all .Rmd -> HTML (rmarkdown::render)
make <file>.pdf        # render a single paper, e.g. make ola-26.pdf
make <file>.html       # render a single explainer, e.g. make ola-26-explainer.html
make deps              # install pandoc/libuv1-dev + required R packages
```
`.qmd` files (e.g. `cec-two-stage-explainer.qmd`) render with `quarto render <file>.qmd`, not via the Makefile.

`energyR` package (`energyR/Makefile`, run from `energyR/`):
```
make test              # devtools::test('.') - full testthat suite
make document          # roxygen2::roxygenise('.') -> NAMESPACE + man/ (man/ is not committed)
make check             # R CMD check (--no-manual)
make check-cran        # R CMD check --as-cran, errors on warning (pre-submission gate)
make build / install   # build tarball / install locally
```
Run a single testthat file (from `energyR/`):
```
Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-compute_deltas.R')"
```

## Architecture

- `data/` — raw experimental output (CSV) and pre-processed data frames (RDS), keyed by paper/venue
  (`lion-*`, `ola-*`, `cec-*`, `europar_*`, `ppsn_*`, `icsme_*`, ...). **Read-only**: these represent
  completed experimental runs that cannot be trivially re-run; never overwrite or delete them.
  CSV column convention: `PKG` (RAPL package energy, Joules), `seconds`, `population_size`, `dimension`,
  `alpha`, `max_gens`, `work` (experiment label), `evaluations`, `generations`, `different_seeds`, `diff_fitness`.
- `R/` — small reusable helpers sourced ad hoc from paper code via `source("R/process_deltas.R")` etc.
- `energyR/` — the standalone, CRAN-style package that supersedes ad hoc `R/`/`script/` logic for new work.
  One exported function per file in `energyR/R/`. Man pages are generated, not committed.
- `script/` — standalone per-paper R scripts (not part of the `energyR` package).
- `*.Rnw` — Springer LNCS LaTeX paper sources (`llncs.cls`, `splncs04.bst`), one per conference
  (`ola-26`, `lion-26`, `europar-2026`, `evoapps-26`, `icsme-2026`, `ppsn-2026`, `cec-2026`, `walcom-26`,
  `nihpc-2026`, `pecs-2026`/`pecs-2-2026`).
- `*-explainer.Rmd` / `*-extended.Rmd` — divulgative/expanded companions to a single paper.
- `*-progression.Rmd` (e.g. `lion-ola-progression.Rmd`) — cross-paper documents tracing how understanding
  evolved between papers; governed by the `.agents/skills/rmd-progress/SKILL.md` skill.
- `*-annotated.Rnw` (e.g. `OLA-26-annotated.Rnw`) — annotated paper versions using `\input{lib/annotations.tex}`
  and macros `\keyfinding`, `\methodstep`, `\codelink`, `\important`, `\reference`; escape underscores in
  technical names (`max\_gens`) for LaTeX safety.
- `contexts/` — per-paper JSON manifests (key findings, variable index, statistical models) used when
  drafting explainers/annotated versions/progression docs.
- `bonus/` — standalone rendered HTML simulations/explorations, always implemented in R using `energyR`.
- `preso/` — static HTML presentations; plots for these are saved via `ggsave("preso/img/<name>.png", width=6, height=4.5)`.
- `*.bib` — topical bibliography files (`GAs.bib`, `energy.bib`, `julia.bib`, `metaheuristics.bib`,
  `ours.bib`, `references.bib`, `ga-energy.bib`, ...); add new citations to the most relevant file.
- Two independent CI workflows: `.github/workflows/static.yml` renders all `*.Rmd`/`*.qmd` and converts
  `README.md` -> `index.html` via pandoc, deploying the whole repo to GitHub Pages on every push to `main`;
  `.github/workflows/R-CMD-check.yml` runs `R CMD check` for `energyR` (ubuntu release/devel + macOS) but
  only triggers on changes under `energyR/**`.

## Conventions

- **LaTeX build artifacts are gitignored**: `.tex`, `.pdf`, `.bbl`, `.aux`, `.log`, `.synctex.gz` are all
  excluded (generated at build/CI time) — do not commit them.
- **Energy terminology**: PKG = Intel RAPL package-level energy (Joules); `delta_PKG` = workload PKG minus
  baseline PKG (can be legitimately negative — a known, discussed artefact, not a bug).
- **Hysteresis**: hardware thermal state affects measurements; mixed/interleaved run order and zero-energy
  filtering are intentional mitigations — do not remove them.
- `compute_deltas()` (script-local, in `ola-26.R`) and `process_deltas()` (`R/process_deltas.R` helper) are
  two distinct energy-delta computation patterns — do not conflate them.
- Stats: `wilcox.test()` (Wilcoxon rank-sum) between groups matched on `dimension`/`population_size`; tables
  via `kableExtra::kable(..., "latex")`.
- **`energyR` is ASCII-only**: no literal non-ASCII bytes and no `\uXXXX` escapes anywhere in
  `energyR/R/*.R`, `energyR/tests/`, or `energyR/vignettes/` — the knitr/pandoc vignette build and
  `R CMD check` example runner fail on either form. Use `Lopez` not the accented form, `x` for
  multiplication, `+/-`, `delta`, `log10`, etc.
- **New bonus/explainer/annotated/progression analysis code must be R using `energyR`** — not Python,
  Julia, or shell — so results stay reusable package functions rather than one-off scripts.
- `*-explainer.Rmd` structure: 2 context sentences -> define key terms on first use (`baseline`, `delta`,
  `max_gens`) -> 3-5 visuals (question line + evidence-tied interpretation + bounded takeaway using
  "often"/"in this setup") -> practical implications + reproducibility note. American spelling, inline
  citations. Author field joins names with `&`; first heading starts with "Introduction" and names the
  paper title; bib YAML includes `references.bib`, `ours.bib`, `GAs.bib`, `ga-energy.bib` when citing.
- Progression documents (`*-progression.Rmd`): see `.agents/skills/rmd-progress/SKILL.md` — frame
  methodology changes as deeper understanding, never "invalidation"; apply the later paper's methodology
  operationally to the earlier paper's data rather than just juxtaposing results; use published venue
  names, not submission-era file prefixes (e.g. `evoapps-*` data may back a paper published at LION).
