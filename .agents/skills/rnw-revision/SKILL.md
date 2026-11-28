---
name: rnw-revision
description: |
  Skill to automatically review .Rnw LaTeX papers for orthographic, typographic, and consistency issues.
  It checks spelling, grammar, figure/table citations, and ensures that the abstract and conclusions accurately reflect the content.
  The skill produces a markdown report of findings and optionally fixes simple issues in-place.
---

# Overview

This skill provides a command‑line R script that scans a specified `.Rnw` file (or all `.Rnw` files in a directory) and performs the following checks:

1. **Spelling & Grammar** – Uses the `hunspell` dictionary (via the `hunspell` R package) to flag unknown words and common typos.
2. **Typographic Consistency** – Detects non‑ASCII characters, double spaces, repeated punctuation, and inconsistent use of en‑dash/em‑dash.
3. **Figure / Table References** – Parses LaTeX `\label{}` and `\ref{}` pairs to find:
   * Figures or tables that are defined but never referenced.
   * References to undefined labels.
   * Mismatched numbering (e.g., a reference to a figure using a table label).
4. **Abstract / Conclusion Alignment** – Extracts the abstract and conclusion sections, runs a simple similarity check (TF‑IDF cosine similarity) against the main body to detect large divergence.
5. **Citation Consistency** – Verifies that every `\cite{}` key exists in the supplied bibliography files (`.bib`).

The tool outputs a **Markdown report** (`<paper>-review.md`) listing all findings, their line numbers, and suggested fixes. For trivial issues (spelling, double spaces, missing references) the script can apply in‑place edits when invoked with the `--apply` flag.

# Installation

```bash
# From the repository root
cd .agents/skills/rnw-revision
Rscript -e "install.packages(c('hunspell','stringr','tidyverse','tm','tools'), repos='https://cloud.r-project.org')"
```

# Usage

```bash
# Review a single paper (dry‑run, no changes)
Rscript review_rnw.R --file path/to/paper.Rnw

# Apply automatic fixes where safe
Rscript review_rnw.R --file path/to/paper.Rnw --apply

# Review every .Rnw file under the repository
Rscript review_rnw.R --dir .
```

The script creates a `review_reports/` directory (created next to the paper) containing the markdown report.

# Implementation Details

- **Parsing**: The script reads the `.Rnw` file line‑by‑line, stripping knitr code chunks (`<<>>=` … `@`) before analysis.
- **Spelling**: `hunspell::hunspell` with the default English dictionary; user‑provided custom dictionary can be added via `--dict path/to/extra.dic`.
- **Reference Extraction**:
  - Labels are captured with regex `\\label\{([^}]+)\}`.
  - References with `\\ref\{([^}]+)\}` and `\\autoref\{([^}]+)\}`.
  - Figure/table classification is inferred from surrounding environments (`figure`, `table`).
- **Abstract & Conclusion**: The script looks for LaTeX sections `\begin{abstract}` / `\end{abstract}` and `\section{Conclusion}` (or similar). Content is tokenised and compared to the main body using `tm::TermDocumentMatrix`.
- **Reporting**: Each issue is reported with:
  * **Type** (Spelling, Typo, Reference, Alignment, Citation)
  * **Location** (file, line number) 
  * **Message** and **Suggested fix**.

# Extending the Skill

- Add language support by installing additional Hunspell dictionaries and passing `--lang xx`.
- Hook the script into the project's `Makefile` (e.g., `make check-paper`) for CI enforcement.
- Integrate with the Antigravity SDK to expose the skill as a sub‑agent.

# Limitations

- The similarity check is heuristic; it may flag legitimate abstract rewrites.
- Only English language support provided out‑of‑the‑box.
- Complex LaTeX macro definitions that generate labels dynamically are not fully resolved.

# License

MIT © 2026
