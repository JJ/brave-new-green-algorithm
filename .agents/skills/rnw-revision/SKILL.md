---
name: rnw-revision
description: |
  Review .Rnw LaTeX papers for orthographic, typographic, and consistency issues.
  Checks spelling, grammar, labels, citations, and abstract/conclusion alignment,
  producing a review report or applying fixes directly in-place.
---

# Review and Revise .Rnw Papers

Directly inspect and revise `.Rnw` (LaTeX + knitr) papers for linguistic, typographic, and structural consistency.

## Workflow

1. **Target Identification**:
   - Identify the target file from the user's prompt (e.g., `smartgreens-27.Rnw`).
   - If not specified, locate `.Rnw` files in the repository root or target directory.

2. **Reading & Parsing**:
   - Read the `.Rnw` file.
   - Ignore or preserve knitr code chunks (`<<...>>= ... @`)—do not modify executable R code unless specifically asked.
   - Identify associated `.bib` files referenced in `\bibliography{...}` or present in the repository.

3. **Core Checks**:
   - **Spelling & Grammar**:
     - Check English spelling, grammar, and academic phrasing in text and captions.
     - Respect LaTeX commands, math environments (`$...$`), and inline code (`\texttt{...}`).
   - **Typographic Consistency**:
     - Convert straight quotes (`"text"`) to LaTeX directional quotes (`` ``text'' ``).
     - Ensure correct dash usage: hyphens for compound words, en-dashes (`--`) for ranges and numbers, em-dashes (`---`) for parenthetical thoughts.
     - Add non-breaking spaces (`~`) before citations (e.g., `~\cite{...}`) and cross-references (e.g., `Figure~\ref{...}`, `Table~\ref{...}`, `Section~\ref{...}`).
     - Remove double spaces and fix repeated punctuation.
   - **Figures & Tables**:
     - Verify every `\label{...}` has a corresponding `\ref{...}` or `\autoref{...}` in text.
     - Check that all `\ref{...}` targets exist.
     - Ensure labels match environment types (e.g., `fig:...` in figures, `tab:...` in tables).
   - **Citations**:
     - Cross-check `\cite{...}` keys against existing entries in the repository's `.bib` files.
     - Flag missing or malformed citation keys.
   - **Abstract & Conclusion Alignment**:
     - Verify that key takeaways, quantitative claims, and terminology in the abstract match those stated in the conclusions and results.

4. **Execution Modes**:
   - **Dry Run / Review Report (default)**:
     - Output a clear markdown summary detailing all findings grouped by category, with line numbers, context, and recommended fixes.
   - **Apply Mode (`--apply`)**:
     - If `--apply` is present in the prompt, directly edit the `.Rnw` file using safe in-place edits (typos, quotation marks, dashes, non-breaking spaces `~`, punctuation).
     - Report applied fixes clearly, and list any substantive issues requiring author input.
