---
name: annotate
description: Annotate published version of the paper with different highlights and goodies
---

# Creating Annotated Papers (`*-annotated.Rnw`)
Annotated versions highlight core contributions and implementation details for peer review or pedagogical use.
1. **File format**: the output file needs to be in the same format as the input
   file, that is, .Rnw or Latex with knitr.
2. **Filename**: Append `-annotated` to the base paper name (e.g., `OLA-26-annotated.Rnw`).
3. **Setup**: Use `\input{lib/annotations.tex}` in the preamble. You must also adjust page margins (e.g., using `geometry`) to provide sufficient `marginparwidth` for the notes.
4. **Usage Guidelines**:
   - `\keyfinding`: Highlight high-level conclusions or empirical results.
   - `\methodstep`: Document specific experimental or methodological shifts.
   - `\codelink`: Provide direct links to code blocks, R chunks, or external repository files.
   - `\important`: Call out critical caveats, reproducibility notes, or warnings.
   - `\reference`: Cite the original publication or venue.
5. The annotations should be made as close to the text they're annotating as
   possible, not at the beginning of the section. 
6. The experimental setup should be summarized using `\methodstep`.
7. **The key findings is the most important part of the annotation**. It is
   mandatory that these key findings are identified and marked as such in the paper.
8. **LaTeX Syntax Safety**: Annotations are LaTeX commands. You MUST escape
   special characters like underscores (`_`) or wrap technical terms in
   `\texttt{}` to avoid compilation errors (e.g., use `max\_gens` or
   `\texttt{max_gens}`). Use `\protect` for URLs to avoid Latex errors when urls
   are used or other commands that will break in a moving environment.
9. **Mandatory Standard**: Do not redefine these commands locally; use the shared `lib/annotations.tex` to maintain a consistent look across the project.
