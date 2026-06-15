---
name: annotate
description: Annotate published version of the paper with different highlights and goodies
---

# Creating Annotated Papers (`*-annotated.Rnw`)
Annotated versions highlight core contributions and implementation details for peer review or pedagogical use.
1. **Filename**: Append `-annotated` to the base paper name (e.g., `OLA-26-annotated.Rnw`).
2. **Setup**: Use `\input{lib/annotations.tex}` in the preamble. You must also adjust page margins (e.g., using `geometry`) to provide sufficient `marginparwidth` for the notes.
3. **Usage Guidelines**:
   - `\keyfinding`: Highlight high-level conclusions or empirical results.
   - `\methodstep`: Document specific experimental or methodological shifts.
   - `\codelink`: Provide direct links to code blocks, R chunks, or external repository files.
   - `\important`: Call out critical caveats, reproducibility notes, or warnings.
   - `\reference`: Cite the original publication or venue.
4. **LaTeX Syntax Safety**: Annotations are LaTeX commands. You MUST escape special characters like underscores (`_`) or wrap technical terms in `\texttt{}` to avoid compilation errors (e.g., use `max\_gens` or `\texttt{max_gens}`).
5. **Mandatory Standard**: Do not redefine these commands locally; use the shared `lib/annotations.tex` to maintain a consistent look across the project.
