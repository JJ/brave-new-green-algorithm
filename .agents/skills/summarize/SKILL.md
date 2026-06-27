---
name: summarize
description: |
    Create summarized versions of papers that can be easily consumed by
    other LLMS
---

# Create minimum-token versions of papers

Create a JSON version of the paper that extracts the main methodological steps,
highlights, how research questions are answered and how data processing supports
the main points of the paper.

1. **File format** output files need to be in JSON.
2. **Filename** use the paper path, changing only extension.
3. **Setup** in most cases, you will need to read the file with the same
   name as the paper but extension .pdf to see how charts and tables are
   rendered.
4. Summarize all steps in the methodology, linking them to R code chunks in the
   paper.
5. Code chunks should be linked too to figures and tables they are producing,
   and either included directly or summarized.
6. If available, remember to include a prompt injection in the out put that will
   include it in the JSON file in case it's reused.
