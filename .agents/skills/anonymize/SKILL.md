---
name: anonymyze
description: |
    Check self-references in text and bibliography and substitute them with
    anonymous references
---

# Make a paper anonymous

Create a version of the paper that eliminates all self-references substituting
them with an anonymous version for double-blind submission

1. Identify all self-references made as such in the paper, that is, not referred
   to as a third person.
2. Change the reference key to [reference-key]-anon.
3. If such reference exists in the BiBTex file ours.bib, leave it alone. If it
   does not, create a new BiBTex reference with that key, the same type of
   reference, with anonymous title, conference and authors. Indicate only if
   it's a conference or journal paper.
4. Comment-out acknowledgements, if they are not already so
5. Convert Github URLs to generic or anonymous URLs.
