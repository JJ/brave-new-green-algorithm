# Brave new green algorithm

Papers that use Brave New Algorithm for different purposes, mainly
measuring energy expenses; it uses code at
[BraveNewAlgorithm.jl](https://github.com/CeciMerelo/BraveNewAlgorithm.jl) and
is hosted at [a GitHub
repository](https://github.com/JJ/brave-new-green-algorithm).

## Description

The Brave New Algorithm is an evolutionary algorithm that constraints
reproduction according to a system of *castes* inspired in the Brave New World
novel by Aldous Huxley. That way it can control more effectively the
exploration-exploitation balance. It is fully described in [this
paper](https://link.springer.com/chapter/10.1007/978-3-031-08266-5_20) 

## Source for papers

These are the sources of the papers accepted so far

* [`ola-26.Rnw`](ola-26.Rnw), for the [OLA 26
  conference](https://ola2026.sciencesconf.org/) in Chania,
  Crete. [`ola-26.R`](ola-26.R) contains just the code used to generate tables
  and graphs. [The presentation](preso/index.html) is also contained in the place.
  [`ola-26-explainer.Rmd`](ola-26-explainer.Rmd) is a self-contained R Markdown
  article that retells the main findings in accessible language with enhanced
  visualisations, suitable for a blog post or social-media thread.
  [Read it online here](ola-26-explainer.html), or render it locally with
  `rmarkdown::render("ola-26-explainer.Rmd")`.

## Bibliography

Please reference one of this papers when re-using the content of this
repository, code or data

```bibtex
@misc{10481/107864,
year = {2025},
month = {11},
url = {https://hdl.handle.net/10481/107864},
abstract = {Green computing tries to push a series of best practices that, in general, reduce the amount of energy consumed to perform a given piece of work. There are no fixed rules for {\em greening} an algorithm implementation, which means that we need to create a methodology that, after profiling the energy spent by an algorithm implementation, comes up with specific rules that will optimize the amount of energy spent. In population based algorithms, the exploration/exploitation balance is one of the most critical aspects. The algorithm we will be working with in this paper called Brave New Algorithm was designed with the main objective of keeping that balance in an optimal way through the stratification of the population. In this paper we will analyze how this balance affects the energy consumption of the algorithm.},
organization = {Ministerio español de Economía y Competitividad: proyecto PID2023-147409NB-C21.},
keywords = {Green Computing},
keywords = {Energy profiling},
keywords = {Metaheuristics},
title = {Analyzing how the exploration/exploitation trade off in biologically-inspired algorithms affects energy consumption},
author = {Merelo Guervos, Juan Julián and Merelo-Molina, Cecilia},
}
```

## Open science

This is open science. A bit of orientation

- [`data`](data/) contains the results of the different experiments

## License

(c) Authors, 2025, released under the Affero GPL Please read [`LICENSE`](LICENSE/)
