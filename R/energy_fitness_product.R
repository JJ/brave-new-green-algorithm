# One figure for the energy needed to reach a fitness level: the
# energy-fitness product, energy (J) x diff_fitness (distance to the optimum),
# analogous to the energy-delay product used in green computing. Lower is
# better on both factors, so lower is better for the product.
#
# In log10 it is a sum:
#   energy_fitness_score = log10(energy) + log10(diff_fitness)
# so the fitness level is read directly from it: a version that reaches a 10x
# better fitness (one more fitness level, as in the fitness_level used for
# no_alpha) with the same energy scores 1 less; one that spends 10x the energy
# for the same fitness scores 1 more. Differences between versions are
# log10 ratios: -0.3 = the product halves.
#
# Medians per version, not per-run products: delta_PKG can be zero or negative
# (baseline subtraction), and medians are what the paper reports elsewhere.
# energy_column: "delta_PKG" for the sandwich-subtracted analyses, "PKG" for
# those that model the B runs as covariates.
energy_fitness_product <- function(runs, energy_column = "delta_PKG", version_column = "group") {
  versions <- unique(as.character(runs[[version_column]]))
  energy  <- sapply(versions, function(version) median(runs[[energy_column]][runs[[version_column]] == version]))
  fitness <- sapply(versions, function(version) median(runs$diff_fitness[runs[[version_column]] == version]))
  product <- energy * fitness
  data.frame(version              = versions,
             energy               = energy,
             fitness              = fitness,
             fitness_level        = log10(fitness),
             energy_fitness_product = product,
             energy_fitness_score = log10(product),
             row.names            = versions,
             stringsAsFactors     = FALSE)
}
