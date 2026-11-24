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
# versions: order of the rows (reference version first); by default, order of
# appearance in the data.
energy_fitness_product <- function(runs, energy_column = "delta_PKG", version_column = "group",
                                   versions = unique(as.character(runs[[version_column]]))) {
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

# New vs reference version: ratio of the products (0.5 = half the energy for
# the same fitness, or the same energy for half the distance to the optimum)
# and difference of the scores (= log10 of that ratio; negative = better).
energy_fitness_change <- function(energy_fitness, reference_version, new_version) {
  c(ratio        = energy_fitness[new_version, "energy_fitness_product"] /
                   energy_fitness[reference_version, "energy_fitness_product"],
    score_change = energy_fitness[new_version, "energy_fitness_score"] -
                   energy_fitness[reference_version, "energy_fitness_score"])
}

# LaTeX table: version, median energy, median diff_fitness, score.
# Needs knitr and kableExtra.
energy_fitness_kable <- function(energy_fitness, caption, energy_label = "$\\Delta$PKG (J)") {
  table <- energy_fitness[, c("version", "energy", "fitness", "energy_fitness_score")]
  table$version <- paste0("{\\sf ", gsub("_", "\\\\_", table$version), "}")
  kable(table, digits = c(0, 1, 2, 2), row.names = FALSE, caption = caption,
        col.names = c("Version", energy_label, "{\\sf diff\\_fitness}", "Score"),
        format = "latex", escape = FALSE) %>%
    kable_styling(full_width = F, position = "center", latex_options = "hold_position")
}
