# Operating conditions behind the energy difference between two versions of
# the algorithm (smartgreens-27): running time and initial temperature.
#
# runs: workload runs from process_deltas() (delta_PKG, delta_seconds), with
# initial_temp, evaluations, the four parameters and a `group` column holding
# the version. reference_version is the old version, new_version the change.
#
# Energy = power x time, so the version effect on delta_PKG splits into
#   - through running time: the new version's runs are shorter (or longer);
#   - at the same running time: it draws less (or more) power.
# Models (evaluations and the configuration are always controls):
#   time_model:         delta_seconds ~ version * initial_temp
#   energy_total_model: delta_PKG ~ version * initial_temp
#   energy_model:       delta_PKG ~ version * (delta_seconds + initial_temp)
# Every slope may differ between versions, so version effects are averaged
# over all runs with predict(), not read from a single coefficient.
#
# Savings are positive when the new version spends less, negative when it
# spends more. Slopes and medians are named vectors:
#   c(<reference_version>, <new_version>, difference_p_value)
# p-values: interaction term for slopes, Wilcoxon rank-sum test for medians.
# (Runs are not fully independent: each starts from the temperature the
# previous B run leaves, so p-values for medians are optimistic.)
operating_conditions <- function(runs, reference_version, new_version, significance_level = 0.05) {
  versions <- c(reference_version, new_version)
  runs$group <- factor(as.character(runs$group), levels = versions)

  model_formula <- function(response, terms) {
    as.formula(paste(response, "~", terms, "+ evaluations + steps*max_gens*population_size*alpha"))
  }
  time_model         <- lm(model_formula("delta_seconds", "group * initial_temp"), data = runs)
  energy_total_model <- lm(model_formula("delta_PKG", "group * initial_temp"), data = runs)
  energy_model       <- lm(model_formula("delta_PKG", "group * (delta_seconds + initial_temp)"), data = runs)

  # new - reference, every run kept as it is except for the version
  version_effect <- function(model) {
    as_new       <- runs; as_new$group       <- factor(new_version, levels = versions)
    as_reference <- runs; as_reference$group <- factor(reference_version, levels = versions)
    mean(predict(model, as_new) - predict(model, as_reference))
  }

  slopes <- function(model, variable) {
    coefficients <- coef(model)
    interaction  <- paste0("group", new_version, ":", variable)
    setNames(c(coefficients[[variable]],
               coefficients[[variable]] + coefficients[[interaction]],
               summary(model)$coefficients[interaction, "Pr(>|t|)"]),
             c(versions, "difference_p_value"))
  }

  medians <- function(column) {
    values <- tapply(runs[[column]], runs$group, median)
    setNames(c(values[[reference_version]], values[[new_version]],
               wilcox.test(runs[[column]] ~ runs$group, exact = FALSE)$p.value),
             c(versions, "difference_p_value"))
  }

  energy_saved           <- -version_effect(energy_total_model)
  energy_saved_same_time <- -version_effect(energy_model)
  result <- list(
    seconds_saved                 = -version_effect(time_model),          # s per run
    energy_saved                  = energy_saved,                         # J per run
    energy_saved_same_time        = energy_saved_same_time,               # J, lower power
    energy_saved_through_time     = energy_saved - energy_saved_same_time, # J, shorter runs
    percentage_saved_through_time = 100 * (energy_saved - energy_saved_same_time) / energy_saved,
    power                         = slopes(energy_model, "delta_seconds"),       # J per s (W)
    seconds_per_degree            = slopes(time_model, "initial_temp"),          # s per degree C
    energy_per_degree             = slopes(energy_total_model, "initial_temp"),  # J per degree C
    energy_per_degree_same_time   = slopes(energy_model, "initial_temp"),        # J per degree C, same running time
    seconds                       = medians("delta_seconds"),                    # s
    initial_temp                  = medians("initial_temp"),                     # degrees C
    models = list(time = time_model, energy_total = energy_total_model, energy = energy_model))

  degree <- "$^\\circ$C"
  table <- data.frame(
    quantity = c(paste0("Running time per ", degree, " of initial temperature (s/", degree, ")"),
                 paste0("Energy per ", degree, " of initial temperature (J/", degree, ")"),
                 paste0("Energy per ", degree, " at the same running time (J/", degree, ")"),
                 "Energy per second of running time (W)",
                 "Median running time (s)",
                 paste0("Median initial temperature (", degree, ")")),
    unname(rbind(result$seconds_per_degree, result$energy_per_degree,
                 result$energy_per_degree_same_time, result$power,
                 result$seconds, result$initial_temp)),
    stringsAsFactors = FALSE)
  names(table) <- c("quantity", "reference", "new", "difference_p_value")
  result$table <- table
  # Only what differs significantly between versions goes in the paper table
  result$significant_table <- table[table$difference_p_value < significance_level,
                                    c("quantity", "reference", "new")]
  result
}

# LaTeX table of operating_conditions()$significant_table, one column wide.
# Needs knitr, kableExtra and \usepackage{array} (for column_spec()).
operating_conditions_kable <- function(operating, reference_version, new_version, caption) {
  version_label <- function(version) paste0("{\\sf ", gsub("_", "\\\\_", version), "}")
  kable(operating$significant_table, digits = 2, row.names = FALSE, caption = caption,
        col.names = c("", version_label(reference_version), version_label(new_version)),
        format = "latex", escape = FALSE) %>%
    kable_styling(full_width = F, position = "center", latex_options = "hold_position", font_size = 8) %>%
    column_spec(1, width = "2.8cm")   # wrap the legends so the table fits one column
}
