# Does a version spend more energy because its runs are longer? Tests the chain
#   more generations -> more evaluations -> more running time -> more energy
# between two versions of the algorithm (smartgreens-27).
#
# runs: workload runs from process_deltas() (delta_PKG, delta_seconds), with
# generations, evaluations, initial_temp, final_temp, the four parameters and
# a `group` column holding the version. reference_version is the old version,
# new_version the change. The configuration (four parameters, interacting) is
# a control everywhere; initial_temp (operating context, measured before the
# run) is a control in the running time and energy models.
#
# Two pieces of evidence:
#   links: each step of the chain, from runs of both versions;
#     additional_generations:                 version effect on generations per run
#     evaluations_per_generation:             evaluations ~ generations
#     seconds_per_hundred_thousand_evaluations: delta_seconds ~ evaluations
#     energy_per_second:                      delta_PKG ~ delta_seconds
#   mediation: version effect on delta_PKG (additional energy of the new
#     version), starting from the total (configuration only) and adding, one
#     at a time: initial temperature (carry-over: what previous, longer runs
#     leave behind), generations, evaluations, running time and power drawn
#     by the workload (delta_PKG / delta_seconds). If a variable explains part of the
#     additional energy, the effect shrinks; percentage_this_step is the part
#     of the total absorbed by that step on its own (negative = the effect
#     grows). Steps plus percentage_unexplained add up to 100. Unstable when
#     the total is close to 0.
# chain_supported: every link positive and significant, and carry-over
# (initial temperature) + run length account for more than half of the
# additional energy.
run_length_chain <- function(runs, reference_version, new_version, significance_level = 0.05) {
  versions <- c(reference_version, new_version)
  runs$group <- factor(as.character(runs$group), levels = versions)
  runs$hundred_thousands_of_evaluations <- runs$evaluations / 1e5
  # Power drawn by the workload (W): delta_PKG / delta_seconds, consistent with
  # the baseline-subtracted energy and running time. Memory use is not
  # recorded; dynamic memory activity (reranking, eliminating individuals,
  # garbage collection) shows up only as power drawn, since PKG includes the
  # memory controller. Runs with delta_seconds close to 0 give extreme values.
  if (is.null(runs$power)) runs$power <- runs$delta_PKG / runs$delta_seconds
  version_term <- paste0("group", new_version)

  fit <- function(response, terms) {
    lm(as.formula(paste(response, "~", terms, "+ steps*max_gens*population_size*alpha")), data = runs)
  }
  estimate_of <- function(model, term) {
    coefficients_table <- summary(model)$coefficients
    estimate <- coefficients_table[term, "Estimate"]
    standard_error <- coefficients_table[term, "Std. Error"]
    c(estimate = estimate,
      lower    = estimate - qnorm(0.975) * standard_error,
      upper    = estimate + qnorm(0.975) * standard_error,
      p_value  = coefficients_table[term, "Pr(>|t|)"])
  }

  version_effects <- list(
    generations = estimate_of(fit("generations", "group"), version_term),
    evaluations = estimate_of(fit("evaluations", "group"), version_term),
    seconds     = estimate_of(fit("delta_seconds", "group + initial_temp"), version_term),
    energy      = estimate_of(fit("delta_PKG", "group + initial_temp"), version_term),
    # Without initial temperature as a control: includes what the new version
    # spends because its runs leave the processor hotter for the next ones
    seconds_overall = estimate_of(fit("delta_seconds", "group"), version_term),
    energy_overall  = estimate_of(fit("delta_PKG", "group"), version_term),
    power_overall   = estimate_of(fit("power", "group"), version_term))

  # Power branch of the chain: more generations -> more power drawn (dynamic
  # memory use) -> higher final temperature (not through running longer:
  # running time is a control).
  #   additional_power / additional_power_same_generations: additional total
  #     power of the new version's runs, at the same initial temperature,
  #     without and with generations as a control. If the additional power
  #     comes from the additional generations, it shrinks with the control;
  #     power_percentage_through_generations is how much.
  #   degrees_per_watt: final temperature ~ power, same initial temperature and
  #     running time.
  additional_power                  <- estimate_of(fit("power", "group + initial_temp"), version_term)
  additional_power_same_generations <- estimate_of(fit("power", "group + initial_temp + generations"), version_term)
  power_links <- list(
    additional_power                     = additional_power,
    additional_power_same_generations    = additional_power_same_generations,
    power_percentage_through_generations = 100 * (1 - additional_power_same_generations[["estimate"]] /
                                                      additional_power[["estimate"]]),
    degrees_per_watt = estimate_of(fit("final_temp", "group + power + initial_temp + delta_seconds"), "power"))

  links <- list(
    additional_generations                   = version_effects$generations,
    evaluations_per_generation               = estimate_of(fit("evaluations", "group + generations"), "generations"),
    seconds_per_hundred_thousand_evaluations = estimate_of(fit("delta_seconds", "group + hundred_thousands_of_evaluations + initial_temp"),
                                                           "hundred_thousands_of_evaluations"),
    energy_per_second                        = estimate_of(fit("delta_PKG", "group + delta_seconds + initial_temp"), "delta_seconds"))

  # Variables added one at a time, in causal order. The first row is the
  # total additional energy (configuration only). Initial temperature comes
  # next: it carries what the previous runs left behind (longer, hotter runs
  # leave the processor hotter for the next B and W runs). Then the run
  # length of the run itself, and last the power drawn by the workload.
  # (Final temperature was tried after power and contributed nothing.)
  energy_terms <- c("group",
                    "group + initial_temp",
                    "group + initial_temp + generations",
                    "group + initial_temp + generations + hundred_thousands_of_evaluations",
                    "group + initial_temp + generations + hundred_thousands_of_evaluations + delta_seconds",
                    "group + initial_temp + generations + hundred_thousands_of_evaluations + delta_seconds + power")
  mediation <- as.data.frame(do.call(rbind, lapply(energy_terms, function(terms) {
    estimate_of(fit("delta_PKG", terms), version_term)
  })))
  mediation$controlling_for <- c("Configuration only",
                                 "+ initial temperature", "+ generations", "+ evaluations",
                                 "+ running time", "+ power")
  # Part of the total (first-row) additional energy absorbed by each step on
  # its own: positive = that variable explains part of it, negative = the
  # additional energy grows once it is accounted for. Steps add up to the
  # total explained; what is left is 100 - sum.
  mediation$percentage_this_step <- c(NA, 100 * -diff(mediation$estimate) / mediation$estimate[1])

  step <- function(label) mediation$percentage_this_step[mediation$controlling_for == label]
  percentage_through_initial_temperature <- step("+ initial temperature")
  percentage_through_run_length <- step("+ generations") + step("+ evaluations") + step("+ running time")
  links_positive <- all(sapply(links, function(link) link[["estimate"]] > 0 && link[["p_value"]] < significance_level))
  list(version_effects = version_effects,
       links = links,
       power_links = power_links,
       mediation = mediation,
       percentage_through_initial_temperature = percentage_through_initial_temperature,
       percentage_through_run_length          = percentage_through_run_length,
       percentage_through_power               = step("+ power"),
       percentage_unexplained                 = 100 * mediation$estimate[nrow(mediation)] / mediation$estimate[1],
       # carry-over through initial temperature + run length of the run itself
       chain_supported = links_positive &&
                         percentage_through_initial_temperature + percentage_through_run_length > 50)
}

format_chain_number <- function(values) sapply(signif(values, 3), format, big.mark = ",")
format_chain_p <- function(values) ifelse(values < 1e-4, "$<0.0001$", sprintf("%.2g", values))

# LaTeX table of the differences between versions along the chain (new -
# reference, same configuration): generations, evaluations, running time and
# energy per run, the last one also at the same initial temperature. Estimate
# and p, one column wide. Needs knitr, kableExtra and \usepackage{array}.
run_length_differences_kable <- function(chain, caption) {
  effects <- chain$version_effects
  differences <- rbind(effects$generations, effects$evaluations, effects$seconds_overall,
                       effects$power_overall, effects$energy_overall, effects$energy)
  table <- data.frame(
    quantity = c("Additional generations",
                 "Additional evaluations",
                 "Additional running time (s)",
                 "Additional power (W)",
                 "Additional energy (J)",
                 "\\quad at the same initial temperature (J)"),
    estimate = format_chain_number(differences[, "estimate"]),
    p_value  = format_chain_p(differences[, "p_value"]),
    stringsAsFactors = FALSE)
  kable(table, row.names = FALSE, caption = caption,
        col.names = c("Per run", "Difference", "$p$"),
        format = "latex", escape = FALSE, align = c("l", "r", "r")) %>%
    kable_styling(full_width = F, position = "center", latex_options = "hold_position", font_size = 8) %>%
    column_spec(1, width = "3.2cm")
}

# LaTeX table of the mediation of the additional energy: part of the total
# accounted for by each step, and the part not explained (they add up to
# 100). The total itself (first mediation row, J) goes in the caption.
run_length_mediation_kable <- function(chain, caption) {
  mediation <- chain$mediation[-1, ]
  table <- data.frame(
    step      = c(sub("^\\+ ", "", mediation$controlling_for), "Not explained"),
    accounted = sprintf("%.0f", c(mediation$percentage_this_step, chain$percentage_unexplained)),
    stringsAsFactors = FALSE)
  table$step <- paste0(toupper(substring(table$step, 1, 1)), substring(table$step, 2))
  kable(table, row.names = FALSE, caption = caption,
        col.names = c("Accounted for by", "\\% of the additional energy"),
        format = "latex", escape = FALSE, align = c("l", "r")) %>%
    kable_styling(full_width = F, position = "center", latex_options = "hold_position", font_size = 8) %>%
    row_spec(nrow(table) - 1, hline_after = TRUE)
}
