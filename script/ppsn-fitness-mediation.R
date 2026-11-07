# Fitness model for the PPSN-2026 BNA versions, mediated by the number of evaluations.
#
# Question: how much of the effect of every algorithm parameter (and of every
# algorithm version) on the fitness reached is due to "searching more"
# (more evaluations, indirect/mediated effect) and how much to "searching better"
# (better fitness at the same number of evaluations, direct effect)?
#
# Run from the repository root:
#   Rscript script/ppsn-fitness-mediation.R
#   N_BOOT=20 Rscript script/ppsn-fitness-mediation.R   # quick run
#
# Notes
# - Only workload runs are used; fitness does not depend on operating conditions
#   (temperature, time), so versions run on different days can be pooled.
# - Response: log10(diff_fitness); mediator: log10(evaluations). In log-log
#   scale the slope is a convergence rate (orders of magnitude of fitness gained
#   per order of magnitude of evaluations).
# - All current datasets use a single problem dimension (10), so dimension is
#   not modeled. If other dimensions are added, it must enter as a problem
#   parameter (stratum), not as an algorithm parameter.
# - The stopping criterion (max_gens generations without improvement) makes
#   evaluations partly a *consequence* of fitness progress, so the direct effect
#   must be read as "conditional on the same search effort", not strictly causal.
#   Section 3 separates the slope across runs of the same configuration
#   (seed, realized run length: stopping-rule artifact + luck) from the slope
#   across configurations (set by parameters, max_gens value included).

library(dplyr)
library(ggplot2)

N_BOOT <- as.integer(Sys.getenv("N_BOOT", "500"))

# ---- 1. Data ------------------------------------------------------------------

# no_mutation_alpha: the alpha caste is not mutated (data object keeps its
# original name, ppsn_no_alpha). The alpha parameter (alpha caste size, 10% or
# 25%) is still active in that version.
versions <- c(baseline          = "ppsn_speedup_fixed_gradient_descent",
              gamma_upgrade     = "gamma_upgrade",
              microopt          = "ppsn_microopt",
              low_mutation      = "ppsn_low_mutation",
              no_mutation_alpha = "ppsn_no_alpha")

load_version <- function(name, object) {
  env <- new.env()
  load(file.path("data", paste0(object, ".rds")), envir = env)
  get(object, envir = env) %>%
    filter(!startsWith(work, "base")) %>%
    select(population_size, max_gens, alpha, steps, diff_fitness, evaluations) %>%
    mutate(group = name)
}

fitness_data <- bind_rows(Map(load_version, names(versions), versions)) %>%
  mutate(group     = factor(group, levels = names(versions)),
         log_fit   = log10(diff_fitness),
         log_evals = log10(evaluations),
         across(c(alpha, steps, max_gens, population_size), factor))

params <- c("alpha", "steps", "max_gens", "population_size")
param_terms <- paste0("(", paste(params, collapse = " + "), ")^2")

cat("Workload runs per version:\n")
print(table(fitness_data$group))

# ---- 2. Pooled models: total effect, mediator, direct effect + slopes ---------

f_total <- as.formula(paste("log_fit ~ group *", param_terms))
f_med   <- as.formula(paste("log_evals ~ group *", param_terms))
f_out   <- as.formula(paste("log_fit ~ group *", param_terms, "+ group:log_evals"))

m_total <- lm(f_total, data = fitness_data)
m_med   <- lm(f_med,   data = fitness_data)
m_out   <- lm(f_out,   data = fitness_data)

cat("\nR^2  total:", round(summary(m_total)$r.squared, 3),
    " mediator:", round(summary(m_med)$r.squared, 3),
    " outcome (parameter + evals):", round(summary(m_out)$r.squared, 3), "\n")

# Convergence rate (log-log slope) per version, with 95% CI
slope_ci <- confint(m_out)[grep(":log_evals$", names(coef(m_out))), , drop = FALSE]
slopes <- data.frame(group = sub("^group(.*):log_evals$", "\\1", rownames(slope_ci)),
                     slope = coef(m_out)[rownames(slope_ci)],
                     lower = slope_ci[, 1], upper = slope_ci[, 2], row.names = NULL)
cat("\nlog10(diff_fitness) vs log10(evaluations) slope per version (more negative = more efficient):\n")
print(slopes, digits = 3)

# Partial-regression view of the slopes: strip the parameter effects from both
# axes within each version (Frisch-Waugh-Lovell), so the line through the points
# has exactly the coefficient in `slopes`. Group means are added back to keep
# the axes in real log10 units.
f_y_par <- as.formula(paste("log_fit ~",   param_terms))
f_x_par <- as.formula(paste("log_evals ~", param_terms))

partial <- fitness_data %>%
  group_by(group) %>%
  group_modify(function(d, key) {
    d <- droplevels(d)
    mutate(d,
           fit_adjusted   = resid(lm(f_y_par, d)) + mean(log_fit),
           evals_adjusted = resid(lm(f_x_par, d)) + mean(log_evals))
  }) %>%
  ungroup()

slope_labels <- slopes %>%
  mutate(group = factor(group, levels = names(versions)),
         label = sprintf("slope %.2f [%.2f, %.2f]", slope, lower, upper))

ggplot(partial, aes(evals_adjusted, fit_adjusted, colour = group)) +
  geom_point(alpha = 0.2, size = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.8) +
  geom_text(data = slope_labels, aes(label = label),
            x = -Inf, y = -Inf, hjust = -0.05, vjust = -0.6,
            size = 2.8, colour = "grey20", inherit.aes = FALSE) +
  facet_wrap(~ group, nrow = 1) +
  labs(x = "log10(evaluations) net of parameters (FWL partial)",
       y = "log10(diff_fitness) net of parameters (FWL partial)") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

# Sanity check: operating conditions should NOT explain fitness. Only meaningful
# if temperatures are added to fitness_data; left here as a reminder.

# ---- 3. Slopes across configurations vs across runs ---------------------------
#
# across_configs: slope on a configuration's mean log10(evaluations), i.e. what
#   a configuration that spends more evaluations gains (no parameter terms in
#   the model, so it also absorbs direct parameter effects).
# across_runs: slope on a run's deviation from its configuration mean, i.e.
#   what a run gains by happening to last longer (seed + when max_gens fires).

fitness_data <- fitness_data %>%
  group_by(group, alpha, steps, max_gens, population_size) %>%
  mutate(log_evals_config_mean = mean(log_evals),
         log_evals_run_dev     = log_evals - log_evals_config_mean) %>%
  ungroup()

m_configs_runs <- lm(log_fit ~ group * (log_evals_config_mean + log_evals_run_dev),
                     data = fitness_data)

config_run_slopes <- data.frame(t(sapply(levels(fitness_data$group), function(g) {
  ref <- g == levels(fitness_data$group)[1]
  b <- coef(m_configs_runs)
  c(across_configs = unname(b["log_evals_config_mean"] + if (ref) 0 else b[paste0("group", g, ":log_evals_config_mean")]),
    across_runs    = unname(b["log_evals_run_dev"]     + if (ref) 0 else b[paste0("group", g, ":log_evals_run_dev")]))
})))
cat("\nSlopes across configurations (set by parameters, max_gens included) vs",
    "across runs of the same configuration (seed, realized run length):\n")
print(config_run_slopes, digits = 3)

# Plot of the two slopes. Top row: one point per configuration (its mean
# log10 evaluations and mean log10 fitness, size = number of runs). Bottom row:
# one point per run, as its distance from its configuration's means. A
# run-weighted line through the top row and a plain line through the bottom
# row reproduce across_configs and across_runs exactly.
config_points <- fitness_data %>%
  group_by(group, alpha, steps, max_gens, population_size) %>%
  summarise(x = mean(log_evals), y = mean(log_fit), runs = n(), .groups = "drop") %>%
  mutate(view = "across configurations")

run_points <- fitness_data %>%
  group_by(group, alpha, steps, max_gens, population_size) %>%
  mutate(x = log_evals_run_dev, y = log_fit - mean(log_fit), runs = 1) %>%
  ungroup() %>%
  mutate(view = "across runs (distance from configuration mean)")

views <- c("across configurations", "across runs (distance from configuration mean)")
config_run_points <- bind_rows(config_points, run_points) %>%
  mutate(view = factor(view, levels = views))

config_run_labels <- config_run_slopes %>%
  mutate(group = factor(rownames(config_run_slopes), levels = names(versions))) %>%
  tidyr::pivot_longer(c(across_configs, across_runs), names_to = "view", values_to = "slope") %>%
  mutate(view  = factor(views[match(view, c("across_configs", "across_runs"))], levels = views),
         label = sprintf("slope %.2f", slope))

ggplot(config_run_points, aes(x, y, colour = group)) +
  geom_point(data = filter(config_run_points, view == views[2]),
             alpha = 0.15, size = 0.6) +
  geom_point(data = filter(config_run_points, view == views[1]),
             aes(size = runs), alpha = 0.8) +
  geom_smooth(aes(weight = runs), method = "lm", formula = y ~ x,
              se = FALSE, linewidth = 0.8, colour = "grey20") +
  geom_text(data = config_run_labels, aes(label = label),
            x = -Inf, y = -Inf, hjust = -0.05, vjust = -0.6,
            size = 2.8, colour = "grey20", inherit.aes = FALSE) +
  facet_wrap(view ~ group, nrow = 2, scales = "free",
             labeller = label_wrap_gen(width = 26, multi_line = FALSE)) +
  scale_size_area(max_size = 3, guide = "none") +
  labs(x = "log10(evaluations)", y = "log10(diff_fitness)") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

# Same configuration points, one row per parameter, coloured by its level.
# Arrows join configurations that differ only in that parameter (low -> high):
# along the across_configs slope = it acts by spending more evaluations;
# vertical = it changes fitness at the same cost; ~zero length = no effect.
# Arrows that all point the same way = the better value is always the same.
config_by_param <- bind_rows(lapply(params, function(p) {
  d <- config_points
  d$param      <- p
  d$level      <- as.character(d[[p]])
  d$level_rank <- ifelse(as.integer(d[[p]]) == 1, "low", "high")
  d$others     <- do.call(paste, c(lapply(d[setdiff(params, p)], as.character), sep = "/"))
  d
})) %>%
  mutate(param = factor(param, levels = params))

config_arrows <- config_by_param %>%
  select(group, param, others, level_rank, x, y) %>%
  tidyr::pivot_wider(names_from = level_rank, values_from = c(x, y)) %>%
  filter(!is.na(x_low), !is.na(x_high))

param_level_text <- config_by_param %>%
  group_by(group, param) %>%
  summarise(label = paste0("low = ",  first(level[level_rank == "low"]),
                           ", high = ", first(level[level_rank == "high"])),
            .groups = "drop")

ggplot(config_by_param, aes(x, y)) +
  geom_segment(data = config_arrows,
               aes(x = x_low, y = y_low, xend = x_high, yend = y_high),
               arrow = arrow(length = unit(0.12, "cm")),
               colour = "grey60", linewidth = 0.4) +
  geom_point(aes(colour = level_rank, size = runs), alpha = 0.85) +
  geom_text(data = param_level_text, aes(label = label),
            x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
            size = 2.5, colour = "grey30", inherit.aes = FALSE) +
  facet_wrap(~ param + group, ncol = length(versions), scales = "free",
             labeller = label_wrap_gen(width = 26, multi_line = FALSE)) +
  scale_colour_manual(values = c(low = "#1b9e77", high = "#d95f02"), name = "Level") +
  scale_size_area(max_size = 3, guide = "none") +
  labs(x = "configuration mean log10(evaluations)",
       y = "configuration mean log10(diff_fitness)") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

# ---- FINDINGS so far (2026-11-07), to move to the paper -----------------------
# diff_fitness is distance to the optimum: lower = better. Slopes are log-log,
# so 10x more evaluations -> 10^|slope| times better fitness.
#
# F1. Model fit (section 2, pooled). Version + parameters explain 97.9% of the
#     variance of log10(diff_fitness) and 86.4% of log10(evaluations). Adding
#     evaluations raises it to 98.7%: +0.008, i.e. ~38% of what parameters
#     leave unexplained. That extra part is run-to-run variation within a
#     configuration, not a parameter effect. (R^2 values are fit, not a
#     total = direct + indirect decomposition; that is sections 4-5.)
#
# F2. Convergence rate at fixed configuration (FWL slopes, section 2):
#     baseline -0.32, gamma_upgrade -0.38, microopt -0.33, low_mutation -1.24,
#     no_mutation_alpha -2.68. microopt ~ baseline: the implementation-only
#     negative control behaves as expected. The jump comes with low_mutation and
#     grows with no_mutation_alpha: in those versions a run that lasts longer
#     keeps improving much more (10x evaluations -> ~17x and ~480x better fitness,
#     vs ~2x in the first three versions).
#
# F3. Across configurations vs across runs (this section):
#                       across_configs  across_runs
#     baseline              -0.150        -0.324
#     gamma_upgrade         -0.155        -0.377
#     microopt              -0.144        -0.329
#     low_mutation          -0.440        -1.237
#     no_mutation_alpha     -1.010        -2.683
#     In every version, extra evaluations obtained by choosing a different
#     configuration buy 2.2-2.8x less fitness than extra evaluations a run gets
#     by lasting longer. Caveats: across_configs has no parameter terms, so it
#     also absorbs direct parameter effects; across_runs is steepened by the
#     stopping rule (a run that keeps improving is the one that does not stop).
#     No CIs yet; across_configs CIs need a bootstrap over configurations.
#     across_runs matches the F2 slopes, as expected.
#
# F4. Per-parameter view (arrow plot above; visual reading, to be confirmed
#     with mediation_table from section 4):
#     - steps (16 vs 64) has ~no effect: configurations differing only in
#       steps overlap, which produces the point pairs in every panel.
#     - High alpha (25% vs 10%), high max_gens (25 vs 10) and high
#       population_size (800 vs 400) give better fitness in most versions.
#       The arrow plot alone was misleading about alpha; see F5.
#     - no_mutation_alpha breaks the configuration layout shared by the other
#       versions; F5 shows why (alpha's direct effect is ~9x larger there).
#
# F5. Mediation per parameter (section 4; effect of low -> high on
#     log10(diff_fitness), negative = better; direct (same_budget) = at the
#     same number of evaluations, mediated (via_budget) = through the
#     mediator, the evaluation budget).
#     - steps: no effect on either path.
#     - max_gens: pure budget effect. direct ~0, indirect -0.11 to -0.13
#       (first three versions), -0.36 (low_mutation), -0.69
#       (no_mutation_alpha). More patience -> more evaluations -> better
#       fitness, with no better search.
#     - population_size: direct POSITIVE (worse at the same number of
#       evaluations: +0.04 to +0.06, +0.35 low_mutation, +0.30
#       no_mutation_alpha), indirect negative (more evaluations). It pays off
#       only by making runs longer; in low_mutation the two cancel out (total
#       +0.013, n.s.). Reading: an exploration knob, costs efficiency per
#       evaluation.
#     - alpha: direct strongly NEGATIVE (better at the same budget: -0.14 to
#       -0.15, -0.32 low_mutation, -1.33 no_mutation_alpha = ~21x better),
#       indirect POSITIVE (+0.08 to +0.09, +0.36, +0.55): a larger alpha caste
#       means FEWER evaluations, because the run stagnates and the stopping
#       rule fires earlier. Net gain except in low_mutation (total +0.046, worse):
#       there early stagnation outweighs the better search. Reading: an
#       exploitation knob; classic premature-convergence trade-off. With low
#       mutation the algorithm is already exploitative and more elite tips it
#       into stagnation; with an unmutated alpha caste (pure elitism) the gain
#       per evaluation is largest.
#     - The old prop_mediated (mediated / total) is meaningless when the two
#       paths have opposite signs (alpha, population_size); it is replaced by
#       budget_share = |mediated| / (|direct| + |mediated|). Report both
#       components anyway.
#     E/E readings are interpretations consistent with the signs, not a direct
#     measurement of exploration/exploitation.

# ---- 4. Mediation per version and parameter (bootstrap) ----------------------
#
# Within each version: mediator (evaluation budget) model log_evals ~ params^2
# and outcome model log_fit ~ params^2 + log_evals. For a two-level parameter,
# effects are average marginal effects of switching it from its low to its high
# level, averaged over the observed values of the other parameters. All effects
# are differences in log10(diff_fitness), i.e. log10 of the high/low fitness
# ratio.
#   same_budget:  DIRECT effect, at the same number of evaluations (search
#                 efficiency).
#   via_budget:   MEDIATED (indirect) effect, through the mediator, the
#                 evaluation budget = (change in log10 evaluations) x b, where
#                 b = slope of fitness on the mediator (negative). A positive
#                 value means the parameter shortens runs (fewer evaluations)
#                 and that costs fitness.
#   budget_share: share mediated = |via_budget| / (|same_budget| + |via_budget|),
#                 in [0, 1]; replaces mediated/total, which is meaningless when
#                 the two paths have opposite signs.
# With a single mediator slope b, total = same_budget + via_budget exactly
# (OLS, same regressors).

marginal_effect <- function(model, data, param) {
  lv <- levels(data[[param]])
  hi <- lo <- data
  hi[[param]] <- factor(lv[2], levels = lv)
  lo[[param]] <- factor(lv[1], levels = lv)
  mean(predict(model, hi) - predict(model, lo))
}

mediation_effects <- function(data) {
  f_m <- as.formula(paste("log_evals ~", param_terms))
  f_y <- as.formula(paste("log_fit ~", param_terms, "+ log_evals"))
  f_t <- as.formula(paste("log_fit ~", param_terms))
  mm <- lm(f_m, data); my <- lm(f_y, data); mt <- lm(f_t, data)
  b <- coef(my)["log_evals"]
  do.call(rbind, lapply(params, function(p) {
    total       <- marginal_effect(mt, data, p)
    same_budget <- marginal_effect(my, data, p)
    via_budget  <- b * marginal_effect(mm, data, p)
    data.frame(param = p, total = total, same_budget = same_budget,
               via_budget = unname(via_budget))
  }))
}

mediation_by_group <- function(g) {
  d <- fitness_data %>% filter(group == g) %>% droplevels()
  point <- mediation_effects(d)
  boots <- replicate(N_BOOT,
                     as.matrix(mediation_effects(d[sample(nrow(d), replace = TRUE), ])[, -1]),
                     simplify = "array")
  ci <- function(k, q) apply(boots[, k, ], 1, quantile, q)
  point %>%
    mutate(group = g,
           total_lo       = ci("total", 0.025),       total_hi       = ci("total", 0.975),
           same_budget_lo = ci("same_budget", 0.025), same_budget_hi = ci("same_budget", 0.975),
           via_budget_lo  = ci("via_budget", 0.025),  via_budget_hi  = ci("via_budget", 0.975),
           budget_share   = abs(via_budget) / (abs(same_budget) + abs(via_budget))) %>%
    relocate(group)
}

mediation_table <- bind_rows(lapply(levels(fitness_data$group), mediation_by_group))

cat("\nEffect on log10(diff_fitness) of switching each parameter low -> high",
    "(negative = better fitness): same_budget = direct, via_budget = mediated",
    "by the evaluation budget, 95% bootstrap CI:\n")
print(mediation_table %>%
        transmute(group, param,
                  total       = sprintf("%.3f [%.3f, %.3f]", total, total_lo, total_hi),
                  same_budget = sprintf("%.3f [%.3f, %.3f]", same_budget, same_budget_lo, same_budget_hi),
                  via_budget  = sprintf("%.3f [%.3f, %.3f]", via_budget, via_budget_lo, via_budget_hi),
                  budget_share = round(budget_share, 2)),
      row.names = FALSE)

# ---- 5. Version effects (consecutive pairs, as in the paper) ------------------
#
# Same decomposition, with the version change as the "treatment" and the four
# parameters as covariates. microopt vs baseline is a negative control:
# an implementation-only change should have ~0 total and direct (same_budget)
# effect.

version_pairs <- list(c("baseline", "gamma_upgrade"),
                      c("baseline", "microopt"),
                      c("microopt", "low_mutation"),
                      c("low_mutation", "no_mutation_alpha"))

version_effect <- function(data) {
  mm <- lm(as.formula(paste("log_evals ~ group *", param_terms)), data)
  my <- lm(as.formula(paste("log_fit ~ group *", param_terms, "+ log_evals")), data)
  mt <- lm(as.formula(paste("log_fit ~ group *", param_terms)), data)
  b <- coef(my)["log_evals"]
  c(total       = marginal_effect(mt, data, "group"),
    same_budget = marginal_effect(my, data, "group"),
    via_budget  = unname(b * marginal_effect(mm, data, "group")))
}

version_table <- do.call(rbind, lapply(version_pairs, function(pair) {
  d <- fitness_data %>% filter(group %in% pair) %>%
    mutate(group = factor(as.character(group), levels = pair))
  point <- version_effect(d)
  boots <- replicate(N_BOOT, version_effect(d[sample(nrow(d), replace = TRUE), ]))
  q <- apply(boots, 1, quantile, c(0.025, 0.975))
  data.frame(change = paste(pair, collapse = " -> "),
             total       = sprintf("%.3f [%.3f, %.3f]", point["total"],       q[1, "total"],       q[2, "total"]),
             same_budget = sprintf("%.3f [%.3f, %.3f]", point["same_budget"], q[1, "same_budget"], q[2, "same_budget"]),
             via_budget  = sprintf("%.3f [%.3f, %.3f]", point["via_budget"],  q[1, "via_budget"],  q[2, "via_budget"]),
             budget_share = round(abs(point["via_budget"]) /
                                  (abs(point["same_budget"]) + abs(point["via_budget"])), 2),
             row.names = NULL)
}))

cat("\nEffect of each version change on log10(diff_fitness), 95% bootstrap CI:\n")
print(version_table, row.names = FALSE)

# ---- 6. Plot ------------------------------------------------------------------

ggplot(fitness_data, aes(x = log_evals, y = log_fit, color = group)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(x = "log10(evaluations)", y = "log10(diff_fitness)", color = "Version") +
  theme_minimal() + theme(legend.position = "bottom")

# ---- 7. no_mutation_alpha: which parameter splits the fitness/evaluations cloud?
#
# In the plot above the no_mutation_alpha cloud is visibly split in two
# parallel bands. A parameter that creates the split shifts fitness *at the
# same number of evaluations*, i.e. it acts through the direct (same_budget)
# path, not through the mediator (evaluation budget).
# For each parameter:
#   - offset: difference in mean residual of log_fit ~ log_evals between its
#     levels (vertical distance between the two bands, in log10 units);
#   - r2_gain: extra R^2 of log_fit ~ log_evals + param over log_fit ~ log_evals;
#   - agreement: how well its levels match a 2-cluster k-means split of those
#     residuals (0.5 = unrelated, 1 = the parameter *is* the split).

nma_data <- fitness_data %>% filter(group == "no_mutation_alpha") %>% droplevels()

m_nma_evals   <- lm(log_fit ~ log_evals, data = nma_data)
nma_data$res  <- resid(m_nma_evals)
r2_evals     <- summary(m_nma_evals)$r.squared
set.seed(1)
nma_clusters  <- kmeans(nma_data$res, centers = quantile(nma_data$res, c(0.25, 0.75)))$cluster

split_table <- do.call(rbind, lapply(params, function(p) {
  lv  <- levels(nma_data[[p]])
  tab <- table(nma_data[[p]], nma_clusters)
  data.frame(param     = p,
             offset    = mean(nma_data$res[nma_data[[p]] == lv[2]]) -
                         mean(nma_data$res[nma_data[[p]] == lv[1]]),
             r2_gain   = summary(lm(as.formula(paste("log_fit ~ log_evals +", p)),
                                    nma_data))$r.squared - r2_evals,
             agreement = max(sum(diag(tab)), sum(tab) - sum(diag(tab))) / sum(tab))
})) %>% arrange(desc(agreement))

cat("\nno_mutation_alpha: separation of the fitness bands by parameter",
    "(offset = high - low level, log10 units):\n")
print(split_table, digits = 3, row.names = FALSE)
split_param <- split_table$param[1]
cat("Parameter that best explains the split:", split_param, "\n")

# Mediation by the evaluation budget for no_mutation_alpha (from section 4):
# the split parameter should show a large *direct* (same_budget) effect.
cat("\nno_mutation_alpha: total / direct (same_budget) / mediated (via_budget)",
    "effect of each parameter (from section 4):\n")
print(mediation_table %>%
        filter(group == "no_mutation_alpha") %>%
        transmute(param,
                  total       = sprintf("%.3f [%.3f, %.3f]", total, total_lo, total_hi),
                  same_budget = sprintf("%.3f [%.3f, %.3f]", same_budget, same_budget_lo, same_budget_hi),
                  via_budget  = sprintf("%.3f [%.3f, %.3f]", via_budget, via_budget_lo, via_budget_hi),
                  budget_share = round(budget_share, 2)),
      row.names = FALSE)

# Convergence line (log_fit ~ log_evals) for every level of every parameter.
# Pooling the two bands flattens the slope, so within-level slopes for the
# split parameter are the more meaningful convergence rates.
nma_long <- bind_rows(lapply(params, function(p) {
  nma_data %>% transmute(param = p, level = as.character(.data[[p]]), log_evals, log_fit)
}))

nma_level_slopes <- nma_long %>%
  group_by(param, level) %>%
  group_modify(function(d, key) {
    m  <- lm(log_fit ~ log_evals, d)
    ci <- confint(m)["log_evals", ]
    data.frame(n = nrow(d), intercept = coef(m)[[1]], slope = coef(m)[[2]],
               lower = ci[[1]], upper = ci[[2]])
  }) %>%
  ungroup()
cat("\nno_mutation_alpha: log10(diff_fitness) ~ log10(evaluations) per parameter level:\n")
print(as.data.frame(nma_level_slopes), digits = 3, row.names = FALSE)

# Mediation for the other parameters *within* each level of the split
# parameter, so their effects are not confounded with the band they fall in.
other_params <- setdiff(params, split_param)
cat("\nno_mutation_alpha: effects of the remaining parameters within each level of", split_param, "\n")
within_split <- bind_rows(lapply(levels(nma_data[[split_param]]), function(l) {
  d  <- nma_data %>% filter(.data[[split_param]] == l)
  pt <- paste0("(", paste(other_params, collapse = " + "), ")^2")
  mm <- lm(as.formula(paste("log_evals ~", pt)), d)
  mt <- lm(as.formula(paste("log_fit ~",   pt)), d)
  my <- lm(as.formula(paste("log_fit ~",   pt, "+ log_evals")), d)
  b  <- coef(my)[["log_evals"]]
  do.call(rbind, lapply(other_params, function(p) {
    data.frame(level = paste(split_param, "=", l), param = p,
               total       = marginal_effect(mt, d, p),
               same_budget = marginal_effect(my, d, p),
               via_budget  = b * marginal_effect(mm, d, p))
  }))
}))
print(within_split, digits = 3, row.names = FALSE)

ggplot(nma_long, aes(log_evals, log_fit, colour = level)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.8) +
  facet_wrap(~ param, nrow = 1) +
  labs(x = "log10(evaluations)", y = "log10(diff_fitness)", colour = "Level",
       title = "no_mutation_alpha: fitness vs evaluations by parameter level") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

