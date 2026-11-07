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

# ---- 5b. Version effects with a version-specific convergence slope ------------
#
# Section 5 uses one slope b of fitness on evaluations for both versions in a
# pair, so via_budget values every extra evaluation at the same rate in both,
# while F2 shows the slopes differ ~4x (microopt -> low_mutation) and ~2x
# (low_mutation -> no_mutation_alpha). Here the outcome model gets a
# group:log_evals interaction, so each version has its own slope, and the
# question becomes: are the new version's extra evaluations worth more?
#
# With M_old / M_new = predicted log10(evaluations) of each configuration under
# the old / new version (mediator model), and b_old / b_new the two slopes:
#   same_budget:    new - old version at the OLD version's budget M_old
#                   (better search at the same effort).
#   via_budget:     b_new x (M_new - M_old): what the new version gains from
#                   its change in run length, at its own rate. Split into
#     at_old_rate:  b_old x (M_new - M_old): what those extra evaluations would
#                   have bought the old version;
#     better_use:   (b_new - b_old) x (M_new - M_old): extra gain because the
#                   new version converts evaluations into fitness faster.
#   slope_old, slope_new, slope_diff: the two convergence rates (log-log).
# total is still the model without the mediator; same_budget + via_budget no
# longer has to equal it exactly (it does only for parallel slopes).

version_effect_slopes <- function(data) {
  pair <- levels(data$group)
  mm <- lm(as.formula(paste("log_evals ~ group *", param_terms)), data)
  my <- lm(as.formula(paste("log_fit ~ group *", param_terms, "+ group * log_evals")), data)
  mt <- lm(as.formula(paste("log_fit ~ group *", param_terms)), data)
  old <- new <- data
  old$group <- factor(pair[1], levels = pair)
  new$group <- factor(pair[2], levels = pair)
  m_old <- predict(mm, old)
  m_new <- predict(mm, new)
  slope <- function(d) {                   # model is linear in log_evals
    d1 <- d; d1$log_evals <- d$log_evals + 1
    mean(predict(my, d1) - predict(my, d))
  }
  new_at_old <- new
  old$log_evals        <- m_old
  new_at_old$log_evals <- m_old
  new$log_evals        <- m_new
  b_old <- slope(old); b_new <- slope(new)
  d_m   <- mean(m_new - m_old)
  c(total       = mean(predict(mt, new) - predict(mt, old)),
    same_budget = mean(predict(my, new_at_old) - predict(my, old)),
    via_budget  = b_new * d_m,
    at_old_rate = b_old * d_m,
    better_use  = (b_new - b_old) * d_m,
    slope_old   = b_old,
    slope_new   = b_new,
    slope_diff  = b_new - b_old)
}

fmt_ci <- function(x, lo, hi) sprintf("%.3f [%.3f, %.3f]", x, lo, hi)

version_slope_results <- lapply(version_pairs, function(pair) {
  d <- fitness_data %>% filter(group %in% pair) %>%
    mutate(group = factor(as.character(group), levels = pair))
  point <- version_effect_slopes(d)
  boots <- replicate(N_BOOT, version_effect_slopes(d[sample(nrow(d), replace = TRUE), ]))
  q <- apply(boots, 1, quantile, c(0.025, 0.975))
  list(change = paste(pair, collapse = " -> "), point = point, q = q)
})

version_slope_table <- do.call(rbind, lapply(version_slope_results, function(r) {
  cols <- c("total", "same_budget", "via_budget", "at_old_rate", "better_use")
  as.data.frame(c(list(change = r$change),
                  setNames(lapply(cols, function(k) fmt_ci(r$point[k], r$q[1, k], r$q[2, k])), cols)))
}))

version_slope_rates <- do.call(rbind, lapply(version_slope_results, function(r) {
  cols <- c("slope_old", "slope_new", "slope_diff")
  as.data.frame(c(list(change = r$change),
                  setNames(lapply(cols, function(k) fmt_ci(r$point[k], r$q[1, k], r$q[2, k])), cols)))
}))

cat("\nVersion change effect on log10(diff_fitness) with version-specific slopes,",
    "95% bootstrap CI:\n")
print(version_slope_table, row.names = FALSE)
cat("\nConvergence slopes (log10 fitness per log10 evaluation) in each pair, 95% bootstrap CI:\n")
print(version_slope_rates, row.names = FALSE)

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

# no_mutation_alpha: is the shorter run length of a larger alpha caste a matter
# of generations or of evaluations per generation?
#
# In section 4, a larger alpha caste (25% vs 10%) spends ~38% fewer evaluations
# (via_budget +0.55). Two mechanisms can produce that:
#   - fewer generations: the run exhausts its improvements sooner and the
#     stopping rule (max_gens generations without improvement) fires earlier;
#   - fewer evaluations per generation: smaller lower castes, which may spend
#     more evaluations per individual than the alpha caste's crossover.
# log10(evaluations) = log10(generations) + log10(evaluations per generation),
# so for every parameter (low -> high, average marginal effect, same terms as
# section 4):
#   evaluations_change = generations_change + evaluations_per_generation_change
# exactly. The fitness model then uses the two parts as separate mediators:
#   slope_generations, slope_evaluations_per_generation: log10 fitness gained
#     per log10 of each part (equal slopes = an evaluation is worth the same
#     whether it comes from a longer run or from a more expensive generation);
#   via_generations, via_evaluations_per_generation: each part's contribution
#     to the budget-mediated effect on log10(diff_fitness).
# If evaluations per generation are fully set by the parameters (no variation
# between runs of the same configuration), its slope is not identifiable and
# shows as NA.

no_mutation_alpha_environment <- new.env()
load(file.path("data", "ppsn_no_alpha.rds"), envir = no_mutation_alpha_environment)
no_mutation_alpha_runs <- get("ppsn_no_alpha", envir = no_mutation_alpha_environment) %>%
  filter(!startsWith(work, "base")) %>%
  select(population_size, max_gens, alpha, steps, diff_fitness, evaluations, generations) %>%
  mutate(log_fit                        = log10(diff_fitness),
         log_evals                      = log10(evaluations),
         log_generations                = log10(generations),
         log_evaluations_per_generation = log_evals - log_generations,
         across(all_of(params), factor))

run_length_by_alpha <- no_mutation_alpha_runs %>%
  group_by(alpha) %>%
  summarise(runs                              = n(),
            median_generations                = median(generations),
            median_evaluations                = median(evaluations),
            median_evaluations_per_generation = median(evaluations / generations),
            .groups = "drop")
cat("\nno_mutation_alpha: run length by alpha caste size:\n")
print(as.data.frame(run_length_by_alpha), digits = 4, row.names = FALSE)

run_length_measures <- c("evaluations_change", "generations_change",
                         "evaluations_per_generation_change",
                         "via_generations", "via_evaluations_per_generation")

fit_on_parameters <- function(runs, response, extra_terms = "") {
  lm(as.formula(paste(response, "~", param_terms, extra_terms)), runs)
}

run_length_slopes <- function(runs) {
  fitness_model <- fit_on_parameters(runs, "log_fit",
                                     "+ log_generations + log_evaluations_per_generation")
  c(slope_generations                = unname(coef(fitness_model)["log_generations"]),
    slope_evaluations_per_generation = unname(coef(fitness_model)["log_evaluations_per_generation"]))
}

run_length_effects <- function(runs) {
  evaluations_model    <- fit_on_parameters(runs, "log_evals")
  generations_model    <- fit_on_parameters(runs, "log_generations")
  per_generation_model <- fit_on_parameters(runs, "log_evaluations_per_generation")
  slopes <- run_length_slopes(runs)
  sapply(params, function(parameter) {
    generations_change    <- marginal_effect(generations_model, runs, parameter)
    per_generation_change <- marginal_effect(per_generation_model, runs, parameter)
    c(evaluations_change                = marginal_effect(evaluations_model, runs, parameter),
      generations_change                = generations_change,
      evaluations_per_generation_change = per_generation_change,
      via_generations                   = slopes[["slope_generations"]] * generations_change,
      via_evaluations_per_generation    = slopes[["slope_evaluations_per_generation"]] * per_generation_change)
  })
}

resample_runs <- function(runs) runs[sample(nrow(runs), replace = TRUE), ]

run_length_point      <- run_length_effects(no_mutation_alpha_runs)
run_length_bootstrap  <- replicate(N_BOOT, run_length_effects(resample_runs(no_mutation_alpha_runs)))
slopes_point          <- run_length_slopes(no_mutation_alpha_runs)
slopes_bootstrap      <- replicate(N_BOOT, run_length_slopes(resample_runs(no_mutation_alpha_runs)))

format_estimate <- function(estimate, samples) {
  sprintf("%.3f [%.3f, %.3f]", estimate,
          quantile(samples, 0.025, na.rm = TRUE), quantile(samples, 0.975, na.rm = TRUE))
}

run_length_table <- data.frame(
  parameter = params,
  sapply(run_length_measures, function(measure) {
    sapply(params, function(parameter) {
      format_estimate(run_length_point[measure, parameter],
                      run_length_bootstrap[measure, parameter, ])
    })
  }),
  row.names = NULL)

cat("\nno_mutation_alpha: change in log10 run length (low -> high) split into",
    "generations and evaluations per generation, 95% bootstrap CI:\n")
print(run_length_table, row.names = FALSE)

cat("\nno_mutation_alpha: log10 fitness per log10 generations vs per log10",
    "evaluations per generation, 95% bootstrap CI:\n")
print(data.frame(slope = names(slopes_point),
                 estimate = sapply(names(slopes_point), function(name) {
                   format_estimate(slopes_point[[name]], slopes_bootstrap[name, ])
                 }),
                 row.names = NULL),
      row.names = FALSE)

no_mutation_alpha_runs %>%
  transmute(alpha,
            `log10(generations)`                = log_generations,
            `log10(evaluations per generation)` = log_evaluations_per_generation) %>%
  tidyr::pivot_longer(-alpha, names_to = "run_length_part", values_to = "value") %>%
  ggplot(aes(alpha, value, fill = alpha)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~ run_length_part, scales = "free_y") +
  labs(x = "alpha caste size", y = NULL,
       title = "no_mutation_alpha: run length parts by alpha caste size") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

# Where the evaluations go. One point per run, x = log10(generations),
# y = log10(evaluations per generation). Because log10(evaluations) is their
# sum, the dashed diagonals are lines of equal total evaluations. The arrow in
# each panel joins the median run of the low and the high level of that
# parameter: pointing right = longer runs, up = more expensive generations,
# along a diagonal = same total evaluations spent differently.
runs_by_parameter_level <- bind_rows(lapply(params, function(parameter) {
  no_mutation_alpha_runs %>%
    transmute(parameter   = parameter,
              level       = as.character(.data[[parameter]]),
              level_rank  = ifelse(as.integer(.data[[parameter]]) == 1, "low", "high"),
              log_generations, log_evaluations_per_generation, log_fit)
})) %>%
  mutate(parameter  = factor(parameter, levels = params),
         level_rank = factor(level_rank, levels = c("low", "high")))

level_medians <- runs_by_parameter_level %>%
  group_by(parameter, level_rank, level) %>%
  summarise(log_generations                = median(log_generations),
            log_evaluations_per_generation = median(log_evaluations_per_generation),
            .groups = "drop")

level_arrows <- level_medians %>%
  select(parameter, level_rank, log_generations, log_evaluations_per_generation) %>%
  tidyr::pivot_wider(names_from = level_rank,
                     values_from = c(log_generations, log_evaluations_per_generation))

level_names <- level_medians %>%
  group_by(parameter) %>%
  summarise(label = paste0("low = ",   level[level_rank == "low"],
                           ", high = ", level[level_rank == "high"]),
            .groups = "drop")

generations_range    <- range(no_mutation_alpha_runs$log_generations)
per_generation_range <- range(no_mutation_alpha_runs$log_evaluations_per_generation)
equal_evaluation_lines <- data.frame(log_evaluations = seq(4.5, 7, by = 0.25)) %>%
  mutate(label_x = generations_range[2],
         label_y = log_evaluations - label_x) %>%
  filter(label_y >= per_generation_range[1], label_y <= per_generation_range[2]) %>%
  mutate(label = sprintf("10^%.2f evaluations", log_evaluations))

rank_colours <- c(low = "#1b9e77", high = "#d95f02")

ggplot(runs_by_parameter_level,
       aes(log_generations, log_evaluations_per_generation, colour = level_rank)) +
  geom_abline(data = equal_evaluation_lines, aes(intercept = log_evaluations, slope = -1),
              linetype = "dashed", colour = "grey75", linewidth = 0.3) +
  geom_text(data = equal_evaluation_lines, aes(label_x, label_y, label = label),
            hjust = 1, vjust = -0.4, size = 2.3, colour = "grey50", inherit.aes = FALSE) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_segment(data = level_arrows,
               aes(x = log_generations_low, y = log_evaluations_per_generation_low,
                   xend = log_generations_high, yend = log_evaluations_per_generation_high),
               arrow = arrow(length = unit(0.18, "cm")), colour = "grey15",
               linewidth = 0.7, inherit.aes = FALSE) +
  geom_point(data = level_medians, size = 2.5) +
  geom_text(data = level_names, aes(label = label), x = -Inf, y = Inf,
            hjust = -0.05, vjust = 1.3, size = 2.6, colour = "grey30", inherit.aes = FALSE) +
  facet_wrap(~ parameter, nrow = 1) +
  scale_colour_manual(values = rank_colours, name = "Level") +
  labs(x = "log10(generations)", y = "log10(evaluations per generation)",
       title = "no_mutation_alpha: run length (generations) vs cost of a generation, by parameter level") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

# Companion of the fitness vs evaluations plot above, with generations on the
# x axis: the convergence lines now measure fitness gained per generation, so a
# parameter that only makes generations cheaper or dearer (alpha,
# population_size) shows up as a vertical gap, not as a shift along x.
ggplot(runs_by_parameter_level, aes(log_generations, log_fit, colour = level_rank)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.8) +
  geom_text(data = level_names, aes(label = label), x = -Inf, y = -Inf,
            hjust = -0.05, vjust = -0.6, size = 2.6, colour = "grey30", inherit.aes = FALSE) +
  facet_wrap(~ parameter, nrow = 1) +
  scale_colour_manual(values = rank_colours, name = "Level") +
  labs(x = "log10(generations)", y = "log10(diff_fitness)",
       title = "no_mutation_alpha: fitness vs generations by parameter level") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

# ---- FINDINGS (2026-11-07), section 7 -----------------------------------------
#
# F7. no_mutation_alpha: the alpha caste size splits the fitness bands, and
#     changes what a generation costs rather than how long a run lasts.
#     - Bands: alpha alone explains the two parallel bands of the fitness vs
#       evaluations cloud. At the same number of evaluations, 25% vs 10% alpha
#       is ~11x better (offset -1.05), r2_gain 0.62, agreement with the 2-band
#       split 0.91. Other parameters: agreement 0.50-0.58 (chance); max_gens
#       (offset -0.40) shifts fitness within each band, it does not make one.
#     - Total effect of 25% vs 10% alpha: -0.784 [-0.822, -0.749], ~6x better
#       fitness, with ~38% fewer evaluations (log10 change -0.205).
#     - Run length, medians:   alpha 10%: 128.0 generations, 3968 evaluations
#       per generation, 502263 evaluations; alpha 25%: 140.5, 2323, 315080.
#     - Split of the log10 change in evaluations (low -> high, 95% CI):
#                        evaluations   generations   evaluations/generation
#       alpha              -0.205        +0.037          -0.242
#       max_gens           +0.256        +0.277          -0.021
#       population_size    +0.331        +0.024          +0.306
#       steps              ~0            ~0              ~0
#       * alpha: a 25% alpha caste makes each generation ~43% cheaper and runs
#         ~9% MORE generations. Shorter runs do NOT come from earlier
#         stagnation. The saving is larger than the caste-size shift alone
#         allows (at most ~17% if alpha individuals cost nothing), so
#         individuals in the lower castes also spend fewer evaluations each:
#         gamma's local search stops improving sooner when it works on a
#         better-combined population.
#       * population_size: doubles the cost of a generation (0.306 ~ log10 2,
#         same cost per individual), barely changes the number of generations.
#       * max_gens: acts only through generations (more patience, longer
#         runs); via_generations -0.672 matches section 4's via_budget -0.686.
#     - Gamma: its local search takes most of the evaluations in a generation,
#       and more of them when the alpha caste is small, while those runs reach
#       ~6x worse fitness. It costs a lot of evaluations and contributes little
#       to the final fitness. Indirect evidence: the slope on evaluations per
#       generation is not identifiable (it is set by the configuration and
#       barely varies between runs; via_evaluations_per_generation CI for alpha
#       [-0.86, 0.53]).
#     - Consequence for the section 4 split in this version: for alpha and
#       population_size, via_budget prices per-generation cost at the run-length
#       slope (which comes from generations), so most of it reflects what a
#       generation costs, not how long the run is. Through generations the
#       run-length contributions are small: alpha -0.089, population_size -0.059.
#     - Energy/time reading: evaluations per generation is a cost set by the
#       configuration, mostly gamma's local search. A 25% alpha caste gives
#       ~6x better fitness for ~38% fewer evaluations; to be checked against
#       PKG and running time.

