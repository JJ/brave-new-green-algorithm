# Fitness model for the PPSN-2026 BNA versions, mediated by the number of evaluations.
#
# Question: how much of the effect of every algorithm parameter (and of every
# algorithm version) on the fitness reached is due to "searching more"
# (more evaluations, indirect/mediated effect) and how much to "searching better"
# (better fitness at the same number of evaluations, direct effect)?
#
# Run from the repository root:
#   Rscript script/ppsn-fitness-mediation.R
#
# Notes
# - Only workload runs are used; fitness does not depend on operating conditions
#   (temperature, time), so versions run on different days can be pooled.
# - Response: log10(diff_fitness); mediator: log10(evaluations). In log-log
#   scale the slope is a convergence rate (orders of magnitude of fitness gained
#   per order of magnitude of evaluations).
# - The stopping criterion (max_gens generations without improvement) makes
#   evaluations partly a *consequence* of fitness progress, so the direct effect
#   must be read as "conditional on the same search effort", not strictly causal.
#   Section 3 separates within-configuration slopes (stopping-rule artefact +
#   luck) from between-configuration slopes (parameter effect).

library(dplyr)
library(ggplot2)

N_BOOT <- 500

# ---- 1. Data ------------------------------------------------------------------

versions <- c(baseline      = "ppsn_speedup_fixed_gradient_descent",
              gamma_upgrade = "gamma_upgrade",
              microopt      = "ppsn_microopt",
              low_mutation  = "ppsn_low_mutation",
              no_alpha      = "ppsn_no_alpha")

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
           fit_adj   = resid(lm(f_y_par, d)) + mean(log_fit),
           evals_adj = resid(lm(f_x_par, d)) + mean(log_evals))
  }) %>%
  ungroup()

slope_labels <- slopes %>%
  mutate(group = factor(group, levels = names(versions)),
         label = sprintf("slope %.2f [%.2f, %.2f]", slope, lower, upper))

ggplot(partial, aes(evals_adj, fit_adj, colour = group)) +
  geom_point(alpha = 0.2, size = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.8) +
  geom_text(data = slope_labels, aes(label = label),
            x = -Inf, y = -Inf, hjust = -0.05, vjust = -0.6,
            size = 2.8, colour = "grey20", inherit.aes = FALSE) +
  facet_wrap(~ group, nrow = 1) +
  labs(x = "log10(evaluations), adjusted for parameters",
       y = "log10(diff_fitness), adjusted for parameters") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

# ---- 2. Pooled models: total effect, mediator, direct effect + slopes ---------

f_total <- as.formula(paste("log_fit ~ group *", param_terms))
f_med   <- as.formula(paste("log_evals ~ group *", param_terms))
f_out   <- as.formula(paste("log_fit ~ group *", param_terms, "+ group:log_evals"))

m_total <- lm(f_total, data = fitness_data)
m_med   <- lm(f_med,   data = fitness_data)
m_out   <- lm(f_out,   data = fitness_data)

cat("\nR^2  total:", round(summary(m_total)$r.squared, 3),
    " mediator:", round(summary(m_med)$r.squared, 3),
    " direct:", round(summary(m_out)$r.squared, 3), "\n")

# Convergence rate (log-log slope) per version, with 95% CI
slopes <- confint(m_out)[grep(":log_evals$", names(coef(m_out))), , drop = FALSE]
slopes <- data.frame(group = sub("^group(.*):log_evals$", "\\1", rownames(slopes)),
                     slope = coef(m_out)[rownames(slopes)],
                     lower = slopes[, 1], upper = slopes[, 2], row.names = NULL)
cat("\nlog10(diff_fitness) vs log10(evaluations) slope per version (more negative = more efficient):\n")
print(slopes, digits = 3)


# Sanity check: operating conditions should NOT explain fitness. Only meaningful
# if temperatures are added to fitness_data; left here as a reminder.

# ---- 3. Within vs between configuration slopes --------------------------------

fitness_data <- fitness_data %>%
  group_by(group, alpha, steps, max_gens, population_size) %>%
  mutate(log_evals_cell   = mean(log_evals),
         log_evals_within = log_evals - log_evals_cell) %>%
  ungroup()

m_within_between <- lm(log_fit ~ group * (log_evals_cell + log_evals_within),
                       data = fitness_data)

wb <- data.frame(t(sapply(levels(fitness_data$group), function(g) {
  ref <- g == levels(fitness_data$group)[1]
  b <- coef(m_within_between)
  c(between = unname(b["log_evals_cell"]   + if (ref) 0 else b[paste0("group", g, ":log_evals_cell")]),
    within  = unname(b["log_evals_within"] + if (ref) 0 else b[paste0("group", g, ":log_evals_within")]))
})))
cat("\nBetween-configuration (parameter-driven) vs within-configuration (seed/stopping-driven) slopes:\n")
print(wb, digits = 3)

# ---- 4. Mediation per version and parameter (bootstrap) ----------------------
#
# Within each version: mediator model log_evals ~ params^2 and outcome model
# log_fit ~ params^2 + log_evals. For a two-level parameter, effects are
# average marginal effects of switching it from its low to its high level,
# averaged over the observed values of the other parameters. With a single
# mediator slope b, total = direct + indirect exactly (OLS, same regressors).

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
    total    <- marginal_effect(mt, data, p)
    direct   <- marginal_effect(my, data, p)
    indirect <- b * marginal_effect(mm, data, p)
    data.frame(param = p, total = total, direct = direct, indirect = unname(indirect))
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
           total_lo  = ci("total", 0.025),    total_hi  = ci("total", 0.975),
           direct_lo = ci("direct", 0.025),   direct_hi = ci("direct", 0.975),
           indirect_lo = ci("indirect", 0.025), indirect_hi = ci("indirect", 0.975),
           prop_mediated = indirect / total) %>%
    relocate(group)
}

mediation_table <- bind_rows(lapply(levels(fitness_data$group), mediation_by_group))

cat("\nEffect on log10(diff_fitness) of switching each parameter low -> high",
    "(negative = better fitness), 95% bootstrap CI:\n")
print(mediation_table %>%
        transmute(group, param,
                  total    = sprintf("%.3f [%.3f, %.3f]", total, total_lo, total_hi),
                  direct   = sprintf("%.3f [%.3f, %.3f]", direct, direct_lo, direct_hi),
                  indirect = sprintf("%.3f [%.3f, %.3f]", indirect, indirect_lo, indirect_hi),
                  prop_mediated = round(prop_mediated, 2)),
      row.names = FALSE)

# ---- 5. Version effects (consecutive pairs, as in the paper) ------------------
#
# Same decomposition, with the version change as the "treatment" and the four
# parameters as covariates. microopt vs baseline is a negative control:
# an implementation-only change should have ~0 total and direct effect.

version_pairs <- list(c("baseline", "gamma_upgrade"),
                      c("baseline", "microopt"),
                      c("microopt", "low_mutation"),
                      c("low_mutation", "no_alpha"))

version_effect <- function(data) {
  mm <- lm(as.formula(paste("log_evals ~ group *", param_terms)), data)
  my <- lm(as.formula(paste("log_fit ~ group *", param_terms, "+ log_evals")), data)
  mt <- lm(as.formula(paste("log_fit ~ group *", param_terms)), data)
  b <- coef(my)["log_evals"]
  c(total    = marginal_effect(mt, data, "group"),
    direct   = marginal_effect(my, data, "group"),
    indirect = unname(b * marginal_effect(mm, data, "group")))
}

version_table <- do.call(rbind, lapply(version_pairs, function(pair) {
  d <- fitness_data %>% filter(group %in% pair) %>%
    mutate(group = factor(as.character(group), levels = pair))
  point <- version_effect(d)
  boots <- replicate(N_BOOT, version_effect(d[sample(nrow(d), replace = TRUE), ]))
  q <- apply(boots, 1, quantile, c(0.025, 0.975))
  data.frame(change = paste(pair, collapse = " -> "),
             total    = sprintf("%.3f [%.3f, %.3f]", point["total"],    q[1, "total"],    q[2, "total"]),
             direct   = sprintf("%.3f [%.3f, %.3f]", point["direct"],   q[1, "direct"],   q[2, "direct"]),
             indirect = sprintf("%.3f [%.3f, %.3f]", point["indirect"], q[1, "indirect"], q[2, "indirect"]),
             prop_mediated = round(point["indirect"] / point["total"], 2),
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

