load("data/europar_test.rds") # Indistinct die
load("data/europar_taskset.rds") # Only die 1
load("data/europar_taskset_die2.rds") # Only die 2

europar_poster_dataset <- rbind(europar_test,europar_taskset,europar_taskset_die2)
europar_poster_dataset$run <- ifelse( startsWith(europar_poster_dataset$work, "base"), "B", "W")

europar_poster_dataset$alpha <- NULL

time_model <- glm( seconds ~ initial_temp_1*initial_temp_2 + run, data=europar_poster_dataset)
europar_poster_dataset$residual_seconds <- residuals(time_model)

square_energy_model <- glm( PKG ~ initial_temp_1*initial_temp_2 + run*dimension*population_size + residual_seconds + I(residual_seconds^2), data=europar_poster_dataset)

anova_power_model <- anova(square_energy_model)

double_square_energy_model <- glm( PKG ~ initial_temp_1*initial_temp_2 + run*dimension*population_size + residual_seconds + I(residual_seconds^2)+ I(initial_temp_1^2)*I(initial_temp_2^2), data=europar_poster_dataset)

all_vars_square_energy_model <- glm( PKG ~ initial_temp_1*initial_temp_2 +
                                       run*dimension*population_size*max_gens + residual_seconds +
                                       I(residual_seconds^2)+ I(initial_temp_1^2)*I(initial_temp_2^2), data=europar_poster_dataset)

europar_poster_dataset$generations[is.na(europar_poster_dataset$generations)] <- 0
europar_poster_dataset$evaluations[is.na(europar_poster_dataset$evaluations)] <- 0

also_workload_vars_square_energy_model <- glm( PKG ~ initial_temp_1*initial_temp_2 +
                                       run*dimension*population_size*max_gens + residual_seconds +
                                         run:evaluations+run:generations+
                                       I(residual_seconds^2)+ I(initial_temp_1^2)*I(initial_temp_2^2), data=europar_poster_dataset)


# 1. Calculate Explained Deviance (Pseudo R-squared)
# Formula: 1 - (Residual Deviance / Null Deviance)
pseudo_r2_sq <- 1 - (square_energy_model$deviance / square_energy_model$null.deviance)
pseudo_r2_dsq <- 1 - (double_square_energy_model$deviance / double_square_energy_model$null.deviance)

cat("Square Model Explained Variance: ", round(pseudo_r2_sq * 100, 2), "%\n")
cat("Double Square Model Explained Variance: ", round(pseudo_r2_dsq * 100, 2), "%\n")

# 2. Direct AIC Comparison
# Lower is better.
AIC(square_energy_model, double_square_energy_model)

# 3. Formal Statistical Comparison (F-test)
# This tests if the reduction in deviance is statistically significant
# given the 3 degrees of freedom you lost by adding the new variables.
anova(square_energy_model, double_square_energy_model, test="F")

# --- 3. EUROPAR POSTER PLOT (STRICTLY OBSERVED RANGE) ---
library(ggplot2)

# 1. Prepare the raw data points
point_data <- europar_poster_dataset
point_data$run_type <- ifelse(point_data$run == "B", "Baseline", "Workload (runW)")
point_data$run_type <- factor(point_data$run_type, levels = c("Baseline", "Workload (runW)"))

# 2. Generate dummy data constrained strictly to the actual data range
# Using min and max of the actual residuals to avoid extrapolation
time_seq <- seq(min(point_data$residual_seconds, na.rm = TRUE),
                max(point_data$residual_seconds, na.rm = TRUE),
                length.out = 200)

dummy_base <- data.frame(
  residual_seconds = time_seq,
  run = "B",
  initial_temp_1 = mean(point_data$initial_temp_1, na.rm = TRUE),
  initial_temp_2 = mean(point_data$initial_temp_2, na.rm = TRUE),
  dimension = mean(point_data$dimension, na.rm = TRUE),
  population_size = mean(point_data$population_size, na.rm = TRUE)
)

dummy_work <- dummy_base
dummy_work$run <- "W"

# Predict the absolute energy (PKG) for the smooth curves
energy_baseline <- predict(square_energy_model, newdata = dummy_base)
energy_workload <- predict(square_energy_model, newdata = dummy_work)

# Compile the curves into a tidy data frame
curve_data <- data.frame(
  residual_seconds = rep(time_seq, 2),
  energy = c(energy_baseline, energy_workload),
  run_type = factor(rep(c("Baseline", "Workload (runW)"), each = length(time_seq)),
                    levels = c("Baseline", "Workload (runW)"))
)

# 3. Build the plot without the extrapolation
poster_plot <- ggplot() +
  # Plot the raw experimental data points first
  geom_point(data = point_data, aes(x = residual_seconds, y = PKG, color = run_type),
             alpha = 0.15, size = 1.5, stroke = 0) +

  # Draw the predicted model curves strictly within the observed range
  geom_line(data = curve_data, aes(x = residual_seconds, y = energy, color = run_type),
            linewidth = 1.5) +

  # Add labels and formatting
  labs(
    title = "Total Package Energy by Run Type and Execution Time",
    subtitle = "Quadratic model fit constrained to observed execution times (adjusted for average temperature)",
    x = "Residual Seconds (Unexpected Execution Time)",
    y = "Total Package Energy (Joules)",
    color = "Experiment Phase"
  ) +
  scale_color_manual(values = c("Baseline" = "#1f77b4", "Workload (runW)" = "#d62728")) +

  # Ensure the legend points are opaque
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +

  # Clean academic theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

# Render the plot
print(poster_plot)
