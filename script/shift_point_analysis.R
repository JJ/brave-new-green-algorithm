library(dplyr)
library(ecp)
library(ggplot2)
library(gridExtra)
library(rpart)
library(rpart.plot)

# =========================================================================
# 1. Read Data and Split Time Series
# =========================================================================
df <- read.csv("data/europar_poster_dataset.csv")

# Create a new time series ID every time 'cum_seconds' restarts.
# We also initialize an empty segment_id to map our change-points later.
df <- df %>%
  mutate(
    restart = ifelse(row_number() == 1, 0, cum_seconds < lag(cum_seconds)),
    ts_id = cumsum(restart) + 1,
    segment_id = NA
  )

segments_list <- list()

# =========================================================================
# 2. Extract Segments, Compute Metrics & Map Global Indices
# =========================================================================
for (id in unique(df$ts_id)) {
  ts_data <- df %>% filter(ts_id == id)

  # Matrix for e.divisive multivariate analysis
  ts_matrix <- ts_data %>%
    select(initial_temp_1, initial_temp_2, power) %>%
    as.matrix()

  if (nrow(ts_matrix) > 5) {
    # Perform change-point analysis (NO set.seed() used!)
    cp_result <- e.divisive(ts_matrix, sig.lvl = 0.05, R = 199)
    cps <- cp_result$estimates

    for (i in 1:(length(cps) - 1)) {
      start_idx <- cps[i]
      end_idx <- cps[i+1] - 1
      segment_data <- ts_matrix[start_idx:end_idx, , drop = FALSE]

      # Map the segment ID back to the exact rows in the raw dataframe
      # (Crucial for the decision tree feature engineering later)
      global_indices <- which(df$ts_id == id)[start_idx:end_idx]
      df$segment_id[global_indices] <- i

      t1 <- segment_data[, "initial_temp_1"]
      t2 <- segment_data[, "initial_temp_2"]
      pwr <- segment_data[, "power"]

      # Safe calculation for correlation (needs >2 points and variance)
      if (length(t1) > 2 && sd(t1, na.rm = TRUE) > 0 && sd(t2, na.rm = TRUE) > 0) {
        intra_cor <- cor(t1, t2, use = "complete.obs")
      } else {
        intra_cor <- NA
      }

      # Store both the averages and the new structural calculations
      segments_list[[length(segments_list) + 1]] <- data.frame(
        ts_id = id,
        segment_id = i,
        start_idx = start_idx,
        end_idx = end_idx,
        avg_temp_1 = mean(t1, na.rm = TRUE),
        avg_temp_2 = mean(t2, na.rm = TRUE),
        avg_power  = mean(pwr, na.rm = TRUE),
        intra_cor  = intra_cor,
        delta_T_mean = mean(abs(t1 - t2), na.rm = TRUE),
        var_T1       = var(t1, na.rm = TRUE),
        var_T2       = var(t2, na.rm = TRUE)
      )
    }
  }
}

segments_df <- bind_rows(segments_list)

# Compute COMBINED Total Thermal Volatility (Var T1 + Var T2)
segments_df <- segments_df %>%
  mutate(total_var = var_T1 + var_T2)

# =========================================================================
# 3. Cluster USING ONLY AVERAGES
# =========================================================================

# Select only the 3 original variables for K-means and scale them
cluster_features <- segments_df %>%
  select(avg_temp_1, avg_temp_2, avg_power)

scaled_features <- scale(cluster_features)

# Run K-means (NO set.seed() used!)
km_res <- kmeans(scaled_features, centers = 2)
segments_df$cluster <- as.factor(km_res$cluster)

# Dynamically assign high-power / low-power labels
power_means <- aggregate(segments_df$avg_power, by=list(cluster=segments_df$cluster), FUN=mean)
high_power_idx <- power_means$cluster[which.max(power_means$x)]

segments_df <- segments_df %>%
  mutate(
    power_label = ifelse(cluster == high_power_idx, "high-power", "low-power")
  )

# =========================================================================
# 4. Visualization: Averages vs Architectural Characteristics
# =========================================================================

# Plot A: Standard Scatter showing the clustering on averages
p_scatter <- ggplot(segments_df, aes(x = avg_temp_1, y = avg_temp_2, color = power_label)) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Cluster View: Temp 1 vs Temp 2", color = "Power Regime")

# Plot D: Boxplot showing Total Thermal Volatility
p_var <- ggplot(segments_df, aes(x = power_label, y = total_var, fill = power_label)) +
  geom_boxplot(alpha = 0.7, notch = TRUE) +
  theme_minimal() +
  labs(title = "Total Thermal Volatility", x = "", y = "Var(T1) + Var(T2)") +
  theme(legend.position = "none")

# Arrange all 4 plots together
grid.arrange(p_scatter, p_var, ncol = 2)

# =========================================================================
# 5. Statistical Validation: Wilcoxon Rank-Sum Tests
# =========================================================================
# We use exact = FALSE to suppress warnings regarding ties,
# which are highly likely to occur in derived metrics like variance and correlation.

# 1. Intra-segment Correlation
test_cor <- wilcox.test(intra_cor ~ power_label, data = segments_df, exact = FALSE)

# 2. Thermal Asymmetry (Delta T)
test_delta <- wilcox.test(delta_T_mean ~ power_label, data = segments_df, exact = FALSE)

# 3. Total Thermal Volatility (Var T1 + Var T2)
test_var <- wilcox.test(total_var ~ power_label, data = segments_df, exact = FALSE)

# Compile the results into a clean dataframe
wilcox_results <- data.frame(
  Metric = c("Intra-segment Correlation",
             "Thermal Asymmetry (Delta T)",
             "Total Thermal Volatility (Var T1+T2)"),
  W_Statistic = c(test_cor$statistic,
                  test_delta$statistic,
                  test_var$statistic),
  P_Value = c(test_cor$p.value,
              test_delta$p.value,
              test_var$p.value)
)

# Add a quick boolean column for standard significance (alpha = 0.05)
wilcox_results$Significant <- wilcox_results$P_Value < 0.05

# Format p-values for cleaner console output (avoids messy scientific notation for larger numbers)
wilcox_results$P_Value <- formatC(wilcox_results$P_Value, format = "e", digits = 3)

cat("\n--- Wilcoxon Rank-Sum Test Results (High-Power vs Low-Power) ---\n")
print(wilcox_results, row.names = FALSE)

# =========================================================================
# 7. Train Telemetry Tree on Pre-Shift States (WITH CLASS BALANCING)
# =========================================================================
# We only care about predicting shifts *from* low power.
shift_df <- telemetry_df %>%
  filter(power_label == "low-power") %>%
  mutate(
    will_shift = as.factor(ifelse(future_regime == "high-power", "Shift to High", "Stay Low"))
  )

# Separate the rare shift events from the common stay events
shifts_only <- shift_df %>% filter(will_shift == "Shift to High")
stays_only  <- shift_df %>% filter(will_shift == "Stay Low")

if (nrow(shifts_only) > 0) {

  # Downsample the "Stay Low" class so the tree doesn't get lazy.
  # We randomly sample 3 times as many 'Stay Low' as 'Shift to High'
  # to give it balanced context. (No set.seed used!)
  stays_sampled <- stays_only %>% sample_n(min(n(), nrow(shifts_only) * 3))

  balanced_shift_df <- bind_rows(shifts_only, stays_sampled)

  # Build the tree using the balanced dataset and a highly sensitive 'cp'
  telemetry_tree <- rpart(
    will_shift ~ initial_temp_1 + initial_temp_2 + power +
      temp_1_slope + temp_2_slope + power_slope,
    data = balanced_shift_df,
    method = "class",
    control = rpart.control(cp = 0.02, minsplit = 5)
  )

  # Plot the causal telemetry tree
  rpart.plot(telemetry_tree,
             main = "Telemetry Precursors: Causes of Low-to-High Regime Shifts",
             type = 2,
             extra = 104,
             box.palette = "RdBu",
             shadow.col = "gray",
             nn = TRUE,
             tweak=2)
  # =========================================================================
  # 8. Extract and Print Exact Variable Ranges and Outcomes
  # =========================================================================

  cat("\n--- Exact Telemetry Rules, Variable Ranges, and Outcomes ---\n")

  # rpart.rules directly extracts every single path to a leaf node,
  # showing the outcome, the percentage of data it covers, and the exact ranges.
  rpart.rules(telemetry_tree,
              style = "tall",      # "tall" puts each condition on a new line for perfect readability
              cover = TRUE)        # Shows what percentage of cases fall into this rule profile
} else {
  print("No low-to-high intra-series shifts were found in the segmented data.")
}
