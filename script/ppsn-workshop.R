## ----europar-poster.setup, echo=F, message=F---------------------------------------------------------------------
europar_poster_data <- read.csv("data/europar_poster_dataset.csv")
europar_base_count <- nrow( europar_poster_data[ startsWith(europar_poster_data$work,"base"),])
europar_workload_count <- nrow( europar_poster_data[ !startsWith(europar_poster_data$work,"base"),])


## ----europar-poster.initial, echo=F, message=F, fig.cap="Power vs. average initial temperature, with statistical distribution shown at the margin.", fig.height=2.5,out.width="100%", fig.pos="h!tbp"----
europar_poster_data$power <- europar_poster_data$PKG/europar_poster_data$seconds
library(ggplot2)
library(ggExtra)
distribution_run <- ggplot(europar_poster_data, aes(x=initial_temp, y=power, group=run, color=run))+geom_point()+ theme_minimal()+theme(legend.position="bottom")
ggMarginal(distribution_run, type="density", groupFill=T, alpha=0.5)


## ----europar-poster.cluster, echo=F, message=F, cache=T, fig.cap="Segment center values projected to the temperature plane (left) and thermal volatility (right) for both clusters, color representing cluster index", out.width="50%", fig.height=3, fig.pos="h!tbp", fig.show="hold"----
library(dplyr)
library(ecp)

# Create a new time series ID every time 'cum_seconds' restarts.
# We also initialize an empty segment_id to map our change-points later.
europar_poster_data <- europar_poster_data %>%
  mutate(
    restart = ifelse(row_number() == 1, 0, cum_seconds < lag(cum_seconds)),
    ts_id = cumsum(restart) + 1,
    segment_id = NA
  )

segments_list <- list()

# =========================================================================
# 2. Extract Segments, Compute Metrics & Map Global Indices
# =========================================================================
for (id in unique(europar_poster_data$ts_id)) {
  ts_data <- europar_poster_data %>% filter(ts_id == id)

  # Matrix for e.divisive multivariate analysis
  ts_matrix <- ts_data %>%
    select(initial_temp_1, initial_temp_2, power, seconds ) %>%
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
      global_indices <- which(europar_poster_data$ts_id == id)[start_idx:end_idx]
      europar_poster_data$segment_id[global_indices] <- i

      t1 <- segment_data[, "initial_temp_1"]
      t2 <- segment_data[, "initial_temp_2"]
      time <- segment_data[, "seconds"]
      pwr <- segment_data[, "power"]

      # Store both the averages and the new structural calculations
      segments_list[[length(segments_list) + 1]] <- data.frame(
        ts_id = id,
        segment_id = i,
        start_idx = start_idx,
        end_idx = end_idx,
        avg_temp_1 = mean(t1, na.rm = TRUE),
        avg_temp_2 = mean(t2, na.rm = TRUE),
        avg_power  = mean(pwr, na.rm = TRUE),
        var_T1       = var(t1, na.rm = TRUE),
        var_T2       = var(t2, na.rm = TRUE),
        avg_time     = mean( time, na.rm = TRUE)
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
  select(avg_temp_1, avg_temp_2, avg_power, avg_time)

scaled_features <- scale(cluster_features)

# Run K-means with 3 clusters
km_res <- kmeans(scaled_features, centers = 3)
segments_df$cluster <- as.factor(km_res$cluster)

# Dynamically assign high-power / low-power labels
power_means <- aggregate(segments_df$avg_power, by=list(cluster=segments_df$cluster), FUN=mean)
high_power_idx <- power_means$cluster[which.max(power_means$x)]
low_power_idx <- power_means$cluster[which.min(power_means$x)]

segments_df <- segments_df %>%
  mutate(
    power_label = ifelse(cluster == high_power_idx, "high-power", ifelse(cluster == low_power_idx, "low-power", "mid-power"))
  )

# =========================================================================
# 4. Visualization: Averages vs Architectural Characteristics
# =========================================================================

# Plot A: Standard Scatter showing the clustering on averages
ggplot(segments_df, aes(x = avg_temp_1, y = avg_temp_2, color = power_label)) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Cluster View: Temp 1 vs Temp 2", color = "Power Regime")

# Plot B: Standard Scatter showing the clustering on averages for time and power
ggplot(segments_df, aes(x = avg_time, y = avg_power, color = power_label)) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Cluster View: Temp 1 vs Temp 2", color = "Power Regime")

# Plot D: Boxplot showing Total Thermal Volatility
ggplot(segments_df, aes(x = power_label, y = total_var, fill = power_label)) +
  geom_boxplot(alpha = 0.7, notch = TRUE) +
  theme_minimal() +
  labs(title = "Total Thermal Volatility", x = "", y = "Var(T1) + Var(T2)") +
  theme(legend.position = "none")

