load("data/europar_test.rds")
load("data/europar_taskset.rds")
load("data/europar_taskset_die2.rds")

library(dplyr)
regular <- rbind(europar_test, europar_taskset, europar_taskset_die2)

regular <- regular %>%
  # Create a common identifier for the sandwich (e.g., both become "europar-test")
  mutate(experiment_block = sub("^base-", "", work)) %>%
  # Group by the unified block
  group_by(experiment_block) %>%
  # CRITICAL: Rows must be in chronological order of execution.
  # If you have an execution index/timestamp, uncomment the line below:
  # arrange(execution_time, .by_group = TRUE) %>%
  mutate(
    prev_temp_1 = lag(initial_temp_1),
    prev_temp_2 = lag(initial_temp_2),
    # Capturing the previous work tag is a great sanity check
    prev_work = lag(work)
  ) %>%
  ungroup()

regular_base <- regular[ startsWith(regular$work, "base"),]
regular_base$work <- "regular"

load("data/icsm_hot_first.rds")
icsm_hot_first <- icsm_hot_first %>%
  mutate(experiment_block = sub("^base-", "", work)) %>%
  group_by(experiment_block) %>%
  mutate(
    prev_temp_1 = lag(initial_temp_1),
    prev_temp_2 = lag(initial_temp_2),
    prev_work = lag(work)
  ) %>%
  ungroup()
hot_first_base <- icsm_hot_first[ startsWith(icsm_hot_first$work,"base"), ]
hot_first_base$work <- "hot-first"

regular_vs_hot_first <- rbind(regular_base,hot_first_base)

library(ggplot2)

ggplot(regular_vs_hot_first, aes(x=seconds, y=PKG, group=work, color=work))+
  geom_point()+theme_minimal()

regular_vs_hot_first$dimension <- as.factor(regular_vs_hot_first$dimension)
regular_vs_hot_first$population_size <- as.factor(regular_vs_hot_first$population_size)
regular_vs_hot_first_time_model <- glm( seconds ~ work*dimension*population_size+initial_temp_1*initial_temp_2, data=regular_vs_hot_first)

regular_vs_hot_first_temp1_model <- glm( initial_temp_1 ~ work*dimension*population_size+prev_temp_1*prev_temp_2+I(prev_temp_1^2)*I(prev_temp_2^2), data=regular_vs_hot_first)
regular_vs_hot_first_temp2_model <- glm( initial_temp_2 ~ work*dimension*population_size+prev_temp_1*prev_temp_2+I(prev_temp_1^2)*I(prev_temp_2^2), data=regular_vs_hot_first)

regular_vs_hot_first$residual_seconds <- residuals(regular_vs_hot_first_time_model)

regular_vs_hot_first_PKG_model <- glm( PKG ~ work*dimension*population_size +
                                        initial_temp_1*initial_temp_2+
                                        I(initial_temp_1^2)*I(initial_temp_2^2)+
                                        residual_seconds + I(residual_seconds^2),
                                       data=regular_vs_hot_first)

ggplot(regular_vs_hot_first, aes(x=initial_temp_1, y=initial_temp_2, group=work, color=work))+
  geom_point()+theme_minimal()

ggplot(regular_vs_hot_first, aes(x=initial_temp_1, y=initial_temp_2, group=work, color=work)) +
  geom_point(alpha = 0.3) +
  geom_density_2d(linewidth = 0.8) +
  theme_minimal() +
  labs(title = "Temperature Distribution: Regular vs Hot-First")

library(ggExtra)

# First, assign your ggplot to an object
p <- ggplot(regular_vs_hot_first, aes(x=initial_temp_1, y=initial_temp_2, color=work)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "bottom") # Moving legend prevents squishing the marginal plots

# Add marginal densities grouped by the 'work' variable
ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE, alpha = 0.3)


#-----------
#
# install.packages("ggeffects") # Highly recommended for extracting GLM predictions
library(ggeffects)


# ---------------------------------------------------------
# PLOT 2: The Model Predictions (The Statistical Truth)
# ---------------------------------------------------------
# ggpredict cleanly extracts the isolated effect of your parameters
# while mathematically freezing the temperatures and residual time.
pred_pkg <- ggpredict(regular_vs_hot_first_PKG_model,
                      terms = c("dimension", "work", "population_size"))

ggplot(pred_pkg, aes(x = x, y = predicted, color = group, group = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 0.8) +
  facet_wrap(~facet, labeller = label_both) +
  theme_minimal() +
  scale_color_manual(values = c("hot-first" = "#e74c3c", "regular" = "#3498db")) +
  labs(title = "Model Predictions: Isolated Energy Footprint",
       subtitle = "Holding thermal carryover and OS time jitter constant",
       x = "Dimension",
       y = "Predicted Energy (PKG)",
       color = "Scheduler")

library(ggplot2)
# install.packages("ggExtra")
library(ggExtra)

# 1. Create the base plot with a quadratic fit
p_corrected <- ggplot(regular_vs_hot_first, aes(x = seconds, y = PKG, color = work)) +
  geom_point(alpha = 0.3, stroke = 0) +
  theme_minimal() +
  scale_color_manual(values = c("hot-first" = "#e74c3c", "regular" = "#3498db")) +
  # Move legend to the bottom so marginal plots have room
  theme(legend.position = "bottom") +
  labs(title = "Energy vs. Time: The Hot-First Advantage",
       subtitle = "Hot-first draws more peak power (steeper curve) but finishes faster, using less total energy",
       x = "Execution Time (seconds)",
       y = "Total Energy Consumed (PKG)")

# 2. Add marginal boxplots to show the true centers of mass
ggMarginal(p_corrected, type = "violin", groupColour = TRUE, groupFill = TRUE, alpha = 0.4)
