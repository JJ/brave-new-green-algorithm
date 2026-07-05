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
regular_vs_hot_first_time_model <- glm( seconds ~ work*dimension*population_size, data=regular_vs_hot_first)

regular_vs_hot_first_temp1_model <- glm( initial_temp_1 ~ work*dimension*population_size+prev_temp_1*prev_temp_2, data=regular_vs_hot_first)
regular_vs_hot_first_temp2_model <- glm( initial_temp_2 ~ work*dimension*population_size, data=regular_vs_hot_first)

regular_vs_hot_first$residual_seconds <- residuals(regular_vs_hot_first_time_model)
regular_vs_hot_first$residual_i_temp_1 <- residuals(regular_vs_hot_first_temp1_model)
regular_vs_hot_first$residual_i_temp_2 <- residuals(regular_vs_hot_first_temp2_model)

regular_vs_hot_first_PKG_model <- glm( PKG ~ work*dimension*population_size +
                                        residual_i_temp_1*residual_i_temp_2+
                                        I(residual_i_temp_1^2)*I(residual_i_temp_2^2)+
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
