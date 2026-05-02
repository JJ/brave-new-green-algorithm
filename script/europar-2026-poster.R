load("data/europar_test.rds")
library(ggplot2)
europar_test$dimension <- as.factor(europar_test$dimension)
europar_test$base <- ifelse( startsWith(europar_test$work, "base"), TRUE, FALSE)
ggplot(europar_test, aes(x=initial_temp, y=PKG, color=base, shape=dimension)) + geom_point()  + labs( title = "Energy Consumption Over Temperature", x = "Temperature", y = "Energy Consumption " ) + theme_minimal()

ggplot(europar_test, aes(x=initial_temp_1))+geom_bar(color="red")+geom_bar(aes(x=initial_temp_2),color="blue", position="dodge")+theme_minimal()

## ----europar.initial.base.model, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Model of energy consumption for the baseline measures, initial", fig.height=4----
library(dplyr)
europar_test_base <- europar_test %>% filter(base == TRUE)
europar_test_base_model_linear <- glm( PKG ~ + initial_temp*dimension,data = europar_test_base)
europar_test_base_model <- glm( PKG ~ I(initial_temp^2) + initial_temp*dimension*population_size,data = europar_test_base)
difference_base_models <- anova(europar_test_base_model,europar_test_base_model_linear)


## ----europar.initial.workload.model, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Energy consumption for the workload measures, initial experiment.", fig.height=3.5, fig.pos="h!tb"----
source("R/process_deltas.R")
europar_test_processed <- process_deltas( europar_test )
europar_test_processed$dimension <- as.factor(europar_test_processed$dimension)
europar_test_processed$population_size <- as.factor(europar_test_processed$population_size)
ggplot(europar_test_processed, aes(x = initial_temp, y = delta_PKG)) +
  geom_point(color=europar_test_processed$dimension ) +
  labs(
    title = "Energy Consumption vs. Temperature",
    x = "Temperature",
    y = "Delta PKG"
  ) + theme_minimal()
europar_test_delta_model_linear <- glm( PKG ~ + initial_temp*dimension*population_size,data = europar_test_processed)
europar_test_delta_model <- glm( PKG ~ I(initial_temp^2) + initial_temp*dimension*population_size,data = europar_test_processed)
europar_test_delta_evals_model <- glm( PKG ~ I(initial_temp^2) + initial_temp*dimension*population_size+evaluations,data = europar_test_processed)
europar_test_delta_evals_interaction_model <- glm( PKG ~ I(initial_temp^2) + initial_temp*dimension+population_size*evaluations,data = europar_test_processed)
difference_delta_models <- anova(europar_test_delta_evals_model,europar_test_delta_model)
difference_delta_interaction <- anova(europar_test_delta_evals_interaction_model,europar_test_delta_evals_model)
europar_test_delta_seconds_model <- glm( PKG ~ initial_temp*dimension*population_size+evaluations*delta_seconds+I(initial_temp^2),data = europar_test_processed)
difference_delta_seconds <- anova(europar_test_delta_seconds_model,europar_test_delta_evals_model)
anova_delta_seconds <- anova(europar_test_delta_seconds_model)
library(equatiomatic)


## ----europar.initial.model.slopes, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Slopes of the model for the workload experiment.", fig.height=3----
library(marginaleffects)
plot_predictions(europar_test_delta_evals_model, condition = c("initial_temp","population_size", "dimension"))


## ----europar.base.time, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Time spent by the baseline experiment vs. temperature.", fig.height=4----
europar_test_base$population_size <- as.factor(europar_test_base$population_size)
europar_test_base_model_time <- glm( PKG ~ I(initial_temp^2) + initial_temp*dimension*population_size+seconds,data = europar_test_base)
anova_time <- anova(europar_test_base_model_time,europar_test_base_model)
anova_test_base_model_time <- anova(europar_test_base_model_time)


## ----europar.taskset, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Energy consumption for the workload experiment with taskset; (left) vs. T, (right) vs. time", fig.pos="h!tb", fig.height=4, fig.show="hold", out.width="50%"----
load("data/europar_taskset.rds")
europar_taskset$dimension <- as.factor(europar_taskset$dimension)
europar_taskset$population_size <- as.factor(europar_taskset$population_size)
ggplot(europar_taskset, aes(x=initial_temp_1, y=PKG, color=base, shape=dimension)) + geom_point()  + labs( x = "Temperature", y = "Energy Consumption " ) + theme_minimal()

europar_taskset_base <- europar_taskset %>% filter(base == TRUE)
europar_taskset_base_model <- glm( PKG ~ I(initial_temp^2) + initial_temp_1*dimension*population_size,data = europar_taskset_base)
europar_taskset_base_model_time <- glm( PKG ~ I(initial_temp^2) + initial_temp*dimension*population_size+seconds,data = europar_taskset_base)
anova_taskset_time <- anova(europar_taskset_base_model_time,europar_taskset_base_model)
anova_taskset_time_model <- anova(europar_taskset_base_model_time)
library(scales)
custom_pow <- trans_new(
  name = "custom_pow",
  transform = function(x) x^0.3,  # Lower power = more stretch at the bottom
  inverse = function(x) x^(1/0.3)
)

ggplot(europar_taskset_base, aes(x=seconds, y=PKG, color=initial_temp, shape=dimension)) +
  scale_color_viridis_c(trans=custom_pow) +
  geom_point()  + labs( x = "Seconds", y = "Energy Consumption " ) + theme_minimal() + xlim(6.75,8)+ylim(250,750)

ggplot(europar_taskset_base, aes(x=seconds, y=PKG, shape=dimension, color=factor(population_size))) +
  geom_point()  + labs( x = "Seconds", y = "Energy Consumption " ) + theme_minimal() + xlim(6.75,8)+ylim(250,750)

europar_taskset_base$power <- europar_taskset_base$PKG/europar_taskset_base$seconds

ggplot( europar_taskset_base, aes(x=initial_temp, y=power,color=dimension,shape=population_size))+geom_point()+theme_minimal()

# Fit a basic linear model of Energy vs Time
base_model <- lm(PKG ~ seconds, data = europar_taskset_base)

# Add the residuals (distance from the trendline) back to your dataset
europar_taskset_base$residuals <- resid(base_model)

# Plot the density to find the valley
d <- density(europar_taskset_base$residuals)
plot(d, main="Density of Residuals")

# 2. Extract the X (residual values) and Y (density/frequency) coordinates
x_vals <- d$x
y_vals <- d$y

# 3. Find where the slope changes from negative (going down the peak)
# to positive (going up the next peak).
# diff() calculates the difference between consecutive points.
# sign() converts those differences to +1 (increasing) or -1 (decreasing).
# diff(sign()) equals 2 exactly at the bottom of a valley (-1 to +1).
valley_indices <- which(diff(sign(diff(y_vals))) == 2) + 1

# 4. Create a data frame of all the local valleys found
valleys <- data.frame(residual_value = x_vals[valley_indices],
                      density = y_vals[valley_indices])

# 5. If there's minor noise, there might be multiple tiny valleys.
# We want the main, deepest one (lowest density). We use the last one, after the "flat" initial surface
exact_cutoff <- valleys$residual_value[5]

# Apply the exact mathematical cutoff to separate the streaks
europar_taskset_base$streak_group <- as.factor(ifelse(europar_taskset_base$residuals > exact_cutoff, "Upper_Streak", "Lower_Streak"))

streak_glm <- glm(streak_group ~ initial_temp_1*initial_temp_2 + population_size*dimension + seconds,
                  data = europar_taskset_base, # Using the subset where the split happens
                  family = binomial)

ggplot(europar_taskset_base, aes(x = initial_temp_1, y = initial_temp_2, color = streak_group)) +
  geom_point(size = 2, alpha = 0.7) +
  # Using distinct, contrasting colors to make the boundary pop
  scale_color_manual(values = c("Lower_Streak" = "#43aa8b", "Upper_Streak" = "#f94144")) +
  labs(title = "The True Cause of the Energy Split",
       subtitle = "Interaction between Initial Temp 1 and Initial Temp 2",
       x = "Initial Temperature 1",
       y = "Initial Temperature 2",
       color = "Energy Baseline") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14))

europar_taskset_base$delta_temp <- europar_taskset_base$initial_temp_1 - europar_taskset_base$initial_temp_2

ggplot(europar_taskset_base, aes(x=initial_temp_1, y=power,color=streak_group))+geom_point()+theme_minimal()

# Trying to find why they overlap
#


# install.packages("plotly")
library(plotly)

plot_ly(europar_taskset_base,
        x = ~initial_temp_1,
        y = ~power,
        z = ~initial_temp_2,
        color = ~streak_group,
        colors = c("#f94144", "#43aa8b"),
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 3, opacity = 0.8)) %>%
  layout(title = "Unmasking the Overlap in 3D")

power_model <- glm( power ~ initial_temp_2 * initial_temp_1, data=europar_taskset_base)
summary(power_model)

ggplot( europar_taskset_base, aes(x=initial_temp_2,y=power,color=initial_temp_2)) + scale_color_viridis_c(trans=custom_pow) + geom_point()+geom_smooth(formula=y~x, method="lm")+ theme_minimal()

anova_power <- anova(power_model)

## ----europar.model.comparison, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Comparison of the models for the baseline measurements with and without taskset.", fig.height=3, fig.pos="h!tb", out.width="100%"----
library(philentropy) # For KL function
library(stats)

joint_base_taskset <- rbind(europar_taskset_base, europar_test_base)

predict_with_taskset <- predict(europar_taskset_base_model_time, newdata=joint_base_taskset, type = "response")
predict_with_base <- predict(europar_test_base_model_time, newdata=joint_base_taskset, type = "response")

probability_taskset <- predict_with_taskset / sum(predict_with_taskset)
probability_base <- predict_with_base / sum(predict_with_base)

kl_divergence <- KL(rbind(probability_taskset, probability_base), unit = "log")


## ----europar.temperature.time, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Comparison of the models for the baseline experiment with and without taskset for different temperatures.", fig.height=5, fig.show="hold", out.width="30%"----
europar_taskset_base$Experiment <- "Taskset"
europar_test_base$Experiment <- "Base"

joint_europar <- rbind(europar_taskset_base, europar_test_base)
ggplot(joint_europar, aes(x=Experiment, y=PKG))+geom_violin()+labs( title = "Energy Consumption Comparison", x = "Experiment Type", y = "Energy Consumption " ) + theme_minimal()
wilcox_test_energy <- wilcox.test(PKG ~ Experiment, data = joint_europar)
ggplot(joint_europar, aes(x=Experiment, y=seconds))+geom_violin()+labs( title = "Time Comparison", x = "Experiment Type", y = "Time (s) " ) + theme_minimal()
wilcox_test_time <- wilcox.test(seconds ~ Experiment, data = joint_europar)
ggplot(joint_europar, aes(x=Experiment, y=initial_temp))+geom_violin()+labs( title = "Temperature Comparison", x = "Experiment Type", y = "Temperature" ) + theme_minimal()
wilcox_test_temperature <- wilcox.test(initial_temp ~ Experiment, data = joint_europar)


# Processed taskset
europar_taskset_processed <- process_deltas( europar_taskset )
europar_taskset_processed$dimension <- as.factor(europar_taskset_processed$dimension)
europar_taskset_processed$population_size <- as.factor(europar_taskset_processed$population_size)

europar_die1_workload <- rbind( europar_taskset_processed, europar_test_processed)


## ----europar.die2, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Energy consumption vs. Temperature for the workload, taskset on die 2.", fig.height=2.5,fig.pos="h!tb", out.width="100%"----
load("data/europar_taskset_die2.rds")
europar_taskset_die2$dimension <- as.factor(europar_taskset_die2$dimension)
europar_taskset_die2$population_size <- as.factor(europar_taskset_die2$population_size)

ggplot(europar_taskset_die2, aes(x=initial_temp, y=PKG, color=base, shape=dimension)) + geom_point()  + labs( title = "Energy Consumption Over Temperature", x = "Temperature", y = "Energy Consumption " ) + theme_minimal()

taskset_die2_base <- europar_taskset_die2 %>% filter(base == TRUE)

taskset_die2_time_model <- glm( PKG ~ I(initial_temp^2) + initial_temp*dimension*population_size+seconds,data = taskset_die2_base)
taskset_joint_time_model <- glm( PKG ~ I(initial_temp^2) + initial_temp*dimension*population_size+seconds,data =  joint_base_taskset)

joint_base_taskset_with_die2 <- rbind(joint_base_taskset, taskset_die2_base)
predict_with_die2 <- predict(taskset_die2_time_model, newdata=joint_base_taskset_with_die2, type = "response")
predict_with_joint <- predict(taskset_joint_time_model, newdata=joint_base_taskset_with_die2, type = "response")

probability_die2 <- predict_with_die2 / sum(predict_with_die2)
probability_joint <- predict_with_joint / sum(predict_with_joint)

kl_divergence_die2 <- KL(rbind(probability_die2, probability_joint), unit = "log")


## ----europar.die2.comparison, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Comparison of the models for the baseline experiment with and without taskset on die 2.", fig.height=4, fig.show="hold", out.width="30%"----
taskset_die2_base$Experiment <- "Taskset Die 2"
joint_base_taskset$Experiment <- "Die 1"

joint_base_with_die2 <- rbind(joint_base_taskset, taskset_die2_base)

ggplot(joint_base_with_die2, aes(x=Experiment, y=PKG))+geom_violin()+labs( title = "Energy Consumption Comparison", x = "Experiment Type", y = "Energy Consumption " ) + theme_minimal()
wilcox_test_energy <- wilcox.test(PKG ~ Experiment, data = joint_base_with_die2)
ggplot(joint_base_with_die2, aes(x=Experiment, y=seconds))+geom_violin()+labs( title = "Time Comparison", x = "Experiment Type", y = "Time (s) " ) + theme_minimal()
wilcox_test_time <- wilcox.test(seconds ~ Experiment, data = joint_base_with_die2)
ggplot(joint_base_with_die2, aes(x=Experiment, y=initial_temp))+geom_violin()+labs( title = "Temperature Comparison", x = "Experiment Type", y = "Temperature" ) + theme_minimal()
wilcox_test_temperature <- wilcox.test(initial_temp ~ Experiment, data = joint_base_with_die2)


## ----europar.die2.table, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Comparison of the models for the baseline experiment with and without taskset on die 2."----
joint_base_with_die2_summary <- joint_base_with_die2 %>%
  group_by(Experiment) %>%
  summarise(
    mean_energy = mean(PKG),
    trimmed_mean_energy = mean(PKG, trim = 0.1),
    iqr_energy = IQR(PKG),
    mean_time = mean(seconds),
    trimmed_mean_time = mean(seconds, trim = 0.1),
    iqr_time = IQR(seconds),
    mean_temperature = mean(initial_temp),
    trimmed_mean_temperature = mean(initial_temp, trim = 0.1),
    iqr_temperature = IQR(initial_temp)
  )
library(kableExtra)
kable(joint_base_with_die2_summary,
      digits = 2,
      col.names=c("Experiment","Mean", "Trim mean", "IQR",
                 "Mean", "Trim mean", "IQR",
                 "Mean", "Trim mean", "IQR"),
      caption = "Summary statistics for energy, time and temperature for the baseline experiment with and without taskset on die 2.") %>% add_header_above(c(" " = 1, "Energy (J)" = 3, "Time (s)" = 3, "Temperature (ºC)" = 3)) %>%
  kable_styling(full_width = FALSE, position = "center")


## ----europar.die2.workload, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Workload energy vs. time, die-2 experiment.", fig.height=2.5, fit.pos="h!", out.width="100%"----
europar_taskset_die2_processed <- process_deltas( europar_taskset_die2 )
europar_taskset_die2_processed$dimension <- as.factor(europar_taskset_die2_processed$dimension)
europar_taskset_die2_processed$population_size <- as.factor(europar_taskset_die2_processed$population_size)
ggplot(europar_taskset_die2_processed, aes(x = delta_seconds, y = delta_PKG)) +
  geom_point(color=europar_taskset_die2_processed$dimension ) +
  labs(
    title = "Energy Consumption vs. Temperature",
    x = "Delta seconds",
    y = "Delta PKG"
  ) + theme_minimal()


## ----europar.die2.workload.table, echo=FALSE, message=FALSE,warning=FALSE---------------------------------------
europar_taskset_die2_processed$Experiment <- "Die 2"
europar_test_processed$Experiment <- "No Die"
joint_workload_with_die2 <- rbind(europar_taskset_die2_processed, europar_test_processed)

summary_joint_workload_with_die2 <- joint_workload_with_die2 %>%
  group_by(dimension,population_size,Experiment) %>%
  summarise(
    mean_delta_energy = mean(delta_PKG),
    trimmed_mean_delta_energy = mean(delta_PKG, trim = 0.1),
    iqr_delta_energy = IQR(delta_PKG),
  )

kable(summary_joint_workload_with_die2,
      digits = 2,
      col.names=c("Dimension", "Population Size", "Experiment", "Delta Energy (J): mean", "Trim mean", "IQR"),
      caption = "Summary statistics for delta energy for the workload experiment with and without taskset on die 2.") %>%
  kable_styling(latex_options = "hold_position")

die2_iqr <- IQR(joint_workload_with_die2[ summary_joint_workload_with_die2$Experiment == "Die 2", ]$delta_PKG)
nodie_iqr <- IQR(joint_workload_with_die2[ summary_joint_workload_with_die2$Experiment == "No Die", ]$delta_PKG)


## ----europar.die2.workload.model, echo=FALSE, message=FALSE,warning=FALSE, fig.cap="Model of energy consumption for the workload experiment with taskset on die 2."----
die2_workload_model <- glm( delta_PKG ~ initial_temp*dimension*population_size+delta_seconds*evaluations+I(initial_temp^2),data = europar_taskset_die2_processed)
anova_die2_workload_model <- anova(die2_workload_model)

