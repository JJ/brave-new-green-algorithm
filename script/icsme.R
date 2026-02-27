load("data/europar_test.rds")
load("data/europar_taskset.rds")
load("data/europar_taskset_die2.rds")


initial_data <- rbind (europar_test, europar_taskset, europar_taskset_die2)

delta_temperatures <- data.frame( delta_temperature = numeric(),
                                  initial_temperature = numeric(),
                                  PKG = numeric(),
                                  running_time = numeric())

for ( row in 1:nrow(initial_data)) {
  if (startsWith(initial_data$work[row], "base-")) {
    next
  }

  delta_temperatures <- rbind(delta_temperatures, data.frame(
    delta_temperature = initial_data$initial_temp[row] - initial_data$initial_temp[row+1],
    initial_temperature = initial_data$initial_temp[row],
    PKG = initial_data$PKG[row],
    running_time = initial_data$seconds[row]
  ))
}

library(ggplot2)
ggplot(delta_temperatures, aes(x = initial_temperature, y = delta_temperature, color=PKG)) +
  geom_point() +
  labs( title = "Delta Temperature vs Initial Temperature",
    x = "Initial Temperature",
    y = "Delta Temperature"
  ) + theme_minimal()

temperature_model <- glm(delta_temperature ~ initial_temperature*PKG*running_time+I(initial_temperature^2), data = delta_temperatures)

anova_temperature_model <- anova(temperature_model)

PKG_levels <- quantile( delta_temperatures$PKG, probs = c(0.1, 0.5, 0.9) )
new_data <- expand.grid(
  initial_temperature = seq(min(delta_temperatures$initial_temperature), max(delta_temperatures$initial_temperature), length.out = 100),
  PKG = PKG_levels,
  running_time = median(delta_temperatures$running_time)
)

new_data$predicted_delta_temperature <- predict(temperature_model, newdata = new_data)
names(new_data)[names(new_data) == "PKG"] <- "PKG_levels"
new_data$PKG_levels <- factor(new_data$PKG_levels, labels = c("Low PKG", "Medium PKG", "High PKG"))
names(new_data)[names(new_data) == "predicted_delta_temperature"] <- "delta_temperature"

library(scales)
custom_pow <- trans_new(
  name = "custom_pow",
  transform = function(x) x^0.3,  # Lower power = more stretch at the bottom
  inverse = function(x) x^(1/0.3)
)
ggplot(new_data, aes(x = initial_temperature, y = delta_temperature, color=PKG_levels)) +
  geom_line(size=2) +
  scale_fill_viridis_c(trans=custom_pow) +
  geom_point(data = delta_temperatures, aes(x = initial_temperature, y = delta_temperature, fill=PKG ),
             shape=21,inherit.aes=F, alpha=0.5, stroke=NA, size=3) +
  labs( title = "Predicted Delta Temperature vs Initial Temperature",
    x = "Initial Temperature",
    y = "Predicted Delta Temperature",
    color = "PKG Level"
  ) + theme_minimal()
