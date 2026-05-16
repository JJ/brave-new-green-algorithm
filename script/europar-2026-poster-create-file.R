load("data/europar_test.rds") # Indistinct die
load("data/europar_taskset.rds") # Only die 1
load("data/europar_taskset_die2.rds") # Only die 2


europar_poster_dataset <- rbind(europar_test,europar_taskset,europar_taskset_die2)
europar_poster_dataset$run <- ifelse( startsWith(europar_poster_dataset$work, "base"), "B", "W")

europar_poster_dataset$alpha <- NULL
europar_poster_dataset$work <- NULL

europar_poster_dataset$power <- europar_poster_dataset$PKG/europar_poster_dataset$seconds
summary(europar_poster_dataset$power)

save(file="data/europar_poster_datset.rds",europar_poster_dataset)

library(ggplot2)
library(ggExtra)
distribution_run <- ggplot(europar_poster_dataset, aes(x=initial_temp_1, y=power, color=run))+geom_point()+ theme_minimal()
ggMarginal(distribution_run, type="density", groupFill=T, alpha=0.5)

library(scales)
custom_pow <- trans_new(
  name = "custom_pow",
  transform = function(x) x^0.3,  # Lower power = more stretch at the bottom
  inverse = function(x) x^(1/0.3)
)
ggplot(europar_poster_dataset, aes(x=initial_temp_1, y=power, color=initial_temp_2))+geom_point()+
  scale_color_viridis_c(trans=custom_pow) + theme_minimal()

time_model <- glm( seconds ~ initial_temp_1*initial_temp_2 + run, data=europar_poster_dataset)
europar_poster_dataset$residual_seconds <- residuals(power_time)

power_model <- glm( power ~ initial_temp_1*initial_temp_2 + run*seconds, data=europar_poster_dataset)
europar_poster_dataset$residual_power <- residuals(power_model)


