load("data/europar_test.rds") # Indistintict die
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
ggplot(europar_poster_dataset, aes(x=initial_temp_1, y=power, color=run))+geom_point()+theme_minimal()

power_model <- glm( power ~ initial_temp_1*initial_temp_2 + run*seconds, data=europar_poster_dataset)
europar_poster_dataset$residual_power <- residuals(power_model)

time_model <- glm( seconds ~ initial_temp_1*initial_temp_2 + run, data=europar_poster_dataset)
