load("data/europar_test.rds")
load("data/europar_taskset.rds")
load("data/europar_taskset_die2.rds")

regular <- rbind(europar_test, europar_taskset, europar_taskset_die2)

regular_base <- regular[ startsWith(regular$work, "base"),]
regular_base$work <- "regular"

load("data/icsm_hot_first.rds")
hot_first_base <- icsm_hot_first[ startsWith(icsm_hot_first$work,"base"), ]
hot_first_base$work <- "hot-first"

regular_vs_hot_first <- rbind(regular_base,hot_first_base)

library(ggplot2)

ggplot(regular_vs_hot_first, aes(x=seconds, y=PKG, group=work, color=work))+
  geom_point()+theme_minimal()

regular_vs_hot_first$dimension <- as.factor(regular_vs_hot_first$dimension)
regular_vs_hot_first$population_size <- as.factor(regular_vs_hot_first$population_size)
regular_vs_hot_first_time_model <- glm( seconds ~ work*dimension*population_size, data=regular_vs_hot_first)

regular_vs_hot_first_temp1_model <- glm( initial_temp_1 ~ work*dimension*population_size, data=regular_vs_hot_first)
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
