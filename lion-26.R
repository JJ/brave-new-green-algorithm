## ----evoapps.baseline.results, echo=FALSE, warnings=FALSE, message=FALSE, fig.height=2.5, fig.pos="h!tb", fig.cap="PKG energy (in Joules) as a function of time for baseline experiments; shapes are related to P and color to D"----
library(dplyr)
# read RDS file data/energy-full-data.rds
readRDS("data/energy-full-data.rds") -> baseline_results
baseline_results$work <- "BNA-baseline"

baseline_results %>% group_by(dimension,population_size) %>%
  summarise(median_energy=median(PKG), sd_energy=sd(PKG),
            trimmed_mean_energy=mean(PKG,trim=0.2),
            median_time=median(seconds), sd_time=sd(seconds),
            trimmed_mean_time=mean(seconds, trim=0.2)) -> summary_baseline_results

# plot energy vs. evaluations with linear model walcom_lm
library(ggplot2)
baseline_results$shape <- ifelse(baseline_results$population_size==200,21,24)
baseline_results$color <- ifelse(baseline_results$dimension==3,"red","blue")
ggplot(baseline_results, aes(x=seconds,y=PKG)) + geom_point( shape=baseline_results$shape, color=baseline_results$color) + geom_smooth(method="lm", formula=y~x, se=FALSE)
ggsave("preso/img/ola-fig-1.png", height=4.5, width=8)

## ----evoapps.baseline.violin, echo=FALSE, warnings=FALSE, message=FALSE, fig.height=2.5, fig.pos="h!tb", fig.cap="Violin plot for the distribution of energy spent for every parameter value, initial configuration."----
ggplot(baseline_results, aes(x=factor(dimension),fill=factor(population_size), y=PKG)) + geom_violin() + theme_minimal() + labs(x="Dimension", y="Energy (J)") + scale_y_log10()
ggsave("preso/img/ola-fig-2.png", height=4.5, width=8)


## ----lion.results, echo=FALSE, warnings=FALSE, message=FALSE, fig.height=2.5, fig.pos="h!tb", out.width="50%", fig.show="hold",fig.cap="Energy spent by the workload as a function of time (left) and difference to target fitness vs. energy (right); point size is related to the max number of generations without change; fill color is blue for dimension =5, red for dimension = 3; finally, shapes are related to population size."----
library(dplyr)
evoapps_results <- read.csv("data/sphere-evoapps-1.11.7-full-bna-16-Oct-17-32-01.csv")

evoapps_results$delta_PKG <- 0
evoapps_results$delta_seconds <- 0

for (dim in c(3,5)) {
  for ( pop_size in c(200,400)) {
    number_of_rows <- nrow(evoapps_results[ evoapps_results$dimension==dim & evoapps_results$population_size==pop_size,])
    evoapps_results[ evoapps_results$dimension==dim & evoapps_results$population_size==pop_size,]$delta_PKG <-
      evoapps_results[ evoapps_results$dimension==dim & evoapps_results$population_size==pop_size,]$PKG  -
      rep(summary_baseline_results[ summary_baseline_results$population_size == pop_size & summary_baseline_results$dimension==dim, ]$median_energy,number_of_rows)

    evoapps_results[ evoapps_results$dimension==dim & evoapps_results$population_size==pop_size,]$delta_seconds <-
      evoapps_results[ evoapps_results$dimension==dim & evoapps_results$population_size==pop_size,]$seconds  -
      rep(summary_baseline_results[ summary_baseline_results$population_size == pop_size & summary_baseline_results$dimension==dim, ]$median_time,number_of_rows)
  }
}

evoapps_results %>% group_by(dimension,population_size,max_gens,alpha) %>%
  summarise(mean_energy=mean(delta_PKG), sd_energy=sd(delta_PKG),
            mean_time=mean(delta_seconds,trim = 0.1), sd_time=sd(delta_seconds)) -> summary_evoapps_results


evoapps_results$population_size <- as.factor(evoapps_results$population_size)
evoapps_results$point_size <- ifelse(evoapps_results$max_gens==10,2,4)
evoapps_results$shape <- ifelse(evoapps_results$population_size==200,21,24)
evoapps_results$color <- ifelse(evoapps_results$dimension==3,"red","blue")
ggplot(evoapps_results, aes(x=delta_PKG,y=delta_seconds)) + geom_point(fill=evoapps_results$color,size=evoapps_results$point_size,shape=evoapps_results$shape) + geom_smooth(method="lm", formula=y~x, se=FALSE)+theme(legend.position="none") + labs(x="Energy (J)", y="Time (s)")
ggsave("preso/img/ola-fig-3.png", height=4.5, width=8)


# Formerly separated
evoapps_results$alpha <- as.factor(evoapps_results$alpha)
evoapps_results$dim_pop <- paste0("D:",evoapps_results$dimension," Pop:",evoapps_results$population_size)
evoapps_results$log_diff <- log10(evoapps_results$diff_fitness)
evoapps_results$dimension <- as.factor(evoapps_results$dimension)
ggplot(evoapps_results, aes(x=delta_PKG,y=log_diff)) + geom_point(,fill=evoapps_results$color,size=evoapps_results$point_size,shape=evoapps_results$shape) + theme(legend.position="none") + labs(x="Energy (J)", y="log(diff. target)")
ggsave("preso/img/ola-fig-4.png", height=4.5, width=8)


## ----evoapps.perf.boost, echo=FALSE, warnings=FALSE, message=FALSE, fig.height=2.5, fig.pos="h!tb", fig.cap="Energy vs. evaluation for the initial (black-filled) and optimized configuration (white-filled). Rest of the legend as above."----
evoapps_perf_results <- read.csv("data/sphere-evoapps-fix-bna-13-Oct-18-54-04.csv")
evoapps_perf_results$work <- "BNA-mutation-perf-boost"
evoapps_perf_results$delta_PKG <- 0
evoapps_perf_results$delta_seconds <- 0

for (dim in c(3,5)) {
  for ( pop_size in c(200,400)) {
    number_of_rows <- nrow(evoapps_perf_results[ evoapps_perf_results$dimension==dim & evoapps_perf_results$population_size==pop_size,])
    evoapps_perf_results[ evoapps_perf_results$dimension==dim & evoapps_perf_results$population_size==pop_size,]$delta_PKG <-
      evoapps_perf_results[ evoapps_perf_results$dimension==dim & evoapps_perf_results$population_size==pop_size,]$PKG  -
      rep(summary_baseline_results[ summary_baseline_results$population_size == pop_size & summary_baseline_results$dimension==dim, ]$median_energy,number_of_rows)

    evoapps_perf_results[ evoapps_perf_results$dimension==dim & evoapps_perf_results$population_size==pop_size,]$delta_seconds <-
      evoapps_perf_results[ evoapps_perf_results$dimension==dim & evoapps_perf_results$population_size==pop_size,]$seconds  -
      rep(summary_baseline_results[ summary_baseline_results$population_size == pop_size & summary_baseline_results$dimension==dim, ]$median_time,number_of_rows)
  }
}
evoapps_perf_results$population_size <- as.factor(evoapps_perf_results$population_size)
evoapps_perf_results$point_size <- ifelse(evoapps_perf_results$max_gens==10,2,4)
evoapps_perf_results$shape <- ifelse(evoapps_perf_results$population_size==200,21,24)
evoapps_perf_results$color <- ifelse(evoapps_perf_results$dimension==3,"red","blue")
evoapps_perf_results$log_diff <- log10(evoapps_perf_results$diff_fitness)
evoapps_perf_results$dim_pop <- paste0("D:",evoapps_perf_results$dimension," Pop:",evoapps_perf_results$population_size)

all_results <- rbind(evoapps_perf_results, evoapps_results)

all_results$fill <- ifelse(all_results$work=="bna","white","black")
ggplot(all_results, aes(x=evaluations,y=delta_PKG)) + geom_point(shape=all_results$shape,alpha=0.5,fill=all_results$fill,size=all_results$point_size,color=all_results$color) + theme_minimal() + labs(x="Evaluations", y="Energy (J)")+scale_x_log10()
ggsave("preso/img/ola-fig-5.png", height=4.5, width=8)

results_anova_pkg <- aov( lm ( delta_PKG ~ work + dimension + population_size + max_gens + alpha, data=all_results ) )
results_anova_result_3 <- aov( lm ( log_diff ~ population_size + max_gens + alpha, data=all_results[ all_results$dimension == 3,] ) )
results_anova_result_5 <- aov( lm ( log_diff ~ population_size + max_gens + alpha, data=all_results[ all_results$dimension == 5,] ) )
results_anova_boost <- aov( lm ( PKG ~ dimension + population_size + max_gens + alpha + work, data=all_results ) )


## ----evoapps.compare, echo=FALSE, warnings=FALSE, message=F, fig.height=2.5, fig.pos="h!tb", fig.cap="Comparing energy consumption for initial configuration vs. optimized one."----
all_results$joules_per_evaluation <- all_results$delta_PKG / all_results$evaluations
ggplot(all_results, aes(x=dim_pop,y=joules_per_evaluation, color=work)) + geom_boxplot(notch=T) + theme_minimal() + labs(y="Energy (J)")
ggsave("preso/img/ola-fig-6.png", height=4.5, width=8)


## ----evoapps.fitness.vs.energy.boost, echo=FALSE, warnings=FALSE, message=FALSE, fig.height=2.5, fig.pos="h!tb", fig.cap="Energy vs fitness difference to the optimum for initial and optimized algorithm. Lower is better for fitness values."----
ggplot(all_results, aes(x=delta_PKG,y=log_diff)) + geom_point(fill=all_results$fill,alpha=0.5,size=all_results$point_size,shape=all_results$shape) + theme(legend.position="none") + labs(x="Energy (J)", y="log(diff. target)")
ggsave("preso/img/ola-fig-7.png", height=4.5, width=8)


## ----evoapps.uniform.mutation, echo=FALSE, warnings=FALSE, message=FALSE, fig.height=2, fig.pos="h!tb", fig.cap="Energy vs fitness difference to the optimum for all experiments for all parameter combinations; we compare here the optimized configuration vs. uniform random mutation."----
evoapps_uniform_results <- read.csv("data/evoapps-1.11.7-fix-rand-bna-fix-rand-25-Oct-11-06-07.csv")
evoapps_uniform_results$delta_PKG <- 0
evoapps_uniform_results$delta_seconds <- 0

for (dim in c(3,5)) {
  for ( pop_size in c(200,400)) {
    number_of_rows <- nrow(evoapps_uniform_results[ evoapps_uniform_results$dimension==dim & evoapps_uniform_results$population_size==pop_size,])
    evoapps_uniform_results[ evoapps_uniform_results$dimension==dim & evoapps_uniform_results$population_size==pop_size,]$delta_PKG <-
      evoapps_uniform_results[ evoapps_uniform_results$dimension==dim & evoapps_uniform_results$population_size==pop_size,]$PKG  -
      rep(summary_baseline_results[ summary_baseline_results$population_size == pop_size & summary_baseline_results$dimension==dim, ]$median_energy,number_of_rows)

    evoapps_uniform_results[ evoapps_uniform_results$dimension==dim & evoapps_uniform_results$population_size==pop_size,]$delta_seconds <-
      evoapps_uniform_results[ evoapps_uniform_results$dimension==dim & evoapps_uniform_results$population_size==pop_size,]$seconds  -
      rep(summary_baseline_results[ summary_baseline_results$population_size == pop_size & summary_baseline_results$dimension==dim, ]$median_time,number_of_rows)
  }
}
evoapps_uniform_results$population_size <- as.factor(evoapps_uniform_results$population_size)
evoapps_uniform_results$point_size <- ifelse(evoapps_uniform_results$max_gens==10,2,4)
evoapps_uniform_results$shape <- ifelse(evoapps_uniform_results$population_size==200,21,24)
evoapps_uniform_results$color <- ifelse(evoapps_uniform_results$dimension==3,"red","blue")
evoapps_uniform_results$log_diff <- log10(evoapps_uniform_results$diff_fitness)
evoapps_uniform_results$dim_pop <- paste0("D:",evoapps_uniform_results$dimension," Pop:",evoapps_uniform_results$population_size)

new_results <- rbind(evoapps_perf_results, evoapps_uniform_results)

new_results$fill <- ifelse(new_results$work=="bna-fix-rand","black","white")
# ggplot(new_results, aes(x=delta_seconds,y=delta_PKG)) +
#   geom_point(shape=all_results$shape,alpha=0.5,fill=all_results$fill,size=all_results$point_size,color=all_results$color) + theme_minimal() + labs(x="Time (s)", y="Energy (J)")
ggplot(new_results, aes(x=delta_PKG,y=log_diff,color=work)) +
  geom_point(alpha=0.5) + labs(y="Log(diff to target)", x="energy") + theme_minimal() + facet_wrap(~dim_pop)
ggsave("preso/img/ola-fig-8.png", height=4.5, width=8)


## ----evoapps.energy.compare, echo=FALSE, warnings=FALSE, message=FALSE, fig.show='hold', fig.height=4, out.width='50%', fig.pos="h!tb", fig.cap="Comparing energy consumption (left) and fitness levels (right) for previous mutation vs. uniform random one."----
ggplot(new_results, aes(x=dim_pop,y=delta_PKG, color=work)) + geom_violin() + theme_minimal() + labs(y="Energy (J)",x='') + theme(legend.position="none")
ggsave("preso/img/ola-fig-9.png", height=4.5, width=8)

ggplot(new_results, aes(x=dim_pop,y=log_diff, color=work)) + geom_boxplot(notch=T) + theme_minimal() + labs(y="Logarithm of difference to target")
ggsave("preso/img/ola-fig-10.png", height=4.5, width=8)


## ----evoapps.parameters.comparison, echo=FALSE, warnings=FALSE, message=FALSE , fig.show='hold', fig.pos="h!tb", fig.height=2.5, out.width="50%", fig.cap="Comparing energy consumption and fitness reached for the different values of $\\protect\\alpha$ and the maximum number of generations without improving the value."----
ggplot(evoapps_uniform_results, aes(color=factor(alpha),y=delta_PKG,x=dim_pop)) + geom_boxplot(notch=T) + theme_minimal() + labs(y="Energy (J)", x="Alpha value") + theme(legend.position="none")
ggsave("preso/img/ola-fig-11.png", height=4.5, width=8)

ggplot(evoapps_uniform_results, aes(color=factor(alpha),y=log_diff, x=dim_pop)) + geom_boxplot(notch=T) + theme_minimal() + labs(y="Log(diff to target)", x="Alpha value")
ggsave("preso/img/ola-fig-12.png", height=4.5, width=8)

ggplot(evoapps_uniform_results, aes(color=factor(max_gens),y=delta_PKG,x=dim_pop)) + geom_boxplot(notch=T) + theme_minimal() + labs(y="Energy (J)", x="Max_gens") + theme(legend.position="none")
ggsave("preso/img/ola-fig-13.png", height=4.5, width=8)

ggplot(evoapps_uniform_results, aes(color=factor(max_gens),y=log_diff,x=dim_pop)) + geom_boxplot(notch=T) + theme_minimal() + labs(y="Log(diff to target)", x="Max_gens")
ggsave("preso/img/ola-fig-14.png", height=4.5, width=8)

# Non-printed ANOVA analysis
anova_uniform_pkg <- aov( lm ( delta_PKG ~ dimension * population_size * max_gens * alpha, data=evoapps_uniform_results ) )
anova_uniform_fitness <- aov( lm ( log_diff ~ dimension * population_size * max_gens * alpha, data=evoapps_uniform_results ) )


## ----lion.julia.1.11.8, echo=FALSE, warnings=FALSE, message=FALSE, fig.height=2.5, fig.pos="h!tb", fig.cap="Boxplot of energy consumption (left) and fitness (right) using Julia 1.11.8, varying maximum number of generations to stop.", fig.show='hold', out.width="50%"----
lion_baseline <- read.csv("data/lion-1.11.8-baseline.csv")
lion_baseline %>% group_by(dimension,population_size) %>%
  summarise(median_energy=median(PKG), sd_energy=sd(PKG),
            trimmed_mean_energy=mean(PKG,trim=0.2),
            median_time=median(seconds), sd_time=sd(seconds),
            trimmed_mean_time=mean(seconds, trim=0.2)) -> summary_lion_baseline

lion_results <- read.csv("data/lion-1.11.8-bna-fix-rand.csv")
lion_results$delta_PKG <- 0
lion_results$delta_seconds <- 0
for (dim in c(3,5)) {
  for ( pop_size in c(200,400)) {
    number_of_rows <- nrow(lion_results[ lion_results$dimension==dim & lion_results$population_size==pop_size,])
    lion_results[ lion_results$dimension==dim & lion_results$population_size==pop_size,]$delta_PKG <-
      lion_results[ lion_results$dimension==dim & lion_results$population_size==pop_size,]$PKG  -
      rep(summary_lion_baseline[ summary_lion_baseline$population_size == pop_size & summary_lion_baseline$dimension==dim, ]$median_energy,number_of_rows)

    lion_results[ lion_results$dimension==dim & lion_results$population_size==pop_size,]$delta_seconds <-
      lion_results[ lion_results$dimension==dim & lion_results$population_size==pop_size,]$seconds  -
      rep(summary_lion_baseline[ summary_lion_baseline$population_size == pop_size & summary_lion_baseline$dimension==dim, ]$median_time,number_of_rows)
  }
}
lion_results$population_size <- as.factor(lion_results$population_size)
lion_results$point_size <- ifelse(lion_results$max_gens==10,2,4)
lion_results$log_diff <- log10(lion_results$diff_fitness)
lion_results$dim_pop <- paste0("D:",lion_results$dimension," Pop:",lion_results$population_size)
lion_results$alpha <- as.factor(lion_results$alpha)

lion_results$max_gens <- as.factor(lion_results$max_gens)
ggplot(lion_results, aes(color=max_gens,y=delta_PKG,x=dim_pop)) + geom_boxplot(notch=T) + theme_minimal() + labs(y="Energy (J)", x="Max_gens") + theme(legend.position="none")
ggsave("preso/img/ola-fig-15.png", height=4.5, width=8)

ggplot(lion_results, aes(color=max_gens,y=log_diff,x=dim_pop)) + geom_boxplot(notch=T) + theme_minimal() + labs(y="Log(diff to target)", x="Max_gens")
ggsave("preso/img/ola-fig-16.png", height=4.5, width=8)

