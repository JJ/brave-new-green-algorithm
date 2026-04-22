## ----ola.base.frequency, echo=FALSE, warning=F, message=F-------------------------------------------
null_baseline_columns <- c("alpha", "max_gens", "different_seeds", "diff_fitness", "generations", "evaluations")
baseline_data_evoapps <- read.csv("data/evoapps-1.11.7-baseline-bna-baseline-16-Oct-11-08-20.csv")
baseline_data_evoapps[ null_baseline_columns ] <- NULL

baseline_data_ola_100s <- read.csv( "data/ola-base-ola-baseline-14-Dec-12-06-42.csv")
baseline_data_ola_100s$work <- "ola-baseline"
baseline_data_ola_100s[null_baseline_columns] <- NULL

initial_baseline_data <- rbind(baseline_data_evoapps, baseline_data_ola_100s)
initial_baseline_data$power <- initial_baseline_data$PKG / initial_baseline_data$seconds
library(ggplot2)
#ggplot(initial_baseline_data,aes(x=PKG,y=seconds,color=work))+geom_point() + theme_minimal() +
  # xlab("Package energy (Joules)") +
  # ylab("Time (seconds)")

library(dplyr)
initial_baseline_data %>% group_by(population_size, dimension, work) %>% summarise(
    mean_pkg = mean(PKG),
    sd_pkg = sd(PKG),
    mean_time = mean(seconds),
    sd_time = sd(seconds),
    mean_power = mean(power),
    sd_power = sd(power)
  ) -> initial_baseline_data_summary

# compute wilcoxon tests between work values for every population_size and dimension
wilcoxon_tests <- function( data, column_name = "PKG" ) {
  wilcoxon_results <- data.frame()
  for (pop_size in unique(data$population_size)) {
    for (dim in unique(data$dimension)) {
      data_subset <- data %>% filter(population_size == pop_size, dimension == dim)
      work_values <- unique(data_subset$work)
      if (length(work_values) == 2) {
        work1_data <- data_subset %>% filter(work == work_values[1]) %>% pull(.data[[column_name]])
        work2_data <- data_subset %>% filter(work == work_values[2]) %>% pull(.data[[column_name]])
        wilcox_test <- wilcox.test(work1_data, work2_data)
        wilcoxon_results <- rbind(wilcoxon_results, data.frame(
          population_size = pop_size,
          dimension = dim,
          work1 = work_values[1],
          work2 = work_values[2],
          p_value = wilcox_test$p.value
        ))
      }
    }
  }
return(wilcoxon_results)
}

wilcoxon_initial_baseline <- wilcoxon_tests( initial_baseline_data )

initial_baseline_data_summary$sd_and_mean_pkg <- paste0("$",
  round(initial_baseline_data_summary$mean_pkg, 2), " \\pm", round(initial_baseline_data_summary$sd_pkg, 2), "$"
)

library(reshape2)
initial_baseline_table <- reshape2::dcast(
  initial_baseline_data_summary,
  population_size + dimension ~ work,
  value.var = "sd_and_mean_pkg"
)

# add a column with the p-values from the wilcoxon tests in wilcoxon_initial_baseline for every population_size and dimension

initial_baseline_table <- merge(
  initial_baseline_table,
  wilcoxon_initial_baseline %>%
    select(population_size, dimension, p_value),
  by = c("population_size", "dimension")
)

initial_baseline_table %>% mutate( p_value = ifelse( as.numeric(p_value) < 0.05, "+", "-" ) ) -> initial_baseline_table

library(kableExtra)

kable(initial_baseline_table, "latex",
      col.names = c("Population size","Dimension","Baseline PKG (J)","Sampling = 100s", "Significant"),
      caption = "Baseline energy consumption for different sampling frequencies",
      escape=FALSE) %>%
  kable_styling( full_width = F)



## ----ola.deltas, echo=FALSE, warning=F, message=F,fig.cap="Workload energy spent vs. time for the initial configuration vs. measurements with sampling period = 100 ms", fig.height=3----
compute_deltas <- function( baseline_summary, workload ) {
  workload$delta_PKG <-0
  workload$delta_seconds <- 0
  for (dim in c(3,5)) {
    for ( pop_size in c(200,400)) {
      number_of_rows <- nrow(workload[ workload$dimension==dim & workload$population_size==pop_size,])
      workload[ workload$dimension==dim & workload$population_size==pop_size,]$delta_PKG <-
      workload[ workload$dimension==dim & workload$population_size==pop_size,]$PKG  -
      rep(baseline_summary[ baseline_summary$population_size == pop_size & baseline_summary$dimension==dim, ]$median_energy,number_of_rows)

      workload[ workload$dimension==dim & workload$population_size==pop_size,]$delta_seconds <-
      workload[ workload$dimension==dim & workload$population_size==pop_size,]$seconds  -
      rep(baseline_summary[ baseline_summary$population_size == pop_size & baseline_summary$dimension==dim, ]$median_time,number_of_rows)
    }
  }
  return (workload)
}

baseline_data_evoapps %>% group_by(dimension,population_size) %>%
  summarise(median_energy=median(PKG), sd_energy=sd(PKG),
            trimmed_mean_energy=mean(PKG,trim=0.2),
            median_time=median(seconds), sd_time=sd(seconds),
            trimmed_mean_time=mean(seconds, trim=0.2)) -> summary_baseline_data_evoapps

workload_data_evoapps <- read.csv("data/evoapps-1.11.7-fix-rand-bna-fix-rand-25-Oct-11-06-07.csv")
workload_data_evoapps <- compute_deltas( summary_baseline_data_evoapps, workload_data_evoapps )

workload_data_evoapps %>% group_by(dimension,population_size,max_gens,alpha) %>%
  summarise(median_delta_energy=median(delta_PKG), sd_delta_energy=sd(delta_PKG),
            trimmed_mean_delta_energy=mean(delta_PKG,trim=0.2),
            median_delta_time=median(delta_seconds), sd_delta_time=sd(delta_seconds),
            trimmed_mean_delta_time=mean(delta_seconds, trim=0.2)) -> summary_workload_data_evoapps

baseline_data_ola_100s %>% group_by(dimension,population_size) %>%
  summarise(median_energy=median(PKG), sd_energy=sd(PKG),
            trimmed_mean_energy=mean(PKG,trim=0.2),
            median_time=median(seconds), sd_time=sd(seconds),
            trimmed_mean_time=mean(seconds, trim=0.2)) -> summary_baseline_data_ola

workload_data_ola <- read.csv("data/ola-1.11.7-ola-14-Dec-13-02-30.csv")
workload_data_ola <- compute_deltas( summary_baseline_data_ola, workload_data_ola )

workload_data <- rbind( workload_data_evoapps, workload_data_ola )

ggplot(workload_data, aes(x=delta_seconds, y=delta_PKG, color=as.factor(work))) +
  geom_point() +
  theme_minimal() +
  xlab("Delta Time (seconds)") +
  ylab("Delta Package energy (Joules)") +
  labs(color="Measurement frequency") + guides(color=guide_legend(title="Experiment"))


## ----ola.micro, echo=FALSE, warning=F, message=F----------------------------------------------------
baseline_ola_v2_data <- read.csv("data/ola-1.11.7-v2-baseline-v2-14-Dec-20-40-47.csv")
baseline_ola_v2_data[ null_baseline_columns ] <- NULL
initial_vs_v2_baseline <- rbind(
  baseline_data_ola_100s %>% mutate( work = "ola-baseline-v1"),
  baseline_ola_v2_data %>% mutate( work = "ola-baseline-v2")
)

wilcoxon_micro_optimization <- wilcoxon_tests( initial_vs_v2_baseline )
initial_vs_v2_baseline %>% group_by(population_size, dimension, work) %>% summarise(
    mean_pkg = mean(PKG),
    sd_pkg = sd(PKG),
    mean_time = mean(seconds),
    sd_time = sd(seconds)
  ) -> initial_vs_v2_baseline_summary

initial_vs_v2_baseline_summary$sd_and_mean_pkg <- paste0("$",
  round(initial_vs_v2_baseline_summary$mean_pkg, 2), " \\pm", round(initial_vs_v2_baseline_summary$sd_pkg, 2), "$"
)

library(reshape2)
initial_vs_v2_baseline_table <- reshape2::dcast(
  initial_vs_v2_baseline_summary,
  population_size + dimension ~ work,
  value.var = "sd_and_mean_pkg"
)

initial_vs_v2_baseline_table <- merge(
  initial_vs_v2_baseline_table,
  wilcoxon_micro_optimization %>%
    select(population_size, dimension, p_value),
  by = c("population_size", "dimension")
)
initial_vs_v2_baseline_table %>% mutate( p_value = ifelse( as.numeric(p_value) < 0.05, "+", "-" ) ) -> initial_vs_v2_baseline_table

kable(initial_vs_v2_baseline_table, "latex",
      col.names = c("Population size","Dimension","Initial version","Micro-optimized version","Significant"),
      caption = "Baseline energy consumption for different implementations of the genetic operators",
      escape=FALSE) %>%
  kable_styling( full_width = F)


## ----ola.micro.plot, echo=FALSE, warning=F, message=F,fig.cap="Workload energy spent vs. time for different implementations of the genetic operators",fig.height=4, fig.pos="h!tb"----
workload_ola_v2_data <- read.csv("data/ola-1.11.7-v2-ola-v2-15-Dec-13-18-43.csv")

baseline_ola_v2_data %>% group_by(population_size, dimension) %>% summarise(
    mean_pkg = mean(PKG),
    sd_pkg = sd(PKG),
    mean_time = mean(seconds),
    sd_time = sd(seconds),
    median_energy = median(PKG),
    median_time = median(seconds),
    trimmed_mean_pkg = mean(PKG, trim=0.2)
  ) -> summary_baseline_ola_v2

workload_ola_v2_data <- compute_deltas( summary_baseline_ola_v2, workload_ola_v2_data )

initial_vs_v2_workload <- rbind(
  workload_data_ola %>% mutate( work = "ola-workload-v1"),
  workload_ola_v2_data %>% mutate( work = "ola-workload-v2")
)

initial_vs_v2_workload$population_dimension <- paste0(
  "Pop. size: ", initial_vs_v2_workload$population_size,
  ", Dim.: ", initial_vs_v2_workload$dimension
)

ggplot(initial_vs_v2_workload, aes(x=work, y=delta_PKG, color=as.factor(work))) +
  geom_violin() +
  theme_minimal() +
  xlab("Version") +
  ylab("Delta Package energy (Joules)") +
  labs(color="Implementation version") +
  facet_wrap( ~ population_dimension )



## ----ola.micro.hysteresis, echo=FALSE, warning=F, message=F,fig.cap="PKG energy data vs. accumulated time for the two baseline runs we are examining", fig.pos="h!tb", fig.height=3----
baseline_data_ola_100s$cumulative_time <- cumsum(baseline_data_ola_100s$seconds)
baseline_data_ola_100s$work <- "Baseline v1"
workload_data_ola$cumulative_time <- cumsum(workload_data_ola$seconds)
workload_data_ola$work <- "Workload v1"
baseline_ola_v2_data$cumulative_time <- cumsum(baseline_ola_v2_data$seconds)
baseline_ola_v2_data$work <- "Baseline v2"
workload_ola_v2_data$cumulative_time <- cumsum(workload_ola_v2_data$seconds)
workload_ola_v2_data$work <- "Workload v2"

ggplot( rbind(baseline_data_ola_100s, baseline_ola_v2_data), aes(x=cumulative_time, y=PKG, color=work) ) +
  geom_point() +
  theme_minimal() +
  xlab("Cumulative Time (seconds)") +
  ylab("Package energy (Joules)") +
  labs(color="Measurement type")



## ----ola.mixed, echo=FALSE, warning=F, message=F,fig.cap="Workload energy spent vs. time for different implementations of the genetic operators after mitigating hysteresis effects",fig.height=3, fig.pos="h!tb"----
ola_mixed_v11_7 <- read.csv("data/ola-1.11.7-mixed-ola-mixed-15-Dec-19-49-11.csv")
ola_mixed_v11_7$cumulative_time <- cumsum(ola_mixed_v11_7$seconds)

for (i in 2:nrow(ola_mixed_v11_7)) {
  if (ola_mixed_v11_7$work[i] == "ola-mixed") {
    ola_mixed_v11_7$delta_seconds[i] <- ola_mixed_v11_7$seconds[i] - ola_mixed_v11_7$seconds[i-1]
    ola_mixed_v11_7$delta_PKG[i] <- ola_mixed_v11_7$PKG[i] - ola_mixed_v11_7$PKG[i-1]
  }
}

ggplot( ola_mixed_v11_7, aes(x=cumulative_time, y=PKG, color=as.factor(work)), group=as.factor(work) ) +
  geom_point() + geom_smooth() +
  geom_bar(ola_mixed_v11_7[ ola_mixed_v11_7$work == "ola-mixed", ], mapping=aes(x=cumulative_time, y=delta_PKG), stat="identity", alpha=0.2, color="blue" ) +
  theme_minimal() +
  xlab("Cumulative Time (seconds)") +
  ylab("Package energy (Joules)") + labs(color="Experiment")


## ----ola.mixed.comparison, echo=FALSE, warning=F, message=F,fig.cap="Workload energy spent compared with different ways of measuring the workload effect; we also compare two values of the maximum number of generations run without improving fitness.",fig.height=3, fig.pos="h!tb", fig.height=4----
ola_mixed_v11_7_summary_data <- ola_mixed_v11_7 %>%
  group_by(work,population_size, dimension, max_gens) %>%
  summarise(
    mean_PKG = mean(PKG),
    median_PKG = median(PKG),
    sd_PKG = sd(PKG),
    trimmed_PKG = mean(PKG, trim = 0.2),
    mean_delta_PKG = mean(delta_PKG, trim=0.2),
    sd_delta_PKG = sd(delta_PKG)
  )

ola_mixed_v11_7_deltas_summary_data <- ola_mixed_v11_7 %>%
  group_by(work,population_size, dimension, max_gens) %>%
  summarise(
    mean_PKG = mean(PKG),
    median_PKG = median(PKG),
    sd_PKG = sd(PKG),
    trimmed_PKG = mean(PKG, trim = 0.2),
    mean_delta_PKG = mean(delta_PKG, trim=0.2),
    sd_delta_PKG = sd(delta_PKG)
  )

workload_ola_v2_data %>% group_by(dimension,population_size,max_gens,alpha) %>%
                         summarise(median_delta_energy=median(delta_PKG), sd_delta_energy=sd(delta_PKG),
                                   trimmed_mean_delta_energy=mean(delta_PKG,trim=0.2),
                          median_delta_time=median(delta_seconds), sd_delta_time=sd(delta_seconds),
                          trimmed_mean_delta_time=mean(delta_seconds, trim=0.2)) -> summary_workload_ola_v2_data

ola_mixed_v11_7[ ola_mixed_v11_7$work == "ola-mixed", ] %>%
  group_by(population_size, dimension, max_gens) %>%
  summarise(
    median_delta_energy = median(delta_PKG),
    sd_delta_energy = sd(delta_PKG),
    trimmed_mean_delta_PKG = mean(delta_PKG, trim=0.2),
    median_delta_time=median(delta_seconds),
    sd_delta_time=sd(delta_seconds),
    trimmed_mean_delta_time=mean(delta_seconds, trim=0.2)
  ) -> summary_ola_mixed_deltas

mixed_vs_separated_workload <- rbind( workload_ola_v2_data, ola_mixed_v11_7[ ola_mixed_v11_7$work == "ola-mixed", ] )
mixed_vs_separated_workload$population_dimension <- paste0(
  "Pop. size: ", mixed_vs_separated_workload$population_size,
  ", Dim.: ", mixed_vs_separated_workload$dimension
)

ggplot(mixed_vs_separated_workload, aes(x=population_dimension, y=delta_PKG, color=as.factor(work))) +
  geom_violin(position="dodge") +
  theme_minimal() +
  xlab("Version") +
  ylab("Delta Package energy (Joules)") +
  labs(color="Implementation version") + # x ticks rotated
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap( ~ max_gens )

# ggplot(mixed_vs_separated_workload, aes(x=population_dimension, y=delta_PKG, color=as.factor(max_gens))) +
#   geom_boxplot(position="dodge") +
#   theme_minimal() +
#   xlab("Version") +
#   ylab("Delta Package energy (Joules)") +
#   labs(color="Implementation version") + # x ticks rotated
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_wrap( ~ work )


## ----ola.v11_8, echo=F, warning=F, message=F, fig.cap="Comparing Julia versions. We have clipped the y axis to 0,250", fig.height=4, fig.pos="h!tb"----
ola_mixed_v11_8 <- read.csv("data/ola-1.11.8-mixed-inverted-ola-mixed-inverted-16-Dec-09-02-52.csv")
ola_mixed_v11_8$cumulative_time <- cumsum(ola_mixed_v11_8$seconds)
for (i in 2:nrow(ola_mixed_v11_8)) {
  if (ola_mixed_v11_8$work[i] == "ola-mixed-inverted") {
    ola_mixed_v11_8$delta_seconds[i] <- ola_mixed_v11_8$seconds[i] - ola_mixed_v11_8$seconds[i-1]
    ola_mixed_v11_8$delta_PKG[i] <- ola_mixed_v11_8$PKG[i] - ola_mixed_v11_8$PKG[i-1]
  }
}

ola_mixed_v11_7_workload <- ola_mixed_v11_7[ ola_mixed_v11_7$work == "ola-mixed",]
ola_mixed_v11_7_workload$work <- "v1.11.7"
ola_mixed_v11_8_workload <- ola_mixed_v11_8[ ola_mixed_v11_8$work == "ola-mixed-inverted",]
ola_mixed_v11_8_workload$work <- "v1.11.8"

ola_v11_7_vs_8_data  <- rbind(ola_mixed_v11_7_workload,ola_mixed_v11_8_workload)
ola_v11_7_vs_8_data$population_dimension <- paste0(
  "Pop. size: ", ola_v11_7_vs_8_data$population_size,
  ", Dim.: ", ola_v11_7_vs_8_data$dimension
)

ggplot(ola_v11_7_vs_8_data, aes(x=population_dimension, y=delta_PKG, color=as.factor(work))) +
  geom_boxplot(position="dodge",notch=T) +
  theme_minimal() +
  xlab("Version") +
  ylab("Delta Package energy (Joules)") +
  labs(color="Implementation version") + # x ticks rotated
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,250)+
  facet_wrap(~ max_gens)


## ----ola.no0, echo=F, fig.height=4, fig.cap="Avoiding zero energy runs, comparison of results"------
ola_mixed_no0 <- read.csv("data/ola-1.11.8-ola-no0-16-Dec-17-43-49.csv" )
ola_mixed_no0$cumulative_time <- cumsum(ola_mixed_no0$seconds)
for (i in 2:nrow(ola_mixed_no0)) {
  if (ola_mixed_no0$work[i] == "ola-no0") {
    ola_mixed_no0$delta_seconds[i] <- ola_mixed_no0$seconds[i] - ola_mixed_no0$seconds[i-1]
    ola_mixed_no0$delta_PKG[i] <- ola_mixed_no0$PKG[i] - ola_mixed_no0$PKG[i-1]
  }
}

ola_mixed_v11_8_workload$work <- "zero energy runs"
ola_mixed_no0_workload <- ola_mixed_no0[ ola_mixed_no0$work == "ola-no0",]
ola_mixed_no0_workload$work <- "no 0 energy runs"

ola_0_vs_no0_data  <- rbind(ola_mixed_v11_8_workload,ola_mixed_no0_workload)
ola_0_vs_no0_data$population_dimension <- paste0(
  "Pop. size: ", ola_0_vs_no0_data$population_size,
  ", Dim.: ", ola_0_vs_no0_data$dimension
)

ggplot(ola_0_vs_no0_data, aes(x=population_dimension, y=delta_PKG, color=as.factor(work))) +
  geom_boxplot(position="dodge",notch=T) +
  theme_minimal() +
  xlab("Version") +
  ylab("Delta Package energy (Joules)") +
  labs(color="Implementation version") + # x ticks rotated
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ max_gens)

