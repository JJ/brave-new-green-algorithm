## ----cec.baseline, echo=FALSE, message=FALSE, warnings=FALSE,fig.cap="PKG energy measured vs. time for baseline measurements in two sets of experiments.", fig.height=2.5----

# Utility functions
process_and_plot <- function(file_path, group_name) {
  data <- read.csv(file_path)
  data$group <- group_name
  data$cum_seconds <- cumsum(data$seconds)
  print( ggplot(data, aes(x = cum_seconds, y = PKG, group=work, color=work)) +
    geom_line() +
    labs(
      title = paste("Energy Consumption Over Time -", group_name),
      x = "Time",
      y = "Energy Consumption "
    ) +
    theme_minimal())
  return(data)
}

process_deltas <- function(data) {
  n <- nrow(data)
  data$delta_PKG <- rep(NA_real_, n)
  data$delta_seconds <- rep(NA_real_, n)
  for (k in seq(from=0,to=n-1,by=61)) {
    for (i in seq(from=2,to=60,by=2)) {
      index <- k+i
      data$delta_seconds[index] <- data$seconds[index] - (data$seconds[index-1]+ data$seconds[index+1])/2
      data$delta_PKG[index] <- data$PKG[index] - (data$PKG[index-1] + data$PKG[index+1])/2

    }
  }
  return(data %>% filter( delta_PKG != 0))
}

create_summary <- function(data) {
  data$energy_per_evaluation <- data$delta_PKG / data$evaluations
  return(
    data %>%
      group_by(dimension, population_size, max_gens ) %>%
      summarise(
        mean_delta_PKG = mean(delta_PKG),
        median_delta_PKG = median(delta_PKG),
        trimmed_mean_delta_PKG = mean(delta_PKG, trim=0.2),
        trimmed_mean_energy_per_evaluation = mean(energy_per_evaluation, trim=0.2),
        sd_delta_PKG = sd(delta_PKG),
        iqr_delta_PKG = IQR(delta_PKG),
        iqr_PKG = IQR(PKG),
        conf_interval_delta_PKG = sprintf("[%s, %s]", round(t.test(delta_PKG)$conf.int[1], 2), round(t.test(delta_PKG)$conf.int[2], 2))
      )
  )
}


baseline_ola_100s <- read.csv("data/ola-base-ola-baseline-14-Dec-12-06-42.csv")
baseline_ola_100s$cum_seconds <- cumsum(baseline_ola_100s$seconds)
baseline_ola_100s$group <- "1"
baseline_ola_v2_data <- read.csv("data/ola-1.11.7-v2-baseline-v2-14-Dec-20-40-47.csv")
baseline_ola_v2_data$cum_seconds <- cumsum(baseline_ola_v2_data$seconds)
baseline_ola_v2_data$group <- "2"

baseline_both <- rbind(baseline_ola_100s, baseline_ola_v2_data)

library(ggplot2)
ggplot(baseline_both, aes(x = cum_seconds, y = PKG,group=group, color=group)) +
    geom_line() +
    labs(
      title = paste("Energy Consumption Over Time - Baseline Measurement"),
      x = "Time",
      y = "Energy Consumption "
    ) +
    theme_minimal()

library(dplyr)
baseline_ola_100s %>%
      group_by(dimension, population_size ) %>%
      summarise(
        mean_PKG = mean(PKG),
        median_PKG = median(PKG),
        trimmed_mean_PKG = mean(PKG, trim=0.2),
        sd_PKG = sd(PKG),
        iqr_PKG = IQR(PKG),
        conf_interval_PKG = sprintf("[%s, %s]", round(t.test(PKG)$conf.int[1], 2), round(t.test(PKG)$conf.int[2], 2))
   ) -> initial_baseline_ola_100s_summary




## ----cec.workload, echo=FALSE, fig.cap="PKG energy measured vs. time for workload measurements. Dashed lines correspond to baseline averages for that segment; color = population size", fig.height=2.5----
workload_data_ola <- read.csv("data/ola-1.11.7-ola-14-Dec-13-02-30.csv")
workload_data_ola$delta_PKG <-0
workload_data_ola$delta_seconds <- 0
workload_data_ola$cum_seconds <- cumsum(workload_data_ola$seconds)
ola_segments <- data.frame( parameters=character(), start_second = numeric(), end_second=numeric(), trimmed_mean_PKG=numeric(), color=character())
color_index <- 1
for (dim in c(3,5)) {
  for ( pop_size in c(200,400)) {
      number_of_rows <- nrow(workload_data_ola[ workload_data_ola$dimension==dim & workload_data_ola$population_size==pop_size,])
      workload_data_ola[ workload_data_ola$dimension==dim & workload_data_ola$population_size==pop_size,]$delta_PKG <-
      workload_data_ola[ workload_data_ola$dimension==dim & workload_data_ola$population_size==pop_size,]$PKG  -
      rep(initial_baseline_ola_100s_summary[ initial_baseline_ola_100s_summary$population_size == pop_size & initial_baseline_ola_100s_summary$dimension==dim, ]$trimmed_mean_PKG,number_of_rows)

      ola_segments <- rbind(ola_segments,
        data.frame(
          parameters = sprintf("dim=%d, pop=%d", dim, pop_size),
          start_second = min(workload_data_ola[ workload_data_ola$dimension==dim & workload_data_ola$population_size==pop_size,]$cum_seconds),
          end_second = max(workload_data_ola[ workload_data_ola$dimension==dim & workload_data_ola$population_size==pop_size,]$cum_seconds),
          trimmed_mean_PKG = initial_baseline_ola_100s_summary[ initial_baseline_ola_100s_summary$population_size == pop_size & initial_baseline_ola_100s_summary$dimension==dim, ]$trimmed_mean_PKG,
          color=ifelse(color_index %% 2 == 0, "red", "blue")
        )
      )
      color_index <- color_index + 1

    }
  }

plot_with_segments <- ggplot(workload_data_ola, aes(x = cum_seconds, y = PKG)) +
    geom_line() +
    labs(
      title = paste("Energy Consumption Over Time - Workload Measurement"),
      x = "Time",
      y = "Energy Consumption "
    ) +
    theme_minimal()+
    ylim(300, 750)

for (i in 1:nrow(ola_segments)) {
  plot_with_segments <- plot_with_segments +
    geom_segment( x=ola_segments$start_second[i], y=ola_segments$trimmed_mean_PKG[i],
                     xend=ola_segments$end_second[i], yend=ola_segments$trimmed_mean_PKG[i], linetype="dashed", color=ola_segments$color[i] )
}

print(plot_with_segments)


## ----cec.sequential, echo=FALSE, message=FALSE, warning=FALSE, fig.cap="PKG energy measured vs. time for sequential baseline and workload measurements, shown in different colors.", fig.height=2.5----
ola_mixed_no0 <- read.csv("data/ola-1.11.8-ola-no0-16-Dec-17-43-49.csv" )
ola_mixed_no0$cumulative_time <- cumsum(ola_mixed_no0$seconds)
for (i in 2:nrow(ola_mixed_no0)) {
  if (ola_mixed_no0$work[i] == "ola-no0") {
    ola_mixed_no0$delta_seconds[i] <- ola_mixed_no0$seconds[i] - ola_mixed_no0$seconds[i-1]
    ola_mixed_no0$delta_PKG[i] <- ola_mixed_no0$PKG[i] - ola_mixed_no0$PKG[i-1]
  }
}
ggplot(ola_mixed_no0, aes(x = cumulative_time, y = PKG, group=work, color=work)) +
    geom_line() +
    labs(
      title = paste("Energy Consumption Over Time - Sequential Baseline and Workload Measurement"),
      x = "Time",
      y = "Energy Consumption "
    ) +
    theme_minimal()


## ----cec.sequential.table, echo=FALSE, message=FALSE-------------------------------------------------------------
ola_mixed_no0 %>%
  filter(work == "ola-no0") %>%
  group_by(dimension, population_size, max_gens) %>%
  summarise(
    mean_delta_PKG = mean(delta_PKG, na.rm=TRUE),
    trimmed_mean_delta_PKG = mean(delta_PKG, trim=0.2, na.rm=TRUE),
    sd_delta_PKG = sd(delta_PKG, na.rm=TRUE),
    iqr_delta_PKG = IQR(delta_PKG, na.rm=TRUE),
    iqr_PKG = IQR(PKG, na.rm=TRUE),
    conf_interval_delta_PKG = sprintf("[%s, %s]", round(t.test(delta_PKG)$conf.int[1], 2), round(t.test(delta_PKG)$conf.int[2], 2))
  ) -> sequential_summary

library(kableExtra)
kable(sequential_summary,
      col.names=c("D","P","max gens", "Delta PKG (J): Mean", "Trimmed Mean", "SD", "IQR", "PKG: IQR", "Delta PKG: CI"),
      caption = "Summary statistics for sequential mixed baseline and workload measurements.", table.env='table*')


## ----cec.sandwich, echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------
cec_sandwich_1 <- read.csv("data/cec-1.12.4-25.10-cec-mixed-v1-26-Jan-08-30-26.csv" )
sandwich_processed_1 <- process_deltas(cec_sandwich_1)
summary_sandwich_1 <- create_summary(sandwich_processed_1)
summary_sandwich_1$trimmed_mean_energy_per_evaluation <- NULL
summary_sandwich_1$median_delta_PKG <- NULL
kable(summary_sandwich_1,
      col.names=c("D","P","max gens", "Delta PKG (J): Mean", "Trimmed Mean", "SD", "IQR", "PKG: IQR", "Delta PKG: CI"),
      caption = "Summary statistics for sandwich baseline and workload measurements.", table.env='table*')


## ----cec.reboots, echo=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------
cec_sandwich_2 <- read.csv("data/cec-1.12.4-25.10-cec-mixed-v2-26-Jan-12-20-59.csv" )
sandwich_processed_2 <- process_deltas(cec_sandwich_2)
cec_sandwich_3  <- read.csv("data/cec-1.12.4-25.10-cec-mixed-v3-26-Jan-17-42-27.csv" )
sandwich_processed_3 <- process_deltas(cec_sandwich_3)
cec_sandwich_4 <- read.csv("data/cec-1.12.4-25.10-cec-mixed-v4-27-Jan-08-09-20.csv")
sandwich_processed_4 <- process_deltas(cec_sandwich_4)
cec_sandwich_5 <- read.csv("data/cec-1.12.4-25.10-cec-mixed-v5-27-Jan-10-16-05.csv" )
sandwich_processed_5 <- process_deltas(cec_sandwich_5)

cec_sandwich_all <- rbind(sandwich_processed_1, sandwich_processed_2, sandwich_processed_3, sandwich_processed_4, sandwich_processed_5)
summary_cec_sandwich_all <- create_summary(cec_sandwich_all)

summary_cec_sandwich_all$trimmed_mean_energy_per_evaluation <- NULL
summary_cec_sandwich_all$median_delta_PKG <- NULL
kable(summary_cec_sandwich_all,
      col.names=c("D","P","max gens", "Delta PKG (J):
 Mean", "Trimmed Mean", "SD", "IQR", "PKG: IQR", "Delta PKG: CI"),
      caption = "Summary statistics for multi-run sandwich baseline and workload measurements",
      table.env='table*')

