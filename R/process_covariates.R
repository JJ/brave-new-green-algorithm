process_covariates <- function(data) {
  n <- nrow(data)
  data$seconds_baseline_prev <- rep(NA_real_, n)
  data$seconds_baseline_post <- rep(NA_real_, n)
  data$PKG_baseline_prev <- rep(NA_real_, n)
  data$PKG_baseline_post <- rep(NA_real_, n)
  for (k in seq(from=0,to=n-1,by=61)) {
    for (i in seq(from=2,to=60,by=2)) {
      index <- k+i
      data$seconds_baseline_prev[index] <- data$seconds[index-1]
      data$seconds_baseline_post[index] <- data$seconds[index+1]
      data$PKG_baseline_prev[index] <- data$PKG[index-1]
      data$PKG_baseline_post[index] <- data$PKG[index+1]

    }
  }
  return(data %>% filter( PKG_baseline_post != 0))
}
