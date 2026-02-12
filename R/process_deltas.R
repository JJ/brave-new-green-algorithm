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
