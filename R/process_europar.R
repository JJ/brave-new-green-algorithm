process_europar <- function(file_name, work_name) {
  df <- read.csv(file_name)
  df$initial_temp <- (df$initial_temp_1 + df$initial_temp_2)/2
  df$color <- ifelse(df$work == work_name, "red", "blue")
  df$base <- ifelse(df$work == work_name, FALSE, TRUE)
  df$shape <- ifelse(df$dimension == 10,21,ifelse(df$dimension == 5, 22,23))
  df$size <- ifelse(df$work == work_name, 4,3)
  df$cum_seconds <- cumsum(df$seconds)
  df$initial_temp <- as.numeric(df$initial_temp)

  print(ggplot(df, aes(x = cum_seconds, y = PKG, size= work ) )+
      scale_color_viridis_c() +
        scale_size_manual( values = c(3,5)) +
      geom_point( aes(color=initial_temp, shape=factor(shape)), alpha=0.5 ) +
      labs(
      title = "Energy Consumption Over time",
      x = "Time",
      y = "Energy Consumption "
    ) +
    theme_minimal())
  return(df)
}
