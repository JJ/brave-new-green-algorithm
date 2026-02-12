plot_temperature <- function(df) {
  print(ggplot(df, aes(x = cum_seconds)) +
    geom_point(color= "red", aes(y=initial_temp_1) ) +
     geom_point(color= "pink", aes(y=initial_temp_2) ) +
    labs(
      title = "Temperature over time",
      x = "Time (s)",
      y = "temperature "
    ) + theme_minimal())
}
