#' Process and visualise EuroPar-format energy data
#'
#' Reads a CSV file in the EuroPar experiment format, adds derived columns
#' (\code{initial_temp}, \code{color}, \code{base}, \code{shape},
#' \code{size}, \code{cum_seconds}), and prints a scatter plot of energy
#' consumption over cumulative time coloured by temperature.
#'
#' @param file_name Character.  Path to the CSV file.  The file must contain
#'   the columns \code{work}, \code{dimension}, \code{PKG}, \code{seconds},
#'   \code{initial_temp_1}, and \code{initial_temp_2}.
#' @param work_name Character.  The value of the \code{work} column that
#'   identifies workload (non-baseline) rows.
#'
#' @return The processed data frame (invisibly, the plot is printed as a side
#'   effect).
#'
#' @examples
#' \dontrun{
#' df <- process_europar("data/europar_experiment.csv", "bna-workload")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal
#'   scale_color_viridis_c scale_size_manual
#' @export
process_europar <- function(file_name, work_name) {
  df <- utils::read.csv(file_name, stringsAsFactors = FALSE)
  df$initial_temp <- (df$initial_temp_1 + df$initial_temp_2) / 2
  df$color  <- ifelse(df$work == work_name, "red", "blue")
  df$base   <- ifelse(df$work == work_name, FALSE, TRUE)
  df$shape  <- ifelse(df$dimension == 10, 21,
               ifelse(df$dimension == 5, 22, 23))
  df$size   <- ifelse(df$work == work_name, 4, 3)
  df$cum_seconds  <- cumsum(df$seconds)
  df$initial_temp <- as.numeric(df$initial_temp)

  print(
    ggplot(df, aes(x = cum_seconds, y = PKG,
                   size = work)) +
      scale_color_viridis_c() +
      scale_size_manual(values = c(3, 5)) +
      geom_point(aes(color = initial_temp,
                     shape = factor(shape)),
                 alpha = 0.5) +
      labs(
        title = "Energy Consumption Over Time",
        x     = "Time",
        y     = "Energy Consumption"
      ) +
      theme_minimal()
  )
  return(df)
}
