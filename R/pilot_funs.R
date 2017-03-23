#' Data graphing function.
#' @param csv Filepath to and name of a csv file holding the data.
#' @param time_col Column holding time points.
#' @param OD_col Column holding optical density data.
#' @examples
#'
data <- read.csv("sample_data.csv")
singraph <- function(csv, OD_col) {
  browser()
  library(ggplot2)
  data <- read.csv(csv)
  ggplot(data, aes(x=data$Time, y=data$OD_col)) +
    geom_line(aes(y=data[, OD_col])) +
    geom_point(aes(y=data[, OD_col])) +
    ylab(expression(paste("Log ", OD[600], sep = ""))) +
    scale_y_log10() +
    xlab("Time (min)")
}
