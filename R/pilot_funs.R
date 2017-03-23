#' Data graphing function.
#' @param csv Filepath to and name of a csv file holding the data.
#' @param time_col Column holding time points.
#' @param OD_col Column holding optical density data.
#' @examples
#'
data <- read.csv("sample_data.csv")
singraph <- function(csv, time_col, OD_col) {
  library(ggplot2)
  data <- read.csv(csv)
  ggplot(data, aes(x=time_col, y=OD_col)) +
    geom_line() +
    geom_point() +
    ylab(expression(paste("Log ", OD[600], sep = ""))) +
    xlab("Time (min)")
}
