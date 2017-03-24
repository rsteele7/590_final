#' Triplicate graphing function.
#' @param csv Filepath to and name of a csv file holding the data.
#' @param time_col Column holding time points.
#' @param OD_col1 Column holding first column of optical density data.
#' @param OD_col2 Column holding second column of optical density data.
#' @param OD_col3 Column holding third column of optical density data.
#' @examples
#'
data <- read.csv("sample_data_trip.csv")
tripgraph <- function(csv, OD_col1, OD_col2, OD_col3) {
  browser()
  library(tidyverse)
  library(matrixStats)
  data <- read.csv(csv)
  eval_cols <- c(OD_col1, OD_col2, OD_col3)
  data <- mutate(data, mean = rowMeans(data[, eval_cols]), stdev = rowSds(as.matrix(data[, eval_cols])))
  ggplot(data, aes(x=data$Time, y=mean)) +
    geom_line(aes(y=mean)) +
    geom_point(aes(y=mean)) +
    geom_errorbar(aes(ymin= mean- stdev, ymax = mean + stdev)) +
    scale_y_log10() +
    ylab(expression(paste("Log ", OD[600], sep = ""))) +
    xlab("Time (min)")
}
