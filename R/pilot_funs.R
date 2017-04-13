#' Data graphing function.
#'
#' @param csv Filepath to and name of the csv file holding the data.
#' @param OD.col Column holding optical density data.
#' @param time.col Column holding times at which data was collected; default is "Time"
#' @param title The title of the plot; default is "Growth Curve"
#' @param nm Wavelength in nanometers at which optical density was measured; default is 600
#' @param time.unit Time unit in which data is collected; default is "min"
#' @return Graph with single growth curve.
#' @examples
#' singraph("sample_data.csv", time.col = "Time", "OD600")
#' singraph("sample_data.csv", "OD600", nm = 300)

singraph <- function(csv, OD.col, time.col = "Time", title = "Growth Curve", nm = 600, time.unit = "min") {
  data <- read.csv(csv)
  ggplot2::ggplot(data, aes(x=data[, time.col], y=data[, OD.col])) +
    geom_line(aes(y=data[, OD.col])) +
    geom_point(aes(y=data[, OD.col])) +
    scale_y_log10() +
    ylab(bquote("Log " ~ OD[.(nm)])) +
    xlab(bquote("Time " ~ (.(time.unit)))) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

#' Generation time calculation function
#'
#' @param csv Filepath to and name of the csv file holding the data.
#' @param OD.col Column holding optical density data.
#' @param midlog For the strain of interest, the typical midlog value.
#' @param time.col Column holding times at which data was collected; default is "Time"
#' @return Generation doubling time in the same unit of time as in the data sheet.
#' @examples
#' singen("sample_data.csv", "OD600", 0.3, time.col = "Time")
#' singen("sample_data.csv", "OD600", 0.4)

singen <- function(csv, OD.col, midlog, time.col="Time") {
  data <- read.csv(csv)
  log_vals <- data[data[, OD.col] >= midlog, ]
  use_vals <- head(log_vals, 2)
  OD.diff <- log10(use_vals[2, OD.col]) - log10(use_vals[1, OD.col])
  time.diff <- use_vals[2, time.col] - use_vals[1, time.col]
  slope <- OD.diff/time.diff
  gen_time <- 0.3/slope
  return(gen_time)
}
