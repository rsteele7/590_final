#' Data graphing function.
#'
#' @param csv Filepath to and name of the csv file holding the data.
#' @param OD_col Column holding optical density data.
#' @param nm Wavelength at which optical density was measured; default is 600nm
#' @return Graph with single growth curve.
#' @examples
#' singraph("sample_data.csv", "OD600")
#' singraph("sample_data.csv", "OD600", 300)

singraph <- function(csv, OD_col, nm=600) {
  data <- read.csv(csv)
  ggplot2::ggplot(data, aes(x=data$Time, y=data$OD_col)) +
    geom_line(aes(y=data[, OD_col])) +
    geom_point(aes(y=data[, OD_col])) +
    scale_y_log10() +
    ylab(bquote("Log " ~ OD[.(nm)])) +
    xlab("Time (min)")
}

#' Generation time calculation function
#'
#' @param csv Filepath to and name of the csv file holding the data.
#' @param OD_col Column holding optical density data.
#' @param midlog For the strain of interest, the typical midlog value.
#' @return Generation doubling time in the same unit of time as in the data sheet.
#' @examples
#' sindoub("sample_data.csv", "OD600", 0.3)
#' sindoub("sample_data.csv", "OD600", 0.4)

singen <- function(csv, OD_col, midlog) {
  data <- read.csv(csv)
  log_vals <- data[data[, OD_col] >= midlog, ]
  use_vals <- head(log_vals, 2)
  OD_diff <- log10(use_vals[2, OD_col]) - log10(use_vals[1, OD_col])
  time_diff <- use_vals[2, "Time"] - use_vals[1, "Time"]
  slope <- OD_diff/time_diff
  gen_time <- 0.3/slope
  return(gen_time)
}
