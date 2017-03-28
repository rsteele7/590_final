#' Triplicate graphing function.
#'
#' @param csv Filepath to and name of the csv file holding the data.
#' @param OD_col1 Column holding first column of optical density data.
#' @param OD_col2 Column holding second column of optical density data.
#' @param OD_col3 Column holding third column of optical density data.
#' @param nm Wavelength at which optical density was measured; default is 600nm
#' @param time.unit Time unit in which data is collected; default is min
#' @examples
#' tripgraph("sample_data_trip.csv", "OD600_1", "OD600_2", "OD600_3", title = "My growth curve in triplicate")
#' tripgraph("sample_data_trip.csv", "OD600_1", "OD600_2", "OD600_3", nm = 200)

tripgraph <- function(csv, OD_col1, OD_col2, OD_col3, title="Growth Curve", nm=600, time.unit = "min") {
  data <- read.csv(csv)
  eval_cols <- c(OD_col1, OD_col2, OD_col3)
  data <- mutate(data, mean = rowMeans(data[, eval_cols]),
                 stdev = matrixStats::rowSds(as.matrix(data[, eval_cols])))
  ggplot2::ggplot(data, aes(x=data$Time, y=mean)) +
    geom_line(aes(y=mean)) +
    geom_point(aes(y=mean)) +
    geom_errorbar(aes(ymin= mean- stdev, ymax = mean + stdev)) +
    scale_y_log10() +
    ylab(bquote("Log " ~ OD[.(nm)])) +
    xlab(bquote("Time " ~ (.(time.unit)))) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}


#' Generation time calculation function for triplicate data series
#'
#' @param csv Filepath to and name of the csv file holding the data.
#' @param OD_col1 Column holding first column of optical density data.
#' @param OD_col2 Column holding second column of optical density data.
#' @param OD_col3 Column holding third column of optical density data.
#' @param midlog For the strain of interest, the typical midlog value.
#' @return Generation doubling time in the same unit of time as in the data sheet.
#' @examples
#' tripgen("sample_data_trip.csv", "OD600_1", "OD600_2", "OD600_3", 0.3)
#' tripgen("sample_data_trip.csv", "OD600_1", "OD600_2", "OD600_3", 0.3)

tripgen <- function(csv, OD_col1, OD_col2, OD_col3, midlog) {
  data <- read.csv(csv)
  eval_cols <- c(OD_col1, OD_col2, OD_col3)
  data <- mutate(data, mean = rowMeans(data[, eval_cols]))
  log_vals <- data[data[, "mean"] >= midlog, ]
  use_vals <- head(log_vals, 2)
  OD_diff <- log10(use_vals[2, "mean"]) - log10(use_vals[1, "mean"])
  time_diff <- use_vals[2, "Time"] - use_vals[1, "Time"]
  slope <- OD_diff/time_diff
  gen_time <- 0.3/slope
  return(gen_time)
}

