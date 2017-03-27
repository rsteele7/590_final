#' Triplicate graphing function.
#' @param csv Filepath to and name of the csv file holding the data.
#' @param OD_col1 Column holding first column of optical density data.
#' @param OD_col2 Column holding second column of optical density data.
#' @param OD_col3 Column holding third column of optical density data.
#' @param nm Wavelength at which optical density was measured; default is 600nm
#' @examples
#' tripgraph("sample_data_trip.csv", "OD600_1", "OD600_2", "OD600_3")

tripgraph <- function(csv, OD_col1, OD_col2, OD_col3, nm=600) {
  library(ggplot2)
  library(matrixStats)
  data <- read.csv(csv)
  eval_cols <- c(OD_col1, OD_col2, OD_col3)
  data <- mutate(data, mean = rowMeans(data[, eval_cols]),
                 stdev = rowSds(as.matrix(data[, eval_cols])))
  ggplot(data, aes(x=data$Time, y=mean)) +
    geom_line(aes(y=mean)) +
    geom_point(aes(y=mean)) +
    geom_errorbar(aes(ymin= mean- stdev, ymax = mean + stdev)) +
    scale_y_log10() +
    ylab(bquote("Log " ~ OD[.(nm)])) +
    xlab("Time (min)")
}
##Notes to self;

###Might be better to convert to long format as it might be more useful for plotting multiple series
###To do this, make a column for bacterial strain/condition info and another for replicate info
###The strain/condition info might be useful for figuring out legends also
