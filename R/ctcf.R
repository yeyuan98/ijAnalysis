# Corrected fluorescence analysis helper functions

#' Read CTCF results table
#'
#' @param path Path to the CTCF analysis csv
#' @param ... Forwarded to `readr::read_csv`
#'
#' @return tibble of CTCF table
#' @export
#'
#' @examples
#' #TODO
ctcf_read_csv <- function(path, ...){
  readr::read_csv(
    path,
    col_names = c(
      "row", "time.point", "measurement", "slice",
      "area", "mean", "integrated", "ctcf"), skip = 1
  )
}

#' Standardized plotting of CTCF data
#'
#' @param data tibble of CTCF data
#' @param x x-axis variable for plotting
#' @param y y-axis variable for plotting
#' @param fill fill variable for plotting
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' # TODO
ctcf_plot <- function(data, x, y, fill){
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  if (rlang::is_missing(fill)){
    fill <- x
  } else {
    fill <- rlang::enquo(fill)
  }

  plot.boxPoint(data, x, y, fill)+
    ggplot2::scale_y_log10(expand = c(0,.01))
}
