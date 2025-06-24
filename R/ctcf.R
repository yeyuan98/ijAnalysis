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
#' This is de facto the versatile X-Y plot generator of this package. Supports
#' multiple plot templates.
#'
#' All possible templates are not exported functions `plot.` in this package.
#' Supported ones are:
#'
#' * `"linePoint"` - uses `plot.linePoint()` - point plot with summary line
#' * `"boxPoint"` - uses `plot.boxPoint()` - point plot with box
#'
#' @param data tibble of CTCF data
#' @param x x-axis variable for plotting
#' @param y y-axis variable for plotting
#' @param fill fill variable for plotting
#' @param template which template to use, see details.
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' # Will warn about the `am` continuous color variable
#' ctcf_plot(mtcars, cyl, mpg, am, template = "linePoint")
#' # Will create a boxPoint plot. Note that for boxPoint X must be discrete.
#' ctcf_plot(
#' mtcars |> dplyr::mutate(gear = as.character(gear), am = as.character(am)),
#' am, mpg, gear)
ctcf_plot <- function(data, x, y, fill = NA, template = "boxPoint"){
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill <- rlang::enquo(fill)
  if (rlang::as_label(fill) == "NA"){
    fill <- x
  }

  switch(
    template,
    boxPoint = {
      plot.boxPoint(data, x, y, fill)+
        ggplot2::scale_y_log10(expand = c(0,.01))+
        ggplot2::scale_fill_viridis_d(begin = .6, end = 1)
    },
    linePoint = {
      plot.linePoint(data, x, y, fill)
    },
    rlang::abort("Unsupported plot template.")
    )
}
