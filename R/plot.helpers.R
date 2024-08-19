# Plotting helpers - convinience helpers

#' Simple wrapper of ggsave for saving plots.
#'
#' @param plots Named list of plots to save.
#' @param widths Width of the plots. Either length one or same as plots.
#' @param heights Height of the plots. Either length one or same as plots.
#' @param units Unit of width and height
#' @param dir Directory under which to save the plot
#' @inheritParams rlang::args_dots_empty
#'
#' @export
#'
#' @examples
#' # TODO
plot_ggsave <- function(
    plots = list(), widths, heights, units = "in", dir = "plots_eps", ...
    ){

  rlang::check_dots_empty()

  # Get names and sizes
  plt_names <- names(plots)
  if (is.null(plt_names)) rlang::abort("Plots must be named.")

  nplt <- length(plots)
  if (length(widths) == 1){
    widths <- rep(widths, nplt)
  }
  if (length(heights) == 1){
    heights <- rep(heights, nplt)
  }
  if (length(widths) != nplt | length(heights) != nplt)
    rlang::abort("Widths/heights must be length one or same as plots.")

  # Saving plots
  for (i in seq_along(plots)){
    # Get current name and sizes
    plt_name <- plt_names[i]
    if (stringr::str_length(plt_name) == 0)
      rlang::abort("All plots must be named.")
    plt_name <- paste0(plt_name, ".eps")
    width <- widths[i]
    height <- heights[i]
    # Get current plot
    plt <- plots[[i]]
    if (!ggplot2::is.ggplot(plt))
      rlang::abort("Plots must be ggplot objects.")
    # Save plot
    save_path <- file.path(dir, plt_name)
    ggplot2::ggsave(
      save_path, plot = plt, device = "eps", width = width, height = height,
      units = units, create.dir = TRUE
      )
  }

  invisible()
}
