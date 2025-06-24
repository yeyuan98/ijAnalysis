# Plotting templates reused in this package
#   Templates here should define the following:
#     1. theme
#     2. layers
#     3. data and aesthetics
#   Templates here must NOT define the following:
#     coordinate system `scale_`

#' Standardized themes for plotting
#'
#' Themes used by different plots in this package for standardized behavior.
#'
#' Implemented themes:
#'
#' * `classic`: Based on `ggplot2::theme_classic()`.
#'
#' @param type standardized theme name, see details.
#'
#' @returns GG theme object.
#'
#' @examples
#' # Internal use
plot.themes <- function(type = "classic"){

  classic <-
    ggplot2::theme_classic(base_size = 24)+
    ggplot2::theme(legend.position = "none")+
    ggplot2::theme(axis.line=ggplot2::element_line(linewidth=0.5))+
    ggplot2::theme(axis.ticks=ggplot2::element_line(linewidth=0.5, colour = "black"))+
    ggplot2::theme(axis.text = ggplot2::element_text(colour = "black"))+
    # The default tick length for base_size = 24 is 6points.
    ggplot2::theme(axis.ticks.length = ggplot2::unit(10, "points"))

  switch(
    type,
    classic = classic,
    stop("Unsupported plot theme."))
}

#' Boxplot with data points
#'
#' @param data Data
#' @param x x, must be already quoted
#' @param y y, must be already quoted
#' @param fill fill, must be already quoted
#' @param theme what theme to apply, see `plot.themes()`.
#'
#' @return ggplot object
#'
#' @examples
#' # Internal use
plot.boxPoint <- function(data, x, y, fill, theme = "classic"){
  ggplot2::ggplot(
    data = data, mapping = ggplot2::aes(x = !!x, y = !!y, fill = !!fill)
  )+
    ggplot2::geom_boxplot(
      outlier.shape = NA, width=0.5, linewidth = 0.5,
      position = ggplot2::position_dodge(width=0.8))+
    ggplot2::geom_point(
      position=ggplot2::position_jitterdodge(
        dodge.width = 0.8,jitter.width = 0.2),
      alpha=1, size = 1, shape = 19)+
    ggplot2::xlab(deparse1(rlang::get_expr(x)))+
    ggplot2::ylab(deparse1(rlang::get_expr(y)))+
    plot.themes(theme)
}

#' Point plot with lines connecting 'mean' values.
#'
#' If `color` not specified or equal to `x`, whole data will be one color.
#'
#' @param data Data
#' @param x x, must be already quoted
#' @param y y, must be already quoted
#' @param color color, must be already quoted
#' @param theme what theme to apply, see `plot.themes()`
#' @param mean_func function to compute 'mean' (sample representative value)
#' @param sd_func function to compute 'sd' (sample spread)
#'
#' @returns ggplot object
#'
#' @examples
#' # Internal use
plot.linePoint <- function(
    data, x, y, color = NA, theme = "classic",
    mean_func = mean, sd_func = \(x) stats::sd(x)/sqrt(length(x))){

  # Process edge cases of color:
  #   1. color and x are quoted and refer to the same symbol
  #   2. color is not quoted and is NA
  if (

    (rlang::is_symbolic(color) &&
     (rlang::as_label(x) == rlang::as_label(color))) ||
    (!rlang::is_symbolic(color) && is.na(color))

    ){
    color <- rlang::expr(".color")
    data[[".color"]] <- "default"
  } else if (!is.language(color))
    if (is.na(color)){
      color <- rlang::expr(".color")
      data[[".color"]] <- "default"
    }

  # Warn user if color is continuous
  if (is.numeric(rlang::eval_tidy(color, data = data)))
    rlang::warn(paste0(
      "Your color variable is continuous.",
      " Continuous color scale will be required."
      ))

  # Compute summary
  data.summary <- data |> dplyr::group_by(!!x, !!color) |>
    dplyr::summarize(
      mean = mean_func(!!y), sd = sd_func(!!y), .groups = "drop")

  # Create plot
  ggplot2::ggplot(
    data = data, mapping = ggplot2::aes(x = !!x, y = !!y, color = !!color)
  )+
    ggplot2::geom_jitter(size = 1, shape = 19, width = 0.2)+
    ggplot2::geom_line(
      data = data.summary,
      ggplot2::aes(x = !!x, y = mean,
          color = !!color, group = !!color), linewidth = 0.5) +
    ggplot2::geom_errorbar(
      data = data.summary,
      ggplot2::aes(x = !!x, y = mean, ymin = mean-sd, ymax = mean+sd,
          color = !!color, group = !!color), width = 0.2, linewidth = 0.5) +
    ggplot2::xlab(deparse1(rlang::get_expr(x)))+
    ggplot2::ylab(deparse1(rlang::get_expr(y)))+
    plot.themes(theme)
}
