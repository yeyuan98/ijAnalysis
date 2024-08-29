# Plotting templates reused in this package

#' Boxplot with data points, viridis_d color filled
#'
#' @param data Data
#' @param x x, must be already quoted
#' @param y y, must be already quoted
#' @param fill fill, must be already quoted
#'
#' @return ggplot object
#'
#' @examples
#' # Internal use
plot.boxPoint <- function(data, x, y, fill){
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
    ggplot2::scale_fill_viridis_d(begin = .6, end = 1)+
    ggplot2::xlab(deparse1(rlang::get_expr(x)))+
    ggplot2::ylab(deparse1(rlang::get_expr(y)))+
    ggplot2::theme_classic(base_size = 24)+
    ggplot2::theme(legend.position = "none")+
    ggplot2::theme(axis.line=ggplot2::element_line(size=0.5))+
    ggplot2::theme(axis.ticks=ggplot2::element_line(size=0.5, colour = "black"))+
    ggplot2::theme(axis.text = ggplot2::element_text(colour = "black"))+
    # The default tick length for base_size = 24 is 6points.
    ggplot2::theme(axis.ticks.length = ggplot2::unit(10, "points"))
}
