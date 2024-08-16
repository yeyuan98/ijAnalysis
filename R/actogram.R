# Plotting actogram
#   with damr and ggetho packages

#' Read actogram metadata file
#'
#' @param path Path to metadata, must be commonly used delimited or excel.
#' @inheritParams rlang::args_dots_empty
#'
#' @return `behavr::behavr` behavior data table.
#' @export
#'
#' @examples
#' # TODO
actogram_loadFromMetadata <- function(path, ...){
  rlang::check_dots_empty()

  reader <- y3628::flexTableReader(
    col_names = c(
      "file", "start_datetime", "stop_datetime",
      "region_id", "genotype", "replicate"),
    skip = 1
  )
  metadata <- reader(path)
  metadata <- damr::link_dam_metadata(metadata, result_dir = "./")
  dt <- damr::load_dam(metadata)
  summary(dt)
  return(dt)
}


#' Plotting actogram
#'
#' @param behavr `behavr::behavr` behavior data table.
#' @param file Optional path to save the plot. Backend is `ggplot2::ggsave()`.
#' @param file.options List to specify options for saving the plot.
#'
#' @return ggplot plot object.
#' @export
#'
#' @examples
#' # TODO
actogram_plot <-
  function(
    behavr, file = NULL,
    file.options =
      list(device = tools::file_ext(file), height = 9.14, width = 10.6)) {

    hours <- behavr::hours
    behavr <- subset_behavr(behavr, t >= hours(24))

    plt <- ggetho::ggetho(behavr, ggplot2::aes(x=t, z=activity), multiplot = 2)+
      ggetho::stat_bar_tile_etho()+
      ggplot2::scale_x_continuous(
        breaks= c(hours(6), hours(18), hours(30), hours(42)),
        labels=c("0","12","0","12"))+
      ggplot2::xlab("Time (hr)")+
      ggplot2::ylab("Day")+
      ggplot2::theme_classic(base_size = 30)+
      ggplot2::theme(
        axis.line =
          ggplot2::element_blank(),
        panel.border =
          ggplot2::element_rect(colour = "black", fill=NA, linewidth=2))

    if (!is.null(file)){
      file.options$filename <- file
      file.options$plot <- plt
      do.call(ggplot2::ggsave, file.options)
    }

    return(plt)
  }

#' Simple subset of `behavr::behavr` behavior tables
#'
#' @param behavr Behavior table to subset
#' @param expr An expression that evaluates to logical. See details.
#'
#' @return Subsetted behavior table.
#'
#' @details
#' Right now the subsetting creates an intermediate logical vector,
#' which can be very large.
#'
#' @examples
#' # Not exported
subset_behavr <- function(behavr, expr){
  # Grab metadata.
  meta <- data.table::copy(behavr::meta(behavr))
  # Try to evaluate the expression.
  expr <- rlang::enquo(expr)
  tryCatch(
    sel <- rlang::eval_tidy(expr, data = behavr),
    error = function(e) rlang::abort("Failed to evaluate subset logical."))
  # Subset and reset the key column; return new behavr object.
  data <- behavr[sel,]
  data.table::setkey(data, id)
  return(behavr::behavr(data, meta))
}
