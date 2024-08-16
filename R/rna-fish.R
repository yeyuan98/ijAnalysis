# RNA-FISH result plotting

#' Read dots table from snakemake pipeline output
#'
#' @param path Path to the dots csv
#' @param ... Forwarded to `readr::read_csv`
#'
#' @return tibble of dots table
#' @export
#'
#' @examples
#' #TODO
rfish_read_dots <- function(path, ...){
  readr::read_csv(
    path, col_types = "dddddiccddd",
    col_names = c(
      "x.px", "y.px", "z.px", "int",
      "res", "imgN", "sample", "image",
      "physX", "physY", "physZ"
      ), skip = 1, ...
    )
}

#' Read samples table from snakemake pipeline output
#'
#' @param path Path to the samples excel.
#' @param ... Forwarded to `readxl::read_excel`
#'
#' @return tibble of samples table
#' @export
#'
#' @examples
#' #TODO
rfish_read_samples <- function(path, ...){
  if (!(tools::file_ext(path) %in% c("xls", "xlsx")))
    rlang::abort("Samples must be excel file (xls/xlsx).")
  df <- readxl::read_excel(path, ...)
  names(df)[1:12] <- c(
    "sample", "image", "path.mask", "path.fishdot",
    "physX", "physY", "physZ", "npix.mask", "z.dir",
    "z.count", "include", "num.cells"
  )
  if (length(df) > 12)
    names(df)[13:length(df)] <- paste0("note_", 13:length(df) -12)
  return(df)
}

#' Count number of dots per cell
#'
#' @param samples tibble of samples table, `rfish_read_samples()`
#' @param dots tibble of dots table, `rfish_read_dots()`
#' @param ... <[`tidy-select`][tidyselect::language]> extra columns (from samples table) to retain
#'
#' @return Count table (number of dots per cell).
#' @export
#'
#' @examples
#' # TODO
rfish_count <- function(samples, dots, ...){
  df <- dplyr::left_join(samples, dots, by = c("sample", "image")) |>
    dplyr::group_by(!!rlang::sym("sample"), !!rlang::sym("image")) |>
    dplyr::summarise(num.dots = dplyr::n(), .groups = "drop")
  samples <-
    samples |> dplyr::select(dplyr::matches("num.cells|sample|image"), ...)
  df <- dplyr::left_join(df, samples, by = c("sample", "image"))
  df$dpc <- df$num.dots / df$num.cells
  return(df)
}
