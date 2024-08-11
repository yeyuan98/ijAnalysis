# Parser helpers for NCT data
#   NCT = data where there are two ROIs per plane
#     one for whole cell, another for the nucleus

#' Predefined subsetting of spotInRoi nuclear/full-cell data.
#'
#' @param dfNucCyt Parsed spotInRoi nuclear/full-cell data.
#' @param what What to subset. must be `nuclear`, `cytoplasm`, or `full`.
#'
#' @return Subsetted spotInRoi nuclear/full-cell data.
#' @export
#'
#' @examples
#' # TODO
spotInRoi_selectNCT <- function(dfNucCyt, what){
  # Select either nuclear, cytoplasm, or total records
  #   Each plane shall have 2 records with different equi.D
  #     the record with larger equi.D will be total,
  #     smaller equi.D will be nuclear

  # Check that each plane has two records and remove bad ones
  check2 <- dfNucCyt |>
    dplyr::group_by(tp, meas, plane) |>
    dplyr::summarize(count = dplyr::n(), .groups = "drop")
  check_bad <- check2$count != 2
  if (any(check_bad)) rlang::warn(sprintf(
    "%d out of %d cells do NOT have two ROIs. These are removed.",
    sum(check_bad), length(check_bad)))
  keep <- check2[!check_bad,]
  #   remove bad ones
  keepTMP <- with(keep, paste(tp, meas, plane))
  dfTMP <- with(dfNucCyt, paste(tp, meas, plane))
  dfNucCyt <- dfNucCyt[dfTMP %in% keepTMP,]

  # Select based on `what`
  #   deparse
  what <- deparse1(substitute(what))
  supported <- c("nucleus", "cytoplasm", "full")
  if (!any(what == supported)) rlang::abort(sprintf(
    "unsupported select. `what` must be one of: %s",
    paste(supported, collapse=",")
  ))
  #   if `what` is nucleus OR full
  if (any(what == c("nucleus", "full"))){
    select <- .selectNCT.index(dfNucCyt, what)
    #   return selection
    return(dfNucCyt[select,])
  } else{
    # TODO: cytoplasmic case
    rlang::abort("not implemented")
  }
}

#' Get index for spotInRoi nuclear/full-cell data subsetting.
#'
#' @param dfNucCyt Parsed spotInRoi nuclear/full-cell data.
#' @param what What to subset. must be `nuclear`, `cytoplasm`, or `full`.
#'
#' @return Indices for subsetting.
#'
#' @examples
#' # Does not apply. Internal use only.
.selectNCT.index <- function(dfNucCyt, what){
  # Return the indices based on what.
  #   internally used by `.selectNCT`

  # add index
  dfNucCyt$idx <- seq(nrow(dfNucCyt))
  idxByD <- function(f){
    dfNucCyt |>
      dplyr::group_by(tp, meas, plane) |>
      dplyr::summarize(idx = idx[f(equi.D)], .groups = "drop") |>
      dplyr::pull(idx) |>
      sort()
  }

  # return indices
  switch(
    what,
    nucleus = idxByD(which.min),
    full = idxByD(which.max),
    rlang::abort("unsupported `what`")
  )
}
