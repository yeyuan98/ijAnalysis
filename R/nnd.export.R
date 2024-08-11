# Export NND analysis results
#   Uses RoiPointsSaver to support batch export of ROIs for ImageJ viz

#' Order permutation for a NND result record
#'
#' @param nnd.results `y3628::metaRcrd` of NND result.
#' @param ... Passed to `base::order`
#'
#' @return Order permutation using the `nnd` field.
#'
#' @examples
#' # Does not apply. Internal use only.
nnd.order <- function(nnd.results, ...){
  # Order permutation for a NND result record
  # Backend uses base::order
  order(vctrs::field(nnd.results, "nnd"), ...)
}

#' Sort spotInRoi data based on NND results
#'
#' @param df.nnd spotInRoi data with NND results.
#' @param nnd.column Character name of the column that stores NND results.
#' @param ... Passed to `base::order` for order permutation.
#'
#' @return Sorted spotInRoi results.
#' @export
#'
#' @examples
#' # TODO
spotInRoi_nnd.sort <- function(df.nnd, nnd.column = "result", ...){
  # Sort each of the NND result record and accordingly spotIn.x
  # TODO: make dynamic selection of other columns to sort.

  # Get order permutations
  order.perms <- lapply(
    df.nnd[[nnd.column]], \(x) nnd.order(x, ...)
  )

  # Sort spotIn.x and result columns

  #   Helper function, sort one column
  df.sort <- function(df.nnd, which){
    df.nnd[[which]] <- lapply(
      seq(nrow(df.nnd)),
      \(idx) df.nnd[[which]][[idx]][order.perms[[idx]]]
    )
    return(df.nnd)
  }
  #   Sort by the precomputed permutations
  df.nnd <- df.sort(df.nnd, "spotIn.x")
  df.nnd <- df.sort(df.nnd, nnd.column)

  return(df.nnd)
}

#' Expand NND results as data frame entries
#'
#' @param df.nnd Data frame with a NND result column
#' @param nnd.column Character name of the column that stores NND results.
#'
#' @return Expanded data frame with each NND value as one row
#' @export
#'
#' @examples
#' # TODO
spotInRoi_as.data.frame.df.nnd <- function(df.nnd, nnd.column = "result"){
  # Expand NND data to data frame for export
  #   columns to export:
  #     tp, meas, plane, spotIn.x, result
  # Warning: it is assumed that orders of spotIn.x and result are consistent

  expandRow <- function(tp, meas, plane, spotIn.x, result){
    # Expand one NND result row into a data frame
    #   Expand spotIn.x
    spotIn.x <- vctrs::vec_proxy(spotIn.x)
    spotIn.x <- dplyr::select(spotIn.x, -idx, -dist)
    #   Expand result
    result <- vctrs::vec_proxy(result)
    result <- dplyr::select(result, t.x, t.y, nnd)
    #   column bind and return
    cbind(tp = tp, meas = meas, plane = plane, spotIn.x, result)
  }

  df.nnd |>
    dplyr::select(-spotIn.y) |>
    dplyr::rename(result = nnd.column) |>
    purrr::pmap(expandRow) |>
    purrr::list_rbind()
}
