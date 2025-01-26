# Simple fitting routines

#' `nls()` fitting of multiple groups
#'
#' @param .data data.frame with variables to fit with additional group variables.
#' @param var.group group variables. Follow `y3628::grouper()`.
#' @param formula Formula to run `nls()`
#' @param ... Additional parameters forwarded to `nls()`. Most notably, you
#' need to provide start values unless you are using self-starting models.
#' See `stats::nls()`.
#'
#' @returns Data frame of fitting parameters, with group variables.
#' @export
#'
#' @examples
#' # Puromycin, self-starting SSmicmen kinetics
#' nlsGroup(Puromycin, state, rate~SSmicmen(conc,Vm,K))
#' # Custom formula. Must provide start values.
#' michaelis_menten <- rate ~ (Vm)*conc/(K+conc)
#' nlsGroup(Puromycin, state, michaelis_menten, start=list(Vm=100,K=0.01))
nlsGroup <- function(.data, var.group, formula, ...){

  # Grouping data
  var.group <- rlang::enexpr(var.group)
  .data <- y3628::grouper(.data, var.group)

  # NLS fit function for one group
  nls_fn <- function(.x, .y, ...){
    # Run nls
    res <- stats::nls(formula = formula, data = .x, ...)
    res <- res$m$getPars() # get parameters
    # Add back the grouping variable
    res <- data.frame(as.list(res))
    res <- cbind(.y, res)
    return(res)
  }

  # Group map NLS fit
  res.list <- dplyr::group_map(.data = .data, .f = nls_fn, ...)
  #   rbind groups
  res.list <- Reduce(rbind, res.list)
  return(res.list)
}
